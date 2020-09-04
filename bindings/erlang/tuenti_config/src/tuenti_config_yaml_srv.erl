-module(tuenti_config_yaml_srv).
-behaviour(gen_server).

-include_lib("yamerl/include/yamerl_nodes.hrl").

%%% Start/Stop
-export([start_link/0]).

%%% Behaviour's callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%%% API
-export([reload/0]).


-define(PROCESS_NAME, ?MODULE).
-define(ST, ?MODULE).

-record(?ST, {}).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------
-spec reload() -> reloaded | nothing_new.
reload() ->
  gen_server:call(?PROCESS_NAME, reload).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?PROCESS_NAME}, ?MODULE, [], []).


-spec init([term()]) -> {ok, #?ST{}}.
init([]) ->
  case tuenti_config:xconfig_enabled() of
    true ->
      ignored;
    _ ->
      %% note that it can fail
      %% (but we don't care / check the return value)
      % We make sure that the yamerl application is running
      {ok, _} = application:ensure_all_started(yamerl),
      load_from_file()
  end,
  {ok, #?ST{}}.


-spec terminate(term(), #?ST{}) -> _.
terminate(_Reason, _State) ->
  ok.


%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR HANDLES
%%%-----------------------------------------------------------------------------
handle_call(reload, _From, State) ->
  {reply, reload_from_file(), State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.


%%%-----------------------------------------------------------------------------
%%% CODE UPDATE EXPORTS
%%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------
-spec load_from_file() -> ok.
load_from_file() ->
  NewConfig = read_files(),
  _ = tuenti_config_srv:set_full_config(NewConfig),
  ok.


-spec reload_from_file() -> ok.
reload_from_file() ->
  NewConfig = read_files(),
  tuenti_config_srv:set_full_config(NewConfig).


-spec read_files() -> map().
read_files() ->
  % Search for all .yaml files in the XConfig directories
  Paths = string:tokens(tuenti_config:xconfig_config_path(), ":"),

  Filenames = lists:foldl(fun(Path, Acc) ->
                              Filenames = list_yaml_files(Path),
                              Acc ++ Filenames
                          end,
                          [],
                          Paths),

  % Now load the configuration for each of the files
  lists:foldl(fun(Filename, ConfigAcc) ->
                  % Load the config and converts it to our internal format to be loaded in the ETS table
                  maps_deep_merge(ConfigAcc, load_from_file(Filename))
              end,
              #{},
              Filenames).


%% @doc Lists all the .yaml files in a directory
-spec list_yaml_files(string()) -> list(string()).
list_yaml_files(Path) ->
  filelib:wildcard(filename:join(Path, "*.yaml")).


%% @doc Loads the configuration from a file and returns it in the same format that the "exconfig" C driver does
%% See the tuenti_config_xconfig_srv for documentation about the format
-spec load_from_file(string()) -> map().
load_from_file(Filename) ->
  [#yamerl_doc{root = Root} | _] = yamerl_constr:file(Filename, [{detailed_constr, true}, {node_mods, [tuenti_config_yamerl_node_helper]}]),
  % Make sure the root node is a map
  #yamerl_map{} = Root,
  parse_config(Root).


%% @doc Converts from the YAMERL detailed format to the "exconfig" C driver format
-spec parse_config(yamerl_node()) -> map() | list() | binary() | number() | boolean().
parse_config(#yamerl_map{pairs = Pairs}) ->
  lists:foldl(fun
                  ({#yamerl_str{text = KeyText}, Value}, M) ->
                      % In case of UTF-8 encoded keys, we store them as incorrect "latin1" atoms
                      % because atoms doesn't support utf-8 encoding
                      maps:put(binary_to_atom(unicode:characters_to_binary(KeyText, utf8), latin1), parse_config(Value), M);
                  ({#yamerl_int{value = KeyValue}, Value}, M) ->
                      maps:put(list_to_atom(integer_to_list(KeyValue)), parse_config(Value), M);
                  ({#yamerl_float{value = KeyValue}, Value}, M) ->
                      maps:put(list_to_atom(float_to_list(KeyValue)), parse_config(Value), M);
                  ({#yamerl_bool{value = KeyValue}, Value}, M) ->
                      maps:put(KeyValue, parse_config(Value), M)
              end,
              #{},
              Pairs);
parse_config(#yamerl_seq{entries = Entries}) ->
  lists:map(fun parse_config/1, Entries);
parse_config(#yamerl_str{text = Text}) ->
  unicode:characters_to_binary(Text, utf8);
parse_config(#yamerl_int{value = Value}) ->
  Value;
parse_config(#yamerl_float{value = Value}) ->
  Value;
parse_config(#yamerl_bool{value = Value}) ->
  Value;
parse_config(#yamerl_null{}) ->
  null.


-spec maps_deep_merge(M1 :: map(), M2 :: map()) -> map().
maps_deep_merge(M1, M2) when is_map(M1), is_map(M2) ->
    maps:fold(
        fun(K, V2, Acc) ->
            case Acc of
                #{K := V1} ->
                    Acc#{K => maps_deep_merge(V1, V2)};
                _ ->
                    Acc#{K => V2}
            end
        end,
        M1,
        M2);
maps_deep_merge(_, Override) ->
    Override.


