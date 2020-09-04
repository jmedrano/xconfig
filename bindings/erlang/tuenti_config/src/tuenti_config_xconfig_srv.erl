-module(tuenti_config_xconfig_srv).
-behaviour(gen_server).

%%% Start/Stop
-export([start_link/0]).

%%% Behaviour's callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(PROCESS_NAME, ?MODULE).
-define(ST, ?MODULE).
-define(RECEIVE_TIMEOUT, 4000). % waiting for driver to reply
-define(CLOSE_SLEEP_TIME, 5000). % Waiting time for XConfig restart
-define(LOGGER_CONFIG, <<"Config">>).

-record(?ST, {
        port :: port(),
        config_version = 0 :: non_neg_integer()
    }).


%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?PROCESS_NAME}, ?MODULE, [], []).


-spec init([term()]) -> {ok, #?ST{}}.
init([]) ->
    Port = create_port(),
    BinData = receive_data(Port),
    Data = parse_data(BinData),
    case handle_data(Data, 0) of
        {error, Reason} ->
            {stop, Reason};
        {ok, _} ->
            {ok, #?ST{port = Port, config_version = 1}}
    end.


-spec terminate(term(), #?ST{}) -> _.
terminate(_Reason, #?ST{port = Port}) ->
    {_, close} = close_port(Port),
    ok.


%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR HANDLES
%%%-----------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({Port, {data, BinData}}, State = #?ST{port = Port, config_version = ConfigVersion}) ->
    case handle_data(parse_data(BinData), ConfigVersion) of
        {error, Reason} ->
            {stop, Reason, State};
        {ok, _} ->
            {noreply, State#?ST{config_version = ConfigVersion + 1}};
        close ->
            % XConfig connection closed, we should wait some seconds to allow XConfig to restart
            % While tuenti_config_srv is running we still have readable configuration in memory
            timer:sleep(?CLOSE_SLEEP_TIME),
            {stop, xconfig_closed, State}
    end;

handle_info({Port, {exit_status, Status}}, State = #?ST{port = Port}) ->
    Reason = {process_died, Status},
    {stop, Reason, State};

handle_info({'EXIT', Port, PosixCode}, State = #?ST{port = Port}) ->
    Reason = {port_closed, PosixCode},
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.


%%%-----------------------------------------------------------------------------
%%% CODE UPDATE EXPORTS
%%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%-----------------------------------------------------------------------------
%%% INTERNAL HANDLING
%%%-----------------------------------------------------------------------------
-spec handle_data(term(), ConfigVersion :: non_neg_integer()) -> {ok | error, term()} | close.
handle_data({error, Reason}, _) ->
    {error, Reason};
handle_data({ok, FullNewConfig}, ConfigVersion) ->
    Changes = tuenti_config_srv:set_full_config(FullNewConfig, ConfigVersion),
    {ok, Changes};
handle_data(close, _) ->
    close.


-spec receive_data(port()) -> ok | {error, term()}.
receive_data(Port) ->
    receive
        {Port, {data, BinData}}  ->
            BinData
    after
        ?RECEIVE_TIMEOUT ->
            {error, timeout}
    end.



-spec parse_data({error, atom()} | binary()) -> {error, atom()} | close | {ok, tuenti_config_ets:full_config()}.
parse_data({error, Reason}) ->
    {error, Reason};
parse_data(Bin) when is_binary(Bin) ->
  case binary_to_term(Bin) of
    {error, Reason} ->
        {error, Reason};
    {config, {error, Reason}} ->
        {error, Reason};
    {config, FullConfig} ->
        {ok, FullConfig};
    close ->
        close
    end.


%%%-----------------------------------------------------------------------------
%%% INTERNAL PORT CONTROL
%%%-----------------------------------------------------------------------------
-spec create_port() -> port().
create_port() ->
    case code:priv_dir(tuenti_config) of
        {error, _Reason} ->
            exit(error);
        PrivDir ->
            open_port({spawn, exconfig_cmd(PrivDir)}, [binary, {packet, 4}, exit_status])
    end.


-spec close_port(port()) -> _.
close_port(Port) ->
    Port ! {self(), close}.

%%%-----------------------------------------------------------------------------
%%% Utility functions
%%%-----------------------------------------------------------------------------
-spec exconfig_path(string()) -> string().
exconfig_path(PrivDir) ->
  filename:join([PrivDir, "bin", "exconfig"]).

-spec quote_path(string()) -> string().
quote_path(Path) ->
  % Escape the inner quotes and add outer quotes
  "\"" ++ re:replace(Path, "\"", "\\\"", [global]) ++ "\"".

-spec exconfig_cmd(string()) -> string().
exconfig_cmd(PrivDir) ->
  ConfigPath = tuenti_config:xconfig_config_path(),
  report_config_path(ConfigPath),
  PartialCommand = exconfig_path(PrivDir) ++ " "
                   ++ quote_path(ConfigPath) ++ " "
                   ++ quote_path(tuenti_config:xconfig_socket_path()),

  case application:get_env(tuenti_config, exconfig_quiet) of
    {ok, true} ->
        PartialCommand ++ " 2> /dev/null";
    _ ->
        PartialCommand
  end.

report_config_path(ConfigPath) ->
  case catch {ok, logger:module_info()} of
    {ok, _} ->
      logger:notice("Config listener started with paths: ~p", [ConfigPath]);
    _ ->
      error_logger:info_msg("Config listener started with paths: ~p", [ConfigPath])
  end.

