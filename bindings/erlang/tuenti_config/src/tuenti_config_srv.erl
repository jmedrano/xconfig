-module(tuenti_config_srv).
-behaviour(gen_server).

%%% Start/Stop
-export([start_link/0]).

%%% Behaviour's callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

%%% API
-export([
         register_listener/2,
         register_listener/3,
         unregister_listener/1,
         set/2,
         set_full_config/1,
         set_full_config/2,
         delete_key/1,
         delete_all/0,
         monitor/0
        ]).


-define(PROCESS_NAME, ?MODULE).
-define(ST, ?MODULE).

-type listener_mfa() :: tuenti_config:listener_mfa().
-type listener_target() :: {reference(), listener_mfa()}.
-type listeners() :: #{{tuenti_config:config_key(), [tuenti_config:breed(atom())]} := #{
                                                        value := tuenti_config_ets:result(tuenti_config:value()),
                                                        targets := nonempty_list(listener_target())
                                                       }}.

-record(?ST, {
        listeners = #{} :: listeners(),
        %% This is an index by Ref to be able to unregister listeners
        listener_refs = #{} :: #{reference() := tuenti_config:config_key()}
    }).


%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------
%% @doc Add a new listener for a config key. Each key can have multiple listeners
%% and a listener can listen for changes of multiple keys
%%
%% It returns the reference() to be able to unregister this listener and the
%% current value of the config.
%%
%% Note that the current value is returned in tuenti_config_ets:result(tuenti_config:value())
%% format because the key may not exist at the registration time.
%%
%% The listener will be called when a config change in the specifed key is detected as this:
%% Module:Function(Change :: tuenti_config:config_change(), ExtraArgs :: term()).
%%
%% Note that the Key parameter will be a full global one
-spec register_listener(tuenti_config:config_key(), listener_mfa())
        -> {ok, ListenerRef :: reference(), CurrentValue :: tuenti_config:optional_result(), SrvPid :: pid()}.
register_listener(Key, ListenerTarget) ->
    register_listener(Key, [], ListenerTarget).

-spec register_listener(tuenti_config:config_key(), [tuenti_config:breed(atom())], listener_mfa())
        -> {ok, ListenerRef :: reference(), CurrentValue :: tuenti_config:optional_result(), SrvPid :: pid()}.
register_listener(Key, Breeds, {Module, Function, _ExtraArgs} = ListenerTarget)
  when is_list(Key), is_atom(Module), is_atom(Function) ->
    gen_server:call(?PROCESS_NAME, {register, {Key, Breeds}, ListenerTarget}).


%% @doc Unregister a listener using the reference() obtained when calling register_listener
-spec unregister_listener(Ref :: reference()) -> ok.
unregister_listener(Ref) ->
    gen_server:call(?PROCESS_NAME, {unregister, Ref}).


%% @doc Set a single key in the config
%% This function updates the internal config version
%% Config change listeners are called before returning from this function
-spec set(tuenti_config:config_key(), tuenti_config:value()) -> ok.
set(Key, Value) ->
    gen_server:call(?PROCESS_NAME, {set, Key, Value}).


%% @doc Set the full configuration
%% Config change listeners are called before returning from this function
-spec set_full_config(tuenti_config_ets:full_config()) -> ok.
set_full_config(FullConfig) ->
    set_full_config(FullConfig, erlang:unique_integer()).


%% @doc Set the full configuration with version
%% Config change listeners are called before returning from this function
-spec set_full_config(tuenti_config_ets:full_config(), integer()) -> ok.
set_full_config(FullConfig, Version) ->
    gen_server:call(?PROCESS_NAME, {set_full_config, FullConfig, Version}).


%% @doc Delete a key from the config
%% Config change listeners are called before returning from this function
-spec delete_key(tuenti_config:config_key()) -> ok.
delete_key(Key) ->
    gen_server:call(?PROCESS_NAME, {delete_key, Key}).


%% @doc Delete all the config
%% Config change listeners are called before returning from this function
-spec delete_all() -> ok.
delete_all() ->
    gen_server:call(?PROCESS_NAME, delete_all).


%% @doc Creates a monitor of the server process
-spec monitor() -> reference().
monitor() ->
    monitor(process, ?PROCESS_NAME).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?PROCESS_NAME}, ?MODULE, [], []).


-spec init([term()]) -> {ok, #?ST{}}.
init([]) ->
    % if this gen_server dies and respawns
    % we don't wanna crash recreating tables that already exist
    ok = tuenti_config_ets:init(),
    {ok, #?ST{}}.


%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR HANDLES
%%%-----------------------------------------------------------------------------
handle_call({register, KeyBreeds, ListenerMFA}, _From, #?ST{listeners = Listeners, listener_refs = ListenerRefs} = State) ->
    Ref = make_ref(),
    {CurrentValue, NewListenerData} = case Listeners of
                                          #{KeyBreeds := ListenerData} ->
                                              add_target({Ref, ListenerMFA}, ListenerData);
                                          _ ->
                                              new_target({Ref, ListenerMFA}, KeyBreeds)
                                       end,
    NewState = State#?ST{
                listeners = Listeners#{KeyBreeds => NewListenerData},
                listener_refs = ListenerRefs#{Ref => KeyBreeds}
            },
    {reply, {ok, Ref, CurrentValue, self()}, NewState};

handle_call({unregister, Ref}, _From, #?ST{listeners = Listeners, listener_refs = ListenerRefs} = State) when is_reference(Ref) ->
    case maps:take(Ref, ListenerRefs) of
        {Key, NewListenerRefs} ->
            #{Key := #{targets := Targets} = ListenerData} = Listeners,

            case lists:keytake(Ref, 1, Targets) of
                {value, _, []} ->
                    % No more listeners remaining for this key
                    {reply, ok, State#?ST{listeners = maps:remove(Key, Listeners), listener_refs = NewListenerRefs}};
                {value, _, NewTargets} ->
                    NewListenerData = ListenerData#{targets => NewTargets},
                    {reply, ok, State#?ST{listeners = Listeners#{Key => NewListenerData}, listener_refs = NewListenerRefs}}
            end
    end;


handle_call({set, Key, Value}, _From, State) ->
    tuenti_config_ets:set_config(Key, Value),
    {reply, ok, notify_changes(State)};


handle_call({set_full_config, FullConfig, Version}, _From, State) ->
    tuenti_config_ets:set_full_config(FullConfig, Version),
    {reply, ok, notify_changes(State)};


handle_call({delete_key, Key}, _From, State) ->
    tuenti_config_ets:delete_key(Key),
    {reply, ok, notify_changes(State)};


handle_call(delete_all, _From, State) ->
    tuenti_config_ets:delete_all(),
    {reply, ok, notify_changes(State)}.


handle_cast(_, State) ->
    {noreply, State}.


%% Optional callback, but defined to avoid warnings when receiving
%% {'ETS-TRANSFER', Tab, FromPid, GiftData} messages
handle_info(_, State) ->
    {noreply, State}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL
%%%-----------------------------------------------------------------------------

-spec notify_changes(#?ST{}) -> #?ST{}.
notify_changes(#?ST{listeners = Listeners} = State) ->
    NewListeners =
        maps:fold(
            fun({Key, Breeds} = KeyBreeds, #{value := OldValue, targets := Targets} = ListenerData, Acc) ->
                case tuenti_config_ets:get_raw_with_breeds(Key, Breeds) of
                    OldValue ->
                        Acc#{KeyBreeds => ListenerData};
                    NewValue ->
                        Change = calculate_change(Key, OldValue, NewValue),
                        lists:foreach(
                            fun({_Ref, {Module, Function, ExtraArgs}}) ->
                                catch Module:Function(Change, ExtraArgs)
                            end,
                            Targets
                        ),
                        Acc#{KeyBreeds => ListenerData#{value => NewValue}}
                end
            end,
            #{},
            Listeners),
    State#?ST{listeners = NewListeners}.


-spec calculate_change(Key :: tuenti_config:config_key(),
                       OldResult :: tuenti_config_ets:result(tuenti_config:value()),
                       NewResult :: tuenti_config_ets:result(tuenti_config:value())) -> tuenti_config:config_change().
calculate_change(Key, not_found, {found, Value}) ->
    {added, Key, undefined, Value};
calculate_change(Key, {found, Value}, not_found) ->
    {deleted, Key, Value, undefined};
calculate_change(Key, {found, OldValue}, {found, NewValue}) ->
    {changed, Key, OldValue, NewValue}.


add_target(NewTarget, #{value := CurrentValue, targets := Targets} = ListenerData) ->
    NewListenerData = ListenerData#{targets => [NewTarget | Targets]},
    {CurrentValue, NewListenerData}.


new_target(NewTarget, {Key, Breeds}) ->
    CurrentValue = tuenti_config_ets:get_raw_with_breeds(Key, Breeds),
    {CurrentValue, #{value => CurrentValue, targets => [NewTarget]}}.

