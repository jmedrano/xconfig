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
         register_listener/3,
         unregister_listener/1,
         set/2,
         set_full_config/1,
         set_full_config/2,
         delete_key/1,
         delete_all/0,
         monitor/0,

         config_element_to_atom/1
        ]).


-define(PROCESS_NAME, ?MODULE).
-define(ST, ?MODULE).

-type listener_mfa() :: tuenti_config:listener_mfa().
-type ets_listener_key() :: {tuenti_config:config_key(), [tuenti_config:breed()]}.
-type raw_listener_key() :: {tuenti_config:raw_config_key(), [tuenti_config:raw_breed()]}.
-record(listener_data, {
          effective_key = [] :: [tuenti_config:config_key() | tuenti_config:raw_config_key()],
          effective_breeds = [] :: [tuenti_config:breed() | tuenti_config:raw_breed()],
          value = not_found :: tuenti_config_ets:result(tuenti_config:value()),
          targets :: #{listener_mfa() := reference()},
          binary_in_key = true :: boolean(), % config key has binary() that couldn't be transformed into atom
          binary_in_breeds = true :: boolean() % config breeds has binary() that couldn't be transformed into atom
         }).

-record(?ST, {
        listeners = #{} :: #{
          ets_listener_key() | raw_listener_key() := #listener_data{}
         },
        % This is an index by Ref to be able to unregister listeners
        listener_refs = #{} :: #{reference() := {ets_listener_key() | raw_listener_key(), listener_mfa()}}
    }).


%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------
%% @doc Add a new listener for a config key. Each key can have multiple listeners
%% and a listener can listen for changes of multiple keys.
%%
%% Registering the same {Key, MFA} pair several times has no effect, for this
%% comprobation to take effect, the Keys must be exactly the same (including
%% types)
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
%% Note that the Key parameter will be a full global one and without binaries
-spec register_listener({false, tuenti_config:config_key()} | {true, tuenti_config:raw_config_key()}, {false, [tuenti_config:breed()]} | {true, [tuenti_config:raw_breed()]}, listener_mfa())
        -> {ok, ListenerRef :: reference(), CurrentValue :: tuenti_config:optional_result(), SrvPid :: pid()}.
register_listener({CheckKey, Key}, {CheckBreeds, Breeds}, {Module, Function, _ExtraArgs} = ListenerTarget)
  when is_list(Key), is_atom(Module), is_atom(Function) ->
    gen_server:call(?PROCESS_NAME, {register, {Key, Breeds}, CheckKey, CheckBreeds, ListenerTarget}).


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
handle_call({register, {Key, Breeds} = ListenerKey, CheckKey, CheckBreeds, ListenerMFA}, _From, #?ST{listeners = Listeners, listener_refs = ListenerRefs} = State) ->
    {KeyExpectedAtoms, EffectiveKey} = if CheckKey -> key_expect_atoms(Key);
                                          true -> {false, Key}
                                       end,
    {BreedsExpectedAtoms, EffectiveBreeds} = if CheckBreeds -> breeds_expect_atoms(Breeds);
                                                true -> {false, Breeds}
                                             end,

    {Ref, CurrentValue, NewListenerData} = case Listeners of
                                               #{ListenerKey := ListenerData} ->
                                                   add_target(ListenerMFA, ListenerData);
                                               _ ->
                                                   new_target(ListenerMFA, EffectiveKey, EffectiveBreeds, KeyExpectedAtoms, BreedsExpectedAtoms)
                                           end,
    NewState = State#?ST{
                        listeners = Listeners#{ListenerKey => NewListenerData},
                        listener_refs = ListenerRefs#{Ref => {ListenerKey, ListenerMFA}}
                       },
    {reply, {ok, Ref, CurrentValue, self()}, NewState};

handle_call({unregister, Ref}, _From, #?ST{listeners = Listeners, listener_refs = ListenerRefs} = State) when is_reference(Ref) ->
    case maps:take(Ref, ListenerRefs) of
        {{ListenerKey, ListenerMFA}, NewListenerRefs} ->
            #{ListenerKey := #listener_data{targets = Targets} = ListenerData} = Listeners,
            {Ref, CleanTargets} = maps:take(ListenerMFA, Targets),

            case map_size(CleanTargets) of
                0 ->
                    % No more listeners remaining for this key
                    NewState = State#?ST{
                                        listeners = maps:remove(ListenerKey, Listeners),
                                        listener_refs = NewListenerRefs
                                       },
                    {reply, ok, NewState};
                _ ->
                    NewListenerData = ListenerData#listener_data{targets = CleanTargets},
                    NewState = State#?ST{
                                        listeners = Listeners#{ListenerKey => NewListenerData},
                                        listener_refs = NewListenerRefs
                                       },
                    {reply, ok, NewState}
            end;
        _ ->
            {reply, ok, State}
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
    State#?ST{listeners = maps:fold(fun(Key, ListenerData, Acc) ->
                                            Acc#{Key => trigger_listener(check_listener_breeds(check_listener_key(ListenerData)))}
                                    end,
                                    #{},
                                    Listeners)}.


check_listener_key(#listener_data{binary_in_key = false} = ListenerData) ->
    ListenerData;
check_listener_key(#listener_data{effective_key = Key} = ListenerData) ->
    {ExpectedAtoms, EffectiveKey} = key_expect_atoms(Key),
    ListenerData#listener_data{effective_key = EffectiveKey, binary_in_key = ExpectedAtoms}.


check_listener_breeds(#listener_data{binary_in_breeds = false} = ListenerData) ->
    ListenerData;
check_listener_breeds(#listener_data{effective_breeds = Breeds} = ListenerData) ->
    {ExpectedAtoms, EffectiveBreeds} = breeds_expect_atoms(Breeds),
    ListenerData#listener_data{effective_breeds = EffectiveBreeds, binary_in_breeds = ExpectedAtoms}.


trigger_listener(ListenerData) ->
    case listener_get_value(ListenerData) of
        {V, V, NewListenerData} ->
            NewListenerData;
        {OldValue, NewValue, NewListenerData} ->
            Change = calculate_change(ListenerData#listener_data.effective_key, OldValue, NewValue),
            maps:fold(
              fun({Module, Function, ExtraArgs}, _Refs, _) ->
                      catch Module:Function(Change, ExtraArgs),
                      undefined
              end,
              undefined,
              ListenerData#listener_data.targets
             ),
            NewListenerData
    end.


listener_get_value(#listener_data{binary_in_key = true, value = OldValue} = ListenerData) ->
    {OldValue, not_found, ListenerData#listener_data{value = not_found}};
listener_get_value(ListenerData) ->
    #listener_data{
       effective_key = Key,
       effective_breeds = DirtyBreeds,
       value = OldValue
      } = ListenerData,
    Breeds = [{BreedKey, BreedValue} || {BreedKey, BreedValue} <- DirtyBreeds, is_atom(BreedKey), is_atom(BreedValue)],
    NewValue = tuenti_config_ets:get_raw_with_breeds(Key, Breeds),
    {OldValue, NewValue, ListenerData#listener_data{value = NewValue}}.


-spec calculate_change(Key :: tuenti_config:config_key(),
                       OldResult :: tuenti_config_ets:result(tuenti_config:value()),
                       NewResult :: tuenti_config_ets:result(tuenti_config:value())) -> tuenti_config:config_change().
calculate_change(Key, not_found, {found, Value}) ->
    {added, Key, undefined, Value};
calculate_change(Key, {found, Value}, not_found) ->
    {deleted, Key, Value, undefined};
calculate_change(Key, {found, OldValue}, {found, NewValue}) ->
    {changed, Key, OldValue, NewValue}.


add_target(ListenerMFA, #listener_data{value = CurrentValue, targets = Targets} = ListenerData) ->
    case Targets of
        #{ListenerMFA := Ref} ->
            {Ref, CurrentValue, ListenerData};
        _ ->
            Ref = make_ref(),
            {Ref, CurrentValue, ListenerData#listener_data{targets = Targets#{ListenerMFA => Ref}}}
    end.


new_target(ListenerMFA, Key, Breeds, KeyExpectsAtoms, BreedsExpectsAtoms) ->
    Ref = make_ref(),
    {_, CurrentValue, ListenerData} = listener_get_value(#listener_data{
                                                            effective_key = Key,
                                                            effective_breeds = Breeds,
                                                            targets = #{ListenerMFA => Ref},
                                                            binary_in_key = KeyExpectsAtoms,
                                                            binary_in_breeds = BreedsExpectsAtoms
                                                           }),
    {Ref, CurrentValue, ListenerData}.


key_expect_atoms(Key) ->
    key_expect_atoms(Key, false, []).

key_expect_atoms([], BinaryPresent, RevAcc) ->
    {BinaryPresent, lists:reverse(RevAcc)};
key_expect_atoms([Elem | RestKey], BinaryPresent, RevAcc) ->
    {IsAtom, NewElem} = config_element_to_atom(Elem),
    key_expect_atoms(RestKey, BinaryPresent or not IsAtom, [NewElem | RevAcc]).


breeds_expect_atoms(Breeds) ->
    breeds_expect_atoms(Breeds, false, []).

breeds_expect_atoms([], BinaryPresent, RevAcc) ->
    {BinaryPresent, lists:reverse(RevAcc)};
breeds_expect_atoms([{BreedKey, BreedVal} | Restbreeds], BinaryPresent, RevAcc) ->
    {BreedKeyIsAtom, NewBreedKey} = config_element_to_atom(BreedKey),
    {BreedValIsAtom, NewBreedVal} = config_element_to_atom(BreedVal),
    NewBreed = {NewBreedKey, NewBreedVal},
    breeds_expect_atoms(Restbreeds, BinaryPresent or (not BreedKeyIsAtom) or (not BreedValIsAtom), [NewBreed | RevAcc]).


config_element_to_atom(ElemAtom) when is_atom(ElemAtom) ->
    {true, ElemAtom};
config_element_to_atom(ElemBin) when is_binary(ElemBin) ->
    try {true, binary_to_existing_atom(ElemBin, utf8)}
    catch error:badarg -> {false, ElemBin}
    end;
config_element_to_atom(ElemIoList) when is_list(ElemIoList) ->
    config_element_to_atom(iolist_to_binary(ElemIoList)).

