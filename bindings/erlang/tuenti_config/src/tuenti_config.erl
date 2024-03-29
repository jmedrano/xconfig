%%% @doc Application tuernti_config interface module.

-module(tuenti_config).

%%% ===========================================================================
%%% Exports
%%% ===========================================================================

%%% Start/Stop
-export([start/0,
         stop/0]).

%%% API
-export([
         expand_key/1,
         transient_raw_config_key/1,
         transient_raw_breeds/1,

         get_raw/1,
         get_raw/2,
         get_integer/1,
         get_integer/2,
         get_float/1,
         get_float/2,
         get_string/1,
         get_string/2,
         get_binary/1,
         get_binary/2,
         get_bool/1,
         get_bool/2,
         get_mfa/1,
         get_map/1,
         get_map/2,

         global_get_raw/1,
         global_get_raw/2,
         global_get_raw_with_breeds/2,
         global_get_raw_with_breeds/3,
         global_get_integer/1,
         global_get_integer/2,
         global_get_float/1,
         global_get_float/2,
         global_get_string/1,
         global_get_string/2,
         global_get_binary/1,
         global_get_binary/2,
         global_get_bool/1,
         global_get_bool/2,
         global_get_mfa/1,
         global_get_map/1,
         global_get_map/2
        ]).


%% Listener API
-export([
         register_listener/2,
         register_listener/3,
         global_register_listener/2,
         global_register_listener/3,
         register_listener_monitor/2,
         register_listener_monitor/3,
         global_register_listener_monitor/2,
         global_register_listener_monitor/3,
         unregister_listener/1,
         monitor/0
        ]).


%%% Feature disabling API
-export([
         is_feature_enabled/2,
         get_feature_uids/1,
         add_feature_uids/2,
         remove_feature_uids/2,
         set_feature_uids/2,
         get_feature_mode/1,
         set_feature_mode/2,
         get_feature_percent/1,
         set_feature_percent/2,
         set_feature_percent/4
        ]).


%% API for TService
-export([
         get_tservice_config/2,
         get_tservice_config/3
        ]).


%% API for configuration
-export([xconfig_enabled/0,
         xconfig_config_path/0,
         xconfig_socket_path/0]).


%% API for debug/testing
-export([
         flush_cache/0,
         delete_key/1,
         global_delete_key/1,
         delete_all/0,
         set/2,
         global_set/2,
         reload/0
        ]).

%%% ===========================================================================
%%% Types
%%% ===========================================================================

-type key_element() :: atom().
-type config_key(KeyElem) :: [KeyElem].
-type config_key() :: config_key(key_element()).
-type raw_config_key() :: config_key(key_element() | binary() | list()).
-opaque transient_raw_config_key() :: {check_existence, raw_config_key()}.
-type breed(KeyElem) :: {BreedKey :: KeyElem, BreedValue :: KeyElem}.
-type breed() :: breed(key_element()).
-type raw_breed() :: breed(key_element() | binary() | list()).
-opaque transient_raw_breeds() :: {check_existence, [raw_breed()]}.
-type simple_value() :: binary() | number() | boolean() | null.
-type value() :: simple_value() | [value()] | #{atom() := value()}.

-type config_change() :: {changed, Key :: config_key(), OldValue :: value(),   NewValue :: value()}
                       | {deleted, Key :: config_key(), OldValue :: value(),   NewValue :: undefined}
                       | {added,   Key :: config_key(), OldValue :: undefined, NewValue :: value()}.

-type listener_mfa() :: {Module :: atom(), Function :: atom(), ExtraArgs :: term()}.

-type optional_result() :: tuenti_config_ets:result(value()).

-export_type([
              config_key/1,
              config_key/0,
              raw_config_key/0,
              transient_raw_config_key/0,
              simple_value/0,
              value/0,
              config_change/0,
              listener_mfa/0,
              optional_result/0,
              breed/1,
              breed/0,
              raw_breed/0,
              transient_raw_breeds/0
             ]).


%%% ===========================================================================
%%% Definitions
%%% ===========================================================================

-define(FLOAT_OPTIONS, [compact, {decimals, 5}]).


%%% ===========================================================================
%%% Start/Stop Functions
%%% ===========================================================================

%%% @doc Starts application tuenti_config.
-spec start() -> ok | {error, term()}.
start() ->
    application:start(tuenti_config).


%%% @doc Stops application tuenti_config.
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(tuenti_config).


%%% ===========================================================================
%%% API Functions
%%% ===========================================================================

%% @doc Converts a local key to a global one (adds the default root)
-spec expand_key(Key :: key_element() | config_key()) -> config_key();
                (RawKey :: transient_raw_config_key()) -> transient_raw_config_key().
expand_key(Key) when is_atom(Key) ->
    {ok, Prefix} = application:get_env(tuenti_config, default_root),
    [Prefix, Key];

expand_key(Key) when is_list(Key) ->
    {ok, Prefix} = application:get_env(tuenti_config, default_root),
    [Prefix | Key];

expand_key({check_existence, Key}) ->
    {check_existence, expand_key(Key)}.


%% @doc Obtains an opaque raw key to use in the rest of the functions, only
%% needed for keys with binary elements.
-spec transient_raw_config_key(raw_config_key()) -> transient_raw_config_key().
transient_raw_config_key(RawKey) ->
    {check_existence, RawKey}.
-spec transient_raw_breeds([raw_breed()]) -> transient_raw_breeds().
transient_raw_breeds(RawBreeds) ->
    {check_existence, RawBreeds}.


%% @doc Gets a value from the local configuration without any type conversion
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
-spec get_raw(Key :: key_element() | config_key() | transient_raw_config_key()) -> value().
get_raw(Key) ->
    global_get_raw(expand_key(Key)).


%% @doc Gets a value from the local configuration without any type conversion.
%% A default value can be specified.
-spec get_raw(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: any()) -> any().
get_raw(Key, Default) ->
    global_get_raw(expand_key(Key), Default).


%% @doc Gets a value from the local configuration as an integer (if the conversion is possible)
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_integer(Key :: key_element() | config_key() | transient_raw_config_key()) -> integer().
get_integer(Key) ->
    global_get_integer(expand_key(Key)).


%% @doc Gets a value from the local configuration as an integer (if the conversion is possible)
%% A default value can be specified
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_integer(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: integer()) -> integer();
                 (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> integer() | undefined.
get_integer(Key, Default) when Default =:= undefined orelse is_integer(Default) ->
    global_get_integer(expand_key(Key), Default).


%% @doc Gets a value from the local configuration as a float (if the conversion is possible)
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_float(Key :: key_element() | config_key() | transient_raw_config_key()) -> float().
get_float(Key) ->
    global_get_float(expand_key(Key)).


%% @doc Gets a value from the local configuration as a float (if the conversion is possible)
%% A default value can be specified
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_float(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: number()) -> float();
               (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> float() | undefined.
get_float(Key, Default) when Default =:= undefined orelse is_number(Default) ->
    global_get_float(expand_key(Key), Default).


%% @doc Gets a value from the local configuration as a string (if the conversion is possible)
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_string(Key :: key_element() | config_key() | transient_raw_config_key()) -> string().
get_string(Key) ->
    global_get_string(expand_key(Key)).


%% @doc Gets a value from the local configuration as a string (if the conversion is possible)
%% A default value can be specified
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_string(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: string()) -> string();
                (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> string() | undefined.
get_string(Key, Default) when is_list(Default) orelse Default == undefined ->
    global_get_string(expand_key(Key), Default).


%% @doc Gets a value from the local configuration as a binary (if the conversion is possible)
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_binary(Key :: key_element() | config_key() | transient_raw_config_key()) -> binary().
get_binary(Key) ->
    global_get_binary(expand_key(Key)).


%% @doc Gets a value from the local configuration as a binary (if the conversion is possible)
%% A default value can be specified
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_binary(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: binary()) -> binary();
                (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> binary() | undefined.
get_binary(Key, Default) when is_binary(Default) orelse Default == undefined ->
    global_get_binary(expand_key(Key), Default).


%% @doc Gets a value from the local configuration as a boolean (if the conversion is possible)
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_bool(Key :: key_element() | config_key() | transient_raw_config_key()) -> boolean().
get_bool(Key) ->
    global_get_bool(expand_key(Key)).


%% @doc Gets a value from the local configuration as a boolean (if the conversion is possible)
%% A default value can be specified
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_bool(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: boolean()) -> boolean();
              (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> boolean() | undefined.
get_bool(Key, Default) when is_boolean(Default) orelse Default == undefined ->
    global_get_bool(expand_key(Key), Default).


%% @doc Gets a value from the local configuration as an MFA tuple (if the conversion is possible)
%% WARNING: Due to YAML limitations the Args element only can contain lists and binaries. No atoms.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_mfa(Key :: key_element() | config_key() | transient_raw_config_key()) -> {atom(), atom(), [value]} | undefined.
get_mfa(Key) ->
    global_get_mfa(expand_key(Key)).


%% @doc Gets a map from the local configuration.
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_map(Key :: key_element() | config_key() | transient_raw_config_key()) -> #{atom() := value()}.
get_map(Key) ->
    global_get_map(expand_key(Key)).


%% @doc Gets a map from the local configuration.
%% A default value can be specified
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec get_map(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: map()) -> map();
             (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> map() | undefined.
get_map(Key, Default) when is_map(Default) orelse Default == undefined ->
    global_get_map(expand_key(Key), Default).


%% @doc Gets a value from the global configuration without any type conversion
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
-spec global_get_raw(Key :: key_element() | config_key() | transient_raw_config_key()) -> value().
global_get_raw(Key) ->
    Key2 = to_config_key(Key),
    get_found_value(Key2, tuenti_config_ets:get_raw(Key2)).


%% @doc Gets a value from the global configuration without any type conversion
%% A default value can be specified
-spec global_get_raw(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: any()) -> any().
global_get_raw(Key, Default) ->
    try to_config_key(Key) of
        Key2 ->
            get_found_value(Key2, tuenti_config_ets:get_raw(Key2, Default))
    catch
        throw:{undefined_config, _} -> Default
    end.


%% @doc Gets a value from the global configuration without any type conversion, using breeds
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
-spec global_get_raw_with_breeds(Key :: key_element() | config_key() | transient_raw_config_key(), [breed()] | transient_raw_breeds()) -> value().
global_get_raw_with_breeds(Key, Breeds) ->
    Key2 = to_config_key(Key),
    Breeds2 = to_config_breeds(Breeds),
    get_found_value(Key2, tuenti_config_ets:get_raw_with_breeds(Key2, Breeds2)).


%% @doc Gets a value from the global configuration without any type conversion,
%% using breeds. A default value can be specified
-spec global_get_raw_with_breeds(Key :: key_element() | config_key() | transient_raw_config_key(), [breed()] | transient_raw_breeds(), Default :: any()) -> any().
global_get_raw_with_breeds(Key, Breeds, Default) ->
    try {to_config_key(Key), to_config_breeds(Breeds)} of
        {Key2, Breeds2} ->
            get_found_value(Key2, tuenti_config_ets:get_raw_with_breeds(Key2, Breeds2, Default))
    catch
        throw:{undefined_config, _} -> Default
    end.


%% @doc Gets a value from the global configuration as an integer (if the conversion is possible)
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_integer(Key :: key_element() | config_key() | transient_raw_config_key()) -> integer().
global_get_integer(Key) ->
    convert_simple_value(Key, integer).


%% @doc Gets a value from the global configuration as an integer (if the conversion is possible)
%% A default value can be specified
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_integer(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: integer()) -> integer();
                        (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> integer() | undefined.
global_get_integer(Key, Default) when Default =:= undefined orelse is_integer(Default) ->
    convert_simple_value(Key, integer, Default).


%% @doc Gets a value from the global configuration as a float (if the conversion is possible)
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_float(Key :: key_element() | config_key() | transient_raw_config_key()) -> float().
global_get_float(Key) ->
    convert_simple_value(Key, float).


%% @doc Gets a value from the global configuration as a float (if the conversion is possible)
%% A default value can be specified
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_float(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: number()) -> float();
                      (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> float() | undefined.
global_get_float(Key, Default) when Default =:= undefined orelse is_number(Default) ->
    convert_simple_value(Key, float, Default).


%% @doc Gets a value from the global configuration as a string (if the conversion is possible)
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_string(Key :: key_element() | config_key() | transient_raw_config_key()) -> string().
global_get_string(Key) ->
    convert_simple_value(Key, string).


%% @doc Gets a value from the global configuration as a string (if the conversion is possible)
%% A default value can be specified
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_string(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: string()) -> string();
                       (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> string() | undefined.
global_get_string(Key, Default) when is_list(Default) orelse Default == undefined ->
    convert_simple_value(Key, string, Default).


%% @doc Gets a value from the global configuration as a binary (if the conversion is possible)
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_binary(Key :: key_element() | config_key() | transient_raw_config_key()) -> binary().
global_get_binary(Key) ->
    convert_simple_value(Key, binary).


%% @doc Gets a value from the global configuration as a binary (if the conversion is possible)
%% A default value can be specified
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_binary(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: binary()) -> binary();
                       (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> binary() | undefined.
global_get_binary(Key, Default) when is_binary(Default) orelse Default == undefined ->
    convert_simple_value(Key, binary, Default).


%% @doc Gets a value from the global configuration as a boolean (if the conversion is possible)
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_bool(Key :: key_element() | config_key() | transient_raw_config_key()) -> boolean().
global_get_bool(Key) ->
    convert_simple_value(Key, boolean).


%% @doc Gets a value from the global configuration as a boolean (if the conversion is possible)
%% A default value can be specified
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_bool(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: boolean()) -> boolean();
                     (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> boolean() | undefined.
global_get_bool(Key, Default) when is_boolean(Default) orelse Default == undefined ->
    convert_simple_value(Key, boolean, Default).


%% @doc Gets a value from the global configuration as an MFA tuple (if the conversion is possible)
%% WARNING: Due to YAML limitations the Args element only can contain tuenti_config:value() values.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_mfa(Key :: key_element() | config_key() | transient_raw_config_key()) -> {atom(), atom(), [tuenti_config:value()]} | undefined.
global_get_mfa(Key) ->
    try to_config_key(Key) of
        Key2 ->
            case tuenti_config_ets:get_raw(Key2) of
                {found, [Module, Function, Args]} when is_list(Args) ->
                    {value_to_type(Key2, atom, Module, true), value_to_type(Key2, atom, Function, true), Args};
                _ ->
                    undefined
            end
    catch
        throw:{undefined_config, _} -> undefined
    end.


%% @doc Get a map from the global configuration.
%% @throws throw:{undefined_config, Key :: config_key() | raw_config_key()} when requesting an unknown configuration key.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_map(Key :: key_element() | config_key() | transient_raw_config_key()) -> #{atom() := tuenti_config:value()}.
global_get_map(Key) ->
    Key2 = to_config_key(Key),
    value_to_type(Key2, map, get_found_value(Key2, tuenti_config_ets:get_raw(Key2)), false).


%% @doc Get a map from the global configuration.
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec global_get_map(Key :: key_element() | config_key() | transient_raw_config_key(), Default :: map()) -> map();
                    (Key :: key_element() | config_key() | transient_raw_config_key(), undefined) -> map() | undefined.
global_get_map(Key, Default) when is_map(Default) orelse Default == undefined ->
    try to_config_key(Key) of
        Key2 ->
            value_to_type(Key2, map, get_found_value(Key2, tuenti_config_ets:get_raw(Key2, Default)), true)
    catch
        throw:{undefined_config, _} -> Default
    end.


%%% ===========================================================================
%%% Listener API functions
%%% ===========================================================================

%% @doc Add a new listener for a config key. Each key can have multiple listeners
%% and a listener can listen for changes of multiple keys
%%
%% It returns the reference() to be able to unregister this listener and the
%% current value of the config.
%%
%% Note that the current value is returned in tuenti_config:optional_result()
%% format because the key may not exist at the registration time.
%%
%% The listener will be called when a config change in the specified key is detected as this:
%% Module:Function(Change :: tuenti_config:config_change(), ExtraArgs :: term()).
%%
%% Note that the Key parameter will be a full global one
-spec register_listener(Key :: key_element() | config_key() | raw_config_key(), ListenerMFA :: listener_mfa())
        -> {ok, ListenerRef :: reference(), CurrentValue :: tuenti_config:optional_result()}.
register_listener(Key, ListenerMFA) ->
    global_register_listener(expand_key(Key), ListenerMFA).

-spec register_listener(Key :: key_element() | config_key() | raw_config_key(), Breeds :: [breed() | raw_breed()], ListenerMFA :: listener_mfa())
        -> {ok, ListenerRef :: reference(), CurrentValue :: tuenti_config:optional_result()}.
register_listener(Key, Breeds, ListenerMFA) ->
    global_register_listener(expand_key(Key), Breeds, ListenerMFA).


%% @doc This is the same as register_listener but using a global key
%% @see register_listener
-spec global_register_listener(Key :: key_element() | config_key() | transient_raw_config_key(), ListenerMFA :: listener_mfa())
        -> {ok, ListenerRef :: reference(), CurrentValue :: tuenti_config:optional_result()}.
global_register_listener(Key, ListenerMFA) ->
    global_register_listener(Key, [], ListenerMFA).

-spec global_register_listener(Key :: key_element() | config_key() | transient_raw_config_key(), Breeds :: [breed()] | transient_raw_breeds(), ListenerMFA :: listener_mfa())
        -> {ok, ListenerRef :: reference(), CurrentValue :: tuenti_config:optional_result()}.
global_register_listener(Key, Breeds, ListenerMFA) ->
    {ok, ListenerRef, CurrentValue, _} = tuenti_config_srv:register_listener(listener_extract_opaque(Key), listener_extract_opaque(Breeds), ListenerMFA),
    {ok, ListenerRef, CurrentValue}.


%% @doc Same as register_listener, but monitoring the process that reports changes.
%% @see register_listener
register_listener_monitor(Key, ListenerMFA) ->
    global_register_listener_monitor(expand_key(Key), ListenerMFA).

register_listener_monitor(Key, Breeds, ListenerMFA) ->
    global_register_listener_monitor(expand_key(Key), Breeds, ListenerMFA).


%% @doc This is the same as register_listener_monitor but using a global key
%% @see register_listener_monitor
-spec global_register_listener_monitor(Key :: key_element() | config_key() | transient_raw_config_key(), ListenerMFA :: listener_mfa())
        -> {ok, ListenerRef :: reference(), CurrentValue :: tuenti_config:optional_result(), MonitorRef :: reference()}.
global_register_listener_monitor(Key, ListenerMFA) ->
    global_register_listener_monitor(Key, [], ListenerMFA).

-spec global_register_listener_monitor(Key :: key_element() | config_key() | transient_raw_config_key(), Breeds :: [breed()] | transient_raw_config_key(), ListenerMFA :: listener_mfa())
        -> {ok, ListenerRef :: reference(), CurrentValue :: tuenti_config:optional_result(), MonitorRef :: reference()}.
global_register_listener_monitor(Key, Breeds, ListenerMFA) ->
    {ok, ListenerRef, CurrentValue, SrvPid} = tuenti_config_srv:register_listener(listener_extract_opaque(Key), listener_extract_opaque(Breeds), ListenerMFA),
    MonitorRef = monitor(process, SrvPid),
    {ok, ListenerRef, CurrentValue, MonitorRef}.


%% @doc Unregister a listener using the reference returned in register_listener functions
-spec unregister_listener(ListenerRef :: reference()) -> ok.
unregister_listener(ListenerRef) ->
    tuenti_config_srv:unregister_listener(ListenerRef).


%% @doc Creates a monitor of the process that keep tracks of notifying the config changes
-spec monitor() -> reference().
monitor() ->
    tuenti_config_srv:monitor().

%%% ===========================================================================
%%% Feature disabling API Functions
%%% ===========================================================================

%% @doc Determines if a feature is enabled or not for a determined user
-spec is_feature_enabled(atom(), binary() | list() | integer()) -> boolean().
is_feature_enabled(Feature, UserId) ->
    try
       tuenti_config_disable:disable_config_decision(Feature, UserId)
    catch
        _:_ -> false
    end.


-spec get_feature_uids(atom()) -> list(binary()).
get_feature_uids(Feature) when is_atom(Feature) ->
    lists:map(fun
                (UID) when is_binary(UID) ->
                    UID;
                (UID) when is_integer(UID) ->
                    integer_to_binary(UID)
                end,
                tuenti_config:get_raw([disable_config, Feature, uids], [])).


-spec add_feature_uids(atom(), list() | binary()) -> ok.
add_feature_uids(Feature, Uids) when is_atom(Feature),
                                     is_list(Uids) ->
    CurrentUidsSorted = lists:sort(get_feature_uids(Feature)),
    NewUids = lists:umerge(CurrentUidsSorted, lists:sort(Uids)),
    tuenti_config:set([disable_config, Feature, uids], NewUids),
    ok;
add_feature_uids(Feature, Uid) when is_atom(Feature),
                                     is_binary(Uid) ->
    add_feature_uids(Feature, [Uid]).

-spec remove_feature_uids(atom(), list() | binary()) -> ok.
remove_feature_uids(Feature, Uids) when is_atom(Feature),
                                        is_list(Uids) ->
    CurrentUids = lists:sort(get_feature_uids(Feature)),
    NewUids = lists:subtract(CurrentUids, Uids),
    tuenti_config:set([disable_config, Feature, uids], NewUids),
    ok;
remove_feature_uids(Feature, Uid) when is_atom(Feature),
                                       is_binary(Uid) ->
  remove_feature_uids(Feature, [Uid]).

-spec set_feature_uids(atom(), list() | binary()) -> ok.
set_feature_uids(Feature, Uids) when is_atom(Feature),
                                     is_list(Uids) ->
    tuenti_config:set([disable_config, Feature, uids], Uids),
    ok;
set_feature_uids(Feature, Uid) when is_atom(Feature),
                                    is_binary(Uid) ->
    set_feature_uids(Feature, [Uid]).


-spec get_feature_mode(atom()) -> atom().
get_feature_mode(Feature) when is_atom(Feature) ->
    binary_to_atom(tuenti_config:get_binary([disable_config, Feature, mode], <<"DISABLED">>), latin1).


-spec set_feature_mode(atom(), 'LIVE' | 'DISABLED' | 'TESTING') -> ok.
set_feature_mode(Feature, Mode) when Mode =:= 'DISABLED';
                                     Mode =:= 'LIVE';
                                     Mode =:= 'TESTING' ->
    tuenti_config:set([disable_config, Feature, mode], atom_to_binary(Mode, latin1)),
    ok.


-spec get_feature_percent(atom()) -> number() | map().
get_feature_percent(Feature) when is_atom(Feature) ->
    case tuenti_config:get_raw([disable_config, Feature, percent], undefined) of
        Value when is_number(Value) ->
            Value;
        Map when is_map(Map) ->
            Map;
        _ ->
            0
  end.

-spec set_feature_percent(atom(), number()) -> ok.
set_feature_percent(Feature, Percent) when is_atom(Feature), is_number(Percent),
                                    Percent >= 0 andalso Percent =< 100 ->
    tuenti_config:set([disable_config, Feature, percent], Percent),
    ok.

-spec set_feature_percent(atom(), number(), boolean(), integer()) -> ok.
set_feature_percent(Feature, Percent, Rotate, Seed) when is_atom(Feature),
                                                         is_number(Percent),
                                                         Percent >= 0 andalso Percent =< 100,
                                                         is_boolean(Rotate),
                                                         is_integer(Seed) ->
    tuenti_config:set([disable_config, Feature, percent], #{value => Percent, rotate => Rotate, seed => Seed}),
    ok.


%%% ===========================================================================
%%% TService API
%%% ===========================================================================

-spec get_tservice_config(Key :: key_element() | config_key(), list(term())) -> term().
get_tservice_config(Key, Params) ->
    get_tservice_config(Key, Params, []).

-spec get_tservice_config(Key :: key_element() | config_key(), list(term()), [breed()] | transient_raw_breeds()) -> term().
get_tservice_config(tservice_urls, [Service], Breeds) when is_binary(Service) ->
    % This configuration key is deprecated
    BinUrls = get_tservice_value_overrides([[serviceConfig, services, Service, service_location]], raw, Breeds, []),
    [binary_to_list(Url) || Url <- BinUrls];
get_tservice_config(UrlKey, [Service | Rest], Breeds) when is_binary(Service), UrlKey == tservice_http_url;
                                                           is_binary(Service), UrlKey == tservice_amqp_url ->
    ConfigNode = case UrlKey of
                   tservice_http_url -> http_location;
                   tservice_amqp_url -> amqp_location
                 end,
    % Second parameter (optional, only if it exists) is ServiceVersion :: integer()
    ConfigPaths = [[serviceConfig, services, Service, ConfigNode]],
    % If there is version in the request, we first need to check for that path
    NewConfigPaths = case Rest of
                             [] ->
                                 ConfigPaths;
                             _ ->
                                 BinVersion = integer_to_binary(hd(Rest)),
                                 [[serviceConfig, services, [Service, <<".">>, BinVersion], ConfigNode] | ConfigPaths]
                         end,

    get_tservice_value_overrides(NewConfigPaths, binary, Breeds, undefined);
get_tservice_config(sample_rate, [Service | Rest], Breeds) when is_binary(Service) ->
    % Second parameter (optional, only if it exists) is ServiceVersion :: integer()
    ConfigPaths = [[serviceConfig, services, Service, service_sample_rate], [serviceConfig, default, service_sample_rate]],
    % If there is version in the request, we first need to check for that path
    NewConfigPaths = case Rest of
                             [] ->
                                 ConfigPaths;
                             _ ->
                                 BinVersion = integer_to_binary(hd(Rest)),
                                 [[serviceConfig, services, [Service, <<".">>, BinVersion], service_sample_rate] | ConfigPaths]
                         end,
    get_tservice_value_overrides(NewConfigPaths, float, Breeds, 0.0);
get_tservice_config(statsd_hosts, _, _Breeds) ->
    Hosts = global_get_raw([statsConfig, statsd, hosts], []),
    case Hosts of
        [] ->
            exit(no_statsd_hosts_configured);
        _ ->
            lists:map(fun(#{host := Hostname, port := Port}) -> {Hostname, Port} end, Hosts)
    end;
get_tservice_config(cache_mode, [Interface, ServiceVersion, CacheName], Breeds) ->
    get_service_cache_value(Interface, ServiceVersion, CacheName, mode, false, binary, Breeds, <<"enabled">>);
get_tservice_config(cache_ttl, [Interface, ServiceVersion, CacheName], Breeds) ->
    get_service_cache_value(Interface, ServiceVersion, CacheName, ttl, false, integer, Breeds, undefined);
get_tservice_config(cache_version, [Interface, ServiceVersion, CacheName], Breeds) ->
    get_service_cache_value(Interface, ServiceVersion, CacheName, version, true, integer, Breeds, undefined);
get_tservice_config(memcache_servers, _, _Breeds) ->
    GlobalFarmAtom = binary_to_atom(tuenti_config:global_get_binary([memcacheServers, global_farm]), latin1),
    Servers = tuenti_config:global_get_raw([memcacheServers, farm_hosts, GlobalFarmAtom]),
    [{binary_to_list(BinIpAddress), Port} || [BinIpAddress, _, Port |_ ] <- Servers];
get_tservice_config(Key, Params, Breeds) when is_atom(Key) ->
    get_tservice_config([Key], Params, Breeds);
get_tservice_config(Key, _, Breeds) when is_list(Key) ->
    global_get_raw_with_breeds(expand_key([tserviceClient] ++ Key), Breeds).


%%% ===========================================================================
%%% Configuration Functions
%%% ===========================================================================

-spec xconfig_enabled() -> true | false.
xconfig_enabled() ->
    {ok, IsEnabled} = application:get_env(tuenti_config, xconfig_enabled),
    IsEnabled.

-spec xconfig_config_path() -> string().
xconfig_config_path() ->
    case application:get_env(tuenti_config, xconfig_config_path, undefined) of
        Path when is_list(Path)->
            Path;
        undefined ->
            case os:getenv("CONFIG_PATH") of
                false ->
                    "";
                EnvPath ->
                    EnvPath
            end
    end.

-spec xconfig_socket_path() -> string().
xconfig_socket_path() ->
    {ok, Path} = application:get_env(tuenti_config, xconfig_socket_path),
    Path.

%%% ===========================================================================
%%% Debug/Test functions
%%% ===========================================================================

-spec flush_cache() -> ok.
flush_cache() ->
    tuenti_config_ets:flush_cache().

%% @doc Delete a key and all its children from the configuration
%% DEBUG/TEST USE ONLY. Race conditions with background updates may happen
%% This will trigger listener callbacks synchronously. Don't call this in a listener callback
-spec delete_key(Key :: key_element() | config_key() | transient_raw_config_key()) -> ok.
delete_key(Key) ->
    global_delete_key(expand_key(Key)).


%% @doc Delete all the keys
%% DEBUG/TEST USE ONLY. Race conditions with background updates may happen
%% This will trigger listener callbacks synchronously. Don't call this in a listener callback
-spec delete_all() -> ok.
delete_all() ->
    tuenti_config_srv:delete_all().


%% @doc Delete a global key from the configuration
%% DEBUG/TEST USE ONLY. Race conditions with background updates may happen
%% This will trigger listener callbacks synchronously. Don't call this in a listener callback
-spec global_delete_key(Key :: key_element() | config_key() | transient_raw_config_key()) -> ok.
global_delete_key(Key) ->
    try to_config_key(Key) of
        Key2 ->
            tuenti_config_srv:delete_key(Key2)
    catch
        throw:{undefined_config, _} -> ok
    end.


%% @doc Sets the configuration value for the specified key.
%% DEBUG/TEST USE ONLY. Race conditions with background updates may happen
%% This will trigger listener callbacks synchronously. Don't call this in a listener callback
-spec set(Key :: key_element() | config_key(), Value :: value()) -> ok.
set(Key, Value) ->
  global_set(expand_key(Key), Value).


%% @doc Sets the configuration value for the specified key.
%% DEBUG/TEST USE ONLY. Race conditions with background updates may happen
%% This will trigger listener callbacks synchronously. Don't call this in a listener callback
-spec global_set(Key :: key_element() | config_key(), Value :: value()) -> ok. % no raw config key allowed here
global_set(Key, Value) ->
  tuenti_config_srv:set(to_config_key(Key), Value).


%% @doc Reloads the config from disk
%% DEBUG/TEST USE ONLY
%% This will trigger listener callbacks. Don't call this in a listener callback
-spec reload() -> ignored | reloaded | nothing_new | {error, term()}.
reload() ->
    case xconfig_enabled() of
        true ->
            ignored;
        _ ->
            tuenti_config_yaml_srv:reload()
    end.

%%% ===========================================================================
%%% Internal Functions
%%% ===========================================================================

-spec to_config_key(key_element() | config_key() | transient_raw_config_key()) -> config_key().
to_config_key(Atom) when is_atom(Atom) ->
    [Atom];
to_config_key(List) when is_list(List) ->
    List;
to_config_key({check_existence, RawKey}) ->
    case atomize_config_path(RawKey) of
        undefined -> throw({undefined_config, RawKey});
        AtomKey -> AtomKey
    end.


-spec to_config_breeds([breed()] | transient_raw_breeds()) -> [breed()].
to_config_breeds(List) when is_list(List) ->
    List;
to_config_breeds({check_existence, RawBreeds}) ->
    lists:filtermap(fun({BreedKey, BreedVal}) ->
                            {BreedKeyIsAtom, BreedKeyAtom} = tuenti_config_srv:config_element_to_atom(BreedKey),
                            {BreedValIsAtom, BreedValAtom} = tuenti_config_srv:config_element_to_atom(BreedVal),
                            if not (BreedValIsAtom and BreedKeyIsAtom) -> false;
                               true -> {true, {BreedKeyAtom, BreedValAtom}}
                            end
                    end, RawBreeds).


%% @doc Extracts the raw config_key or breeds (it may include binaries that may
%% be atoms in the future), returning whether or not to expect them
-spec listener_extract_opaque({check_existence, Raw}) -> {true, Raw} when Raw :: raw_config_key() | raw_breed();
                             (config_key() | key_element() | [breed()]) -> {false, config_key() | [breed()]}.
listener_extract_opaque(Atom) when is_atom(Atom) -> {false, [Atom]};
listener_extract_opaque(List) when is_list(List) -> {false, List};
listener_extract_opaque({check_existence, Raw}) -> {true, Raw}.


%% @doc Tries to get the value from a tuenti_config_ets:result(). If it's not possible an exception is raised
%% @throws throw:{undefined_config, Key :: config_key()}
-spec get_found_value(Key :: config_key(), tuenti_config_ets:result(Value :: term())) -> Value :: term().
get_found_value(Key, not_found) ->
    throw({undefined_config, Key});
get_found_value(_, {found, Value}) ->
    Value.


convert_simple_value(Key, Type, Default) ->
    try to_config_key(Key) of
        Key2 ->
            value_to_type(Key2, Type, get_found_value(Key2, tuenti_config_ets:get_config_simple_value(Key2, Default)), true)
    catch
        throw:{undefined_config, _} -> Default
    end.

convert_simple_value(Key, Type) ->
    Key2 = to_config_key(Key),
    value_to_type(Key2, Type, get_found_value(Key2, tuenti_config_ets:get_config_simple_value(Key2)), false).


-spec error_type_conversion(config_key(), atom(), tuenti_config:value()) -> no_return().
error_type_conversion(Key, Type, Value) ->
    error({invalid_config_type_conversion, {Key, Type, Value}}).


%% @doc Tries to convert a config value to the type specified. It the conversion is not
%% possible an error will be raised
%% @throws error:{invalid_config_type_conversion, {Key :: config_key(), Type :: atom(), Value :: value()}}.
-spec value_to_type(
        Key :: config_key(),
        Type :: integer | float | string | atom | binary | boolean | map,
        Value :: any(),
        IsUndefinedValid :: boolean()
       ) -> number() | binary() | string() | map() | atom();
       (
        Key :: config_key(),
        Type :: raw,
        Value,
        IsUndefinedValid :: boolean()
       ) -> Value.
value_to_type(_, _, undefined, true) ->
    undefined;
value_to_type(_, integer, V, _) when is_integer(V) ->
    V;
value_to_type(_, integer, V, _) when is_float(V) ->
    trunc(V);
value_to_type(Key, integer, V, _) when is_binary(V) ->
    try
        binary_to_integer(V)
    catch
        error:badarg ->
            error_type_conversion(Key, integer, V)
    end;

value_to_type(_, float, V, _) when is_number(V) ->
    V;
value_to_type(Key, float, V, _) when is_binary(V) ->
    try
        binary_to_float(V)
    catch
        error:badarg ->
            error_type_conversion(Key, float, V)
    end;

value_to_type(_, string, V, _) when is_list(V) ->
    V;
value_to_type(_, string, V, _) when is_binary(V) ->
    unicode:characters_to_list(V, utf8);
value_to_type(_, string, V, _) when is_integer(V) ->
    integer_to_list(V);
value_to_type(_, string, V, _) when is_float(V) ->
    float_to_list(V, ?FLOAT_OPTIONS);

value_to_type(_, binary, V, _) when is_binary(V) ->
    V;
value_to_type(_, binary, V, _) when is_integer(V) ->
    integer_to_binary(V);
value_to_type(_, binary, V, _) when is_float(V) ->
    float_to_binary(V, ?FLOAT_OPTIONS);

value_to_type(_, atom, V, _) when is_atom(V) ->
    V;
value_to_type(_, atom, V, _) when is_binary(V) ->
    binary_to_atom(V, latin1);
value_to_type(_, atom, V, _) when is_integer(V) ->
    binary_to_atom(integer_to_binary(V), latin1);
value_to_type(_, atom, V, _) when is_float(V) ->
    binary_to_atom(float_to_binary(V, ?FLOAT_OPTIONS), latin1);

value_to_type(_, boolean, V, _) when is_boolean(V) ->
    V;

value_to_type(_, map, V, _) when is_map(V) ->
    V;

value_to_type(_, raw, V, _) ->
    V;

value_to_type(Key, Type, V, _) ->
    error_type_conversion(Key, Type, V).


%% @doc
%% Converts all elements in a list to already existing atoms. If it's not possible it will
%% return undefined
-spec atomize_config_path([binary() | atom() | iolist()]) -> config_key() | undefined.
atomize_config_path(Path) ->
    atomize_config_path(Path, []).

atomize_config_path([], Acc) ->
    lists:reverse(Acc);
atomize_config_path([Elem | Rest], Acc) ->
    case tuenti_config_srv:config_element_to_atom(Elem) of
        {true, Atom} ->
            atomize_config_path(Rest, [Atom | Acc]);
        _ ->
            undefined
    end.


%% @doc
%% Given a list of configuration paths, this function will try to fetch the configuration value
%% in order until one is found. If no configuration is found the default value is returned
-spec get_tservice_value_overrides(
        ConfigPaths :: list(list(binary() | atom() | iolist())),
        GetType :: raw | binary | float | integer,
        Breeds :: [breed()] | transient_raw_breeds(),
        DefaultValue :: term()) -> term().
get_tservice_value_overrides([], _GetType, _Breeds, DefaultValue) ->
    DefaultValue;
get_tservice_value_overrides([Path | Rest], GetType, Breeds, DefaultValue) ->
    try
        Key = to_config_key(transient_raw_config_key(Path)),
        Breeds2 = to_config_breeds(Breeds),
        value_to_type(Key, GetType, get_found_value(Key, tuenti_config_ets:get_raw_with_breeds(Key, Breeds2)), false)
    catch
        throw:{undefined_config, _} ->
            get_tservice_value_overrides(Rest, GetType, Breeds, DefaultValue)
    end.


%% @doc
%% Get a cache configuration value from serviceConfig, it will try in various places until a value
%% is found, in order:
%% - Specific cache configuration for the specific service and version
%% - Specific cache configuration for the specific service
%% - Global cache configuration for the specific service (this step may be skipped with SkipServiceGlobalConfig = true)
%% - Default cache configuration for ALL services
%%
%% If no value is found, the one passed as DefaultValue will be returned
-spec get_service_cache_value(
        Interface :: binary(),
        ServiceVersion :: integer(),
        CacheName :: binary(),
        ValueNameAtom :: atom(),
        SkipServiceGlobalConfig :: boolean(),
        GetType :: raw | binary | float | integer,
        Breeds :: [breed()] | transient_raw_breeds(),
        DefaultValue :: term()) -> term().
get_service_cache_value(Interface, ServiceVersion, CacheName, ValueNameAtom, SkipServiceGlobalConfig, GetType, Breeds, DefaultValue) ->
    BinServiceVersion = integer_to_binary(ServiceVersion),
    LocalVersionPath = [serviceConfig, services, [Interface, <<".">>, BinServiceVersion], caches, CacheName, ValueNameAtom],
    LocalPath = [serviceConfig, services, Interface, caches, CacheName, ValueNameAtom],
    ServiceGlobalPath = [serviceConfig, services, Interface, caches, global, ValueNameAtom],
    GlobalPath = [serviceConfig, default, caches, ValueNameAtom],

    ConfigPaths = case SkipServiceGlobalConfig of
                      true ->
                          [LocalVersionPath, LocalPath, GlobalPath];
                      false ->
                          [LocalVersionPath, LocalPath, ServiceGlobalPath, GlobalPath]
                  end,

    get_tservice_value_overrides(ConfigPaths, GetType, Breeds, DefaultValue).

