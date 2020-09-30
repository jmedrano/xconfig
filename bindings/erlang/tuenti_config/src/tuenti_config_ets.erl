-module(tuenti_config_ets).

-export([init/0,
         get_raw/1,
         get_raw/2,
         get_raw_with_breeds/2,
         get_raw_with_breeds/3,
         get_config_simple_value/1,
         get_config_simple_value/2,
         set_config/2,
         set_full_config/2,
         delete_key/1,
         delete_all/0,

         get_current_config_version/0,
         disable_config_get_cached_config_values/1,
         disable_config_set_cached_config_values/3
]).

-ifdef(TEST).

-export([
         convert/1,
         update_step0_create_temporary_table/0,
         update_step1_populate_temp_table/3,
         update_step2_delete_backup_table/0,
         update_step3_rename_temp_to_backup_table/0,
         update_step4_create_backup_temp/0,
         update_step5_copy_backup_to_backup_temp/0,
         update_step6_delete_old_main_table/0,
         update_step7_rename_backup_temp_to_main_table/0,

         get_from_full_config/2
    ]).
-endif.

-export([flush_cache/0]).

-type full_config() :: #{atom() := tuenti_config:value()}.
-type found_result(Type) :: {found, Type}.
-type result(Type) :: not_found | found_result(Type).

-type breed(KeyElem) :: tuenti_config:breed(KeyElem).
-type breed_root(KeyElem) :: {KeyElem, breed, BreedKey :: KeyElem, BreedValue :: KeyElem}.
-type breeded_key() :: tuenti_config:config_key() | [breed_root(atom()) | tuenti_config:config_key()].

-export_type([result/1, full_config/0]).

-define(KEY_POS, 1).
-define(VALUE_POS, 2).
-define(CONFIG_TABLE, tuenti_config).
-define(CONFIG_TABLE_TEMP, tuenti_config_temp).
-define(CONFIG_TABLE_BACKUP, tuenti_config_backup).
-define(CONFIG_TABLE_BACKUP_TEMP, tuenti_config_backup_temp).
-define(CONFIG_TABLE_CACHE, tuenti_config_cache).
-define(DEFAULT_TABLE_OPTIONS, [named_table, public, {read_concurrency, true}, {keypos, ?KEY_POS}]).

%%% ===========================================================================
%%% API Functions
%%% ===========================================================================

-spec init() -> ok.
init() ->
    %% the reason for having two tables (one for backup)
    %% is to make sure gets don't fail during the update
    [ try
          ets:new(Table, get_table_options())
      catch
          error:badarg -> % table exists already
              ignore
     end || Table <- [?CONFIG_TABLE, ?CONFIG_TABLE_BACKUP, ?CONFIG_TABLE_CACHE] ],
    ok.


-spec get_config_simple_value(Key :: tuenti_config:config_key()) -> result(tuenti_config:simple_value()).
get_config_simple_value(Key) ->
    case application:get_env(tuenti_config, use_get_raw_for_simple_values) of
        {ok, true} ->
            get_raw(Key);
        _ ->
            safe_lookup(Key)
    end.


-spec get_config_simple_value(Key :: tuenti_config:config_key(), Default :: any()) -> found_result(any()).
get_config_simple_value(Key, Default) ->
    case get_config_simple_value(Key) of
        not_found ->
            {found, Default};
        Result ->
            Result
    end.


-spec get_raw(Key :: tuenti_config:config_key()) -> result(tuenti_config:value()).
get_raw(Key) ->
    get_raw_with_breeds(Key, []).

-spec get_raw(Key :: tuenti_config:config_key(), Default :: any()) -> found_result(any()).
get_raw(Key, Default) ->
    get_raw_with_breeds(Key, [], Default).

-spec get_raw_with_breeds(Key :: tuenti_config:config_key(), [breed(atom())]) -> result(tuenti_config:value()).
get_raw_with_breeds(Key, Breeds) ->
    get_from_full_config_with_cache(Key, Breeds).

-spec get_raw_with_breeds(Key :: tuenti_config:config_key(), [breed(atom())], Default :: any()) -> found_result(any()).
get_raw_with_breeds(Key, Breeds, Default) ->
    case get_from_full_config_with_cache(Key, Breeds) of
        {found, _} = Result ->
            Result;
        _ ->
            {found, Default}
    end.

-spec get_from_full_config_with_cache(Key :: tuenti_config:config_key(), [breed(atom())]) -> result(tuenti_config:value()).
get_from_full_config_with_cache(Key, Breeds) ->
    try
        ConfigVersionCache = config_version(?CONFIG_TABLE),
        CacheLookupKey = key_for_cache(ConfigVersionCache, {from_full_config, Breeds}, Key),
        ets:lookup_element(?CONFIG_TABLE_CACHE, CacheLookupKey, ?VALUE_POS)
    catch
        error:badarg ->
            try
                {ConfigVersion, Result} = get_from_full_config_with_breeds(Key, Breeds),
                NewCacheKey = key_for_cache(ConfigVersion, {from_full_config, Breeds}, Key),
                ets:insert(?CONFIG_TABLE_CACHE, {NewCacheKey, Result}),
                Result
            catch
                error:badarg ->
                    % Return that the value wasn't found without caching anything
                    not_found
            end
    end.


-spec delete_key(Key :: tuenti_config:config_key()) -> true.
delete_key(Key) ->
    % Get the full config map
    NewFullConfig = delete_key_internal(Key, get_current_full_config()),
    set_full_config(NewFullConfig, erlang:unique_integer()),
    true.


-spec delete_all() -> true.
delete_all() ->
    % beware of the race conditions of this functions!
    % e.g. some reads might fall between the deletions and version reinjection
    % (producing a crash)
    ConfigVersion = config_version(?CONFIG_TABLE),
    catch ets:delete_all_objects(?CONFIG_TABLE),
    true = ets:insert(?CONFIG_TABLE, {{metadata, config_version}, ConfigVersion + 1}),
    ConfigVersionBackup = config_version(?CONFIG_TABLE_BACKUP),
    catch ets:delete_all_objects(?CONFIG_TABLE_BACKUP),
    true = ets:insert(?CONFIG_TABLE_BACKUP, {{metadata, config_version}, ConfigVersionBackup + 1}),
    catch ets:delete_all_objects(?CONFIG_TABLE_CACHE),
    true.

-spec disable_config_get_cached_config_values(Feature :: atom()) -> result(any()).
disable_config_get_cached_config_values(Feature) ->
    try
        CacheKey = key_for_cache(config_version(?CONFIG_TABLE), disable_config_values, Feature),
        Result = ets:lookup_element(?CONFIG_TABLE_CACHE, CacheKey, ?VALUE_POS),
        {found, Result}
    catch
        error:badarg ->
            not_found
    end.

-spec disable_config_set_cached_config_values(ConfigVersion :: integer(), Feature :: atom(), ConfigValues :: term()) -> true.
disable_config_set_cached_config_values(ConfigVersion, Feature, ConfigValues) ->
    CacheKey = key_for_cache(ConfigVersion, disable_config_values, Feature),
    ets:insert(?CONFIG_TABLE_CACHE, {CacheKey, ConfigValues}).

-spec get_current_config_version() -> integer().
get_current_config_version() ->
    try
        config_version(?CONFIG_TABLE)
    catch
        error:badarg ->
            config_version(?CONFIG_TABLE_BACKUP)
    end.

-spec flush_cache() -> ok.
flush_cache() ->
    ets:delete_all_objects(?CONFIG_TABLE_CACHE),
    ok.

%%% ===========================================================================
%%% Internal Functions: Breeds
%%% ===========================================================================

get_from_full_config_with_breeds(Key, Breeds) when is_list(Breeds) ->
    try
        breeds_get_with_version(0, not_found, Key, lists:reverse(Breeds))
    catch
        throw:version_mismatch ->
              get_from_full_config_with_breeds(Key, Breeds)
    end;
get_from_full_config_with_breeds(Key, Something) ->
    get_from_full_config_with_breeds(Key, [Something]).

-spec breed_root(KeyElem, atom(), atom()) -> breed_root(KeyElem) when KeyElem :: atom().
breed_root(Root, BreedKey, BreedValue) when is_atom(BreedKey), is_atom(BreedValue) ->
    {Root, breed, BreedKey, BreedValue}.

-spec breeds_get_with_version(
        AccVersion :: integer(),
        AccValue :: result(tuenti_config:map_value()),
        Key :: tuenti_config:config_key(),
        Breeds :: [breed(atom())]
       ) -> {integer(), result(tuenti_config:value())}.
breeds_get_with_version(_AccVersion, not_found, Key, []) ->
    % Nothing in the acc, return whatever it finds
    get_from_full_config_internal_with_version(Key);
breeds_get_with_version(AccVersion, {found, AccMap} = AccValue, Key, []) ->
    case get_from_full_config_internal_with_version(Key) of
        {_Version, not_found} -> % Nothing found, return whatever it had
            {AccVersion, AccValue};
        {AccVersion, {found, Value}} when is_map(Value) -> % Return both maps
            {AccVersion, {found, deep_map_merge(Value, AccMap)}};
        {AccVersion, _FoundNotMapValue} -> % Found not map value, return current map
            {AccVersion, AccValue};
        _ ->
            throw(version_mismatch)
    end;
breeds_get_with_version(AccVersion, AccValue, [KeyRoot | KeyRest] = Key, [{BreedKey, BreedValue} | RestBreeds]) ->
    case get_from_full_config_internal_with_version([breed_root(KeyRoot, BreedKey, BreedValue)| KeyRest]) of
        {Version, Result} when AccValue == not_found -> % Nothing in the acc, continue with whatever it found
            breeds_get_with_version(Version, Result, Key, RestBreeds);
        {_Version, not_found} ->
            % It's unknown if some intermediate level modified the tree, need to
            % traverse it
            case get_from_full_config_internal_with_version([breed_root(KeyRoot, BreedKey, BreedValue)]) of
                {AccVersion, not_found} ->
                    % Breed does not exist, ignore it
                    breeds_get_with_version(AccVersion, AccValue, Key, RestBreeds);
                {AccVersion, {found, BreedTree}} ->
                    case traverse(KeyRest, BreedTree) of
                        {[_|_], NotMap} when not is_map(NotMap) -> % Found not map value, return current map
                            {AccVersion, AccValue};
                        _ -> % Only maps in the path, just ignore them
                            breeds_get_with_version(AccVersion, AccValue, Key, RestBreeds)
                    end;
                _ ->
                    throw(version_mismatch)
            end;
        {AccVersion, {found, Value}} when is_map(Value) -> % Continue by merging both maps
            {found, AccMap} = AccValue,
            breeds_get_with_version(AccVersion, {found, deep_map_merge(Value, AccMap)}, Key, RestBreeds);
        {AccVersion, _FoundNotMapValue} -> % Found not map value, return current map
            {AccVersion, AccValue};
        _ ->
            throw(version_mismatch)
    end;
breeds_get_with_version(AccVersion, AccValue, Key, [_InvalidBreed | RestBreeds]) ->
    breeds_get_with_version(AccVersion, AccValue, Key, RestBreeds).

traverse([], MultiLevelTree) ->
    {[], MultiLevelTree};
traverse([KeyRoot | KeyRest] = Key, MultiLevelTree) ->
    case MultiLevelTree of
        #{KeyRoot := NextMultiLevelTree} ->
            traverse(KeyRest, NextMultiLevelTree);
        _ ->
            {Key, MultiLevelTree}
    end.

%% This is a mismatch between xconfig merge algorithm and breeds merge
%% algorithm.
%% Although the design was for both algorithms to have the same result,
%% at the moment of integrating Erlang with breeds, other languages
%% have the slightly different breeds merge behaviour already.
%%
%% It's been decided that Erlang should rather be consistent with the other
%% languages than match the xconfig merge behaviour exactly.
%%
%% The issue has its roots in the fact that in order to improve efficiency, the
%% breeds list is traversed in inverse order, from highest priority to lowest
%% priority, merging when required and with an early return if it's not
%% possible to merge anything else.
%% The problem triggers with a map with an overwritten subkey, because with
%% the breeds merge algorithm it's not recorded anywhere that some subkey was
%% overwritten. XConfig merge would overwrite the key and that's it, but
%% breeds merge doesn't, just ignores it.
deep_map_merge(M1, M2) when is_map(M1), is_map(M2) ->
    maps:fold(fun(K, V2, Acc) ->
                      case Acc of
                          #{K := V1} ->
                              Acc#{K => deep_map_merge(V1, V2)};
                          _ ->
                              Acc#{K => V2}
                      end
              end, M1, M2);
deep_map_merge(_, Override) ->
    Override.

% @doc Any [X_breeds, BreedKey, BreedValue | Rest] key is transformed into
% [{X, breed, BreedKey, BreedValue} | Rest]
-spec transform_breeds(map()) -> map().
transform_breeds(Config) ->
    maps:fold(fun(Key, Value, Acc) ->
                      try
                          BreedRootBin = atom_to_binary(Key, utf8),
                          {match, [BreedTargetBin]} = re:run(BreedRootBin, "^(.+)_breeds", [{capture, all_but_first, binary}]),
                          BreedTarget = binary_to_existing_atom(BreedTargetBin, utf8),

                          maps:fold(fun(BreedKeyAtom, Value1, Acc1) ->
                                            maps:fold(fun(BreedValueAtom, BreedMap, Acc2) ->
                                                              Acc2#{breed_root(BreedTarget, BreedKeyAtom, BreedValueAtom) => BreedMap}
                                                      end, Acc1, Value1)
                                    end, Acc, Value)

                      catch
                          _:_ -> Acc#{Key => Value}
                      end
              end, #{}, Config).

%%% ===========================================================================
%%% Internal Functions
%%% ===========================================================================

-spec get_current_full_config() -> tuenti_config:full_config().
get_current_full_config() ->
    case catch get_from_full_config_internal_with_version([]) of
        {_, {found, FullConfig}} ->
            FullConfig;
        _ ->
            #{}
    end.


%% @throws error:badarg if the ?CONFIG_TABLE and ?CONFIG_TABLE_BACKUP are unavailable
-spec get_from_full_config_internal_with_version(Key :: breeded_key()) -> {Version :: integer(), result(tuenti_config:value())}.
-ifdef(TEST).
get_from_full_config_internal_with_version(Key) ->
    try
        ?MODULE:get_from_full_config(?CONFIG_TABLE, Key)
    catch
        error:badarg ->
            ?MODULE:get_from_full_config(?CONFIG_TABLE_BACKUP, Key)
    end.
-else.
get_from_full_config_internal_with_version(Key) ->
    try
        get_from_full_config(?CONFIG_TABLE, Key)
    catch
        error:badarg ->
            get_from_full_config(?CONFIG_TABLE_BACKUP, Key)
    end.
-endif.

-spec get_from_full_config(TableId :: ets:tab(), Key :: breeded_key()) -> {Version :: integer(), result(tuenti_config:value())}.
get_from_full_config(TableId, Key) ->
    % Convert the key to a matchspec key
    % Example: [a, b, c] -> #{a => #{b => #{c => '$1'}}}
    KeySpec = lists:foldr(fun(KeyElem, Acc) -> #{KeyElem => Acc} end, '$1', Key),
    ConfigValueMS = {{full_config, KeySpec}, [], [{{value, '$1'}}]},
    ConfigVersionMS = {{{metadata, config_version}, '$1'}, [], [{{version, '$1'}}]},
    {Result, _} = ets:select(TableId, [ConfigValueMS, ConfigVersionMS], 2),
    {version, Version} = proplists:lookup(version, Result),
    case proplists:lookup(value, Result) of
        none -> {Version, not_found};
        {value, Value} -> {Version, {found, Value}}
    end.


-spec key_for_cache(ConfigVersion :: integer(),
                    Category :: atom() | tuple(),
                    Key :: term()) -> tuple().
key_for_cache(ConfigVersion, Category, Key) ->
    {Category, ConfigVersion, Key}.


%% @doc Lookup a value in the ETS tables, it will always return.
%% The "not_found" value will be returned when the value doesn't exist in the
%% table or due to a race condition (with very low probability) if there are
%% very fast and consecutive updates and it doesn't find a valid ETS table
-spec safe_lookup(tuenti_config:config_key()) -> result(tuenti_config:value()).
safe_lookup(Key) ->
    safe_lookup_internal([?CONFIG_TABLE, ?CONFIG_TABLE_BACKUP], Key).


-spec safe_lookup_internal([ets:tab()], tuenti_config:config_key()) -> result(tuenti_config:value()).
safe_lookup_internal([], _) ->
    not_found;
safe_lookup_internal([Table | RestTable], Key) ->
    try ets:lookup(Table, Key) of
        [{_, Value}] ->
            {found, Value};
        _ ->
            not_found
    catch
        error:badarg ->
            % This can happen if the table doesn't exist (during the update process)
            safe_lookup_internal(RestTable, Key)
    end.


-spec set_config(tuenti_config:config_key(), tuenti_config:value()) -> true.
set_config(Key, Value) when is_list(Key) ->
    FullConfig = get_current_full_config(),
    set_full_config(set_config_internal(Key, Value, FullConfig), erlang:unique_integer()),
    true.


-spec set_config_internal(tuenti_config:config_key(), tuenti_config:value(), full_config()) -> full_config().
set_config_internal([], Value, _) when is_map(Value) ->
    % This case is only used when setting the root node (the full config). We force the root value to
    % be a map
    Value;
set_config_internal([Key], Value, Config) when is_map(Config) ->
    Config#{Key => Value};
set_config_internal([Key], Value, _Config) ->
    #{Key => Value};
set_config_internal([Key | KeyRest], Value, Config) when is_map(Config) ->
    case Config of
        #{Key := SubConfig} when is_map(SubConfig) ->
            Config#{Key => set_config_internal(KeyRest, Value, SubConfig)};
        _ when is_map(Config) ->
            Config#{Key => set_config_internal(KeyRest, Value, #{})};
        _ ->
            #{Key => set_config_internal(KeyRest, Value, #{})}
    end.


-spec delete_key_internal(KeyPart :: tuenti_config:config_key(), PartialConfig :: tuenti_config:value()) -> tuenti_config:value().
delete_key_internal([KeyPart], PartialConfig) when is_map(PartialConfig) ->
    maps:remove(KeyPart, PartialConfig);
delete_key_internal([KeyPart | KeyRest], PartialConfig) ->
    case PartialConfig of
        #{KeyPart := SubPartialConfig} ->
            PartialConfig#{KeyPart => delete_key_internal(KeyRest, SubPartialConfig)};
        _ ->
            % Already deleted
            PartialConfig
    end.

%% @doc Updates the full config, must be only called from tuenti_config_srv
-spec set_full_config(FullNewConfig :: map() | undefined, ConfigVersion :: integer()) -> ok.
set_full_config(FullNewConfig, ConfigVersion) ->
    update(FullNewConfig, ConfigVersion).


%%% The update process is broken into individual steps for unit testing
%%% to ensure 'get' functions are working between each of these steps.
-spec update(FullNewConfig :: full_config() | undefined, ConfigVersion :: integer()) -> ok.
update(FullNewConfig, ConfigVersion) ->
    ProcessedFullNewConfig = transform_breeds(FullNewConfig),
    FlattenedNewConfig = convert(ProcessedFullNewConfig),

    update_step0_create_temporary_table(),
    update_step1_populate_temp_table(ProcessedFullNewConfig, FlattenedNewConfig, ConfigVersion),
    update_step2_delete_backup_table(),
    update_step3_rename_temp_to_backup_table(),
    update_step4_create_backup_temp(),
    update_step5_copy_backup_to_backup_temp(),
    update_step6_delete_old_main_table(),
    update_step7_rename_backup_temp_to_main_table(),

    flush_cache().


%%% Two tables are used to guarantee there is always one available during
%%% the configuration update process. Update process:
%%% When updating table A, a new table X is created with the new configuration.
%%% When the configuration is validated, the table X is renamed to B
%%% and a duplicate table of B is created with name C.
%%% Then the table A is deleted and the table C is renamed to A.
%%% Table B is kept around in case a process just failed looking up A.
%%% This way, there is alway one of the table A or B available.

-spec update_step0_create_temporary_table() -> ets:tab().
update_step0_create_temporary_table() ->
    % Create a 'temporary' table
    ?CONFIG_TABLE_TEMP = ets:new(?CONFIG_TABLE_TEMP, get_table_options()).

-spec update_step1_populate_temp_table(map(), tuenti_config:flattened_config(), integer()) -> true.
update_step1_populate_temp_table(FullNewConfig, FlattenedNewConfig, ConfigVersion) ->
    % Load the new configuration in the 'temporary' table
    true = ets:insert(?CONFIG_TABLE_TEMP, FlattenedNewConfig),
    % The normal config only allows list(atom()) as keys, so there shouldn't be any problem with this key
    true = ets:insert(?CONFIG_TABLE_TEMP, {{metadata, config_version}, ConfigVersion}),
    true = ets:insert(?CONFIG_TABLE_TEMP, {full_config, FullNewConfig}).

-spec update_step2_delete_backup_table() -> true.
update_step2_delete_backup_table() ->
    % Delete the current backup table if it exists
    catch ets:delete(?CONFIG_TABLE_BACKUP).

-spec update_step3_rename_temp_to_backup_table() -> ets:tab().
update_step3_rename_temp_to_backup_table() ->
    % Rename the temporary table to be used as backup table
    ets:rename(?CONFIG_TABLE_TEMP, ?CONFIG_TABLE_BACKUP).

-spec update_step4_create_backup_temp() -> ets:tab().
update_step4_create_backup_temp() ->
    % Create a the second 'temporary' table backup
    ?CONFIG_TABLE_BACKUP_TEMP = ets:new(?CONFIG_TABLE_BACKUP_TEMP, get_table_options()).

-spec update_step5_copy_backup_to_backup_temp() -> true.
update_step5_copy_backup_to_backup_temp() ->
    % Copy the new configuration to the second table backup
    Data = ets:tab2list(?CONFIG_TABLE_BACKUP),
    true = ets:insert(?CONFIG_TABLE_BACKUP_TEMP, Data).

-spec update_step6_delete_old_main_table() -> true.
update_step6_delete_old_main_table() ->
    % Delete the old configuration table
    ets:delete(?CONFIG_TABLE).

-spec update_step7_rename_backup_temp_to_main_table() -> ets:tab().
update_step7_rename_backup_temp_to_main_table() ->
    % Rename the 'temporary' backup table to be the new main table
    ets:rename(?CONFIG_TABLE_BACKUP_TEMP, ?CONFIG_TABLE).


-spec get_table_options() -> list().
get_table_options() ->
    case whereis(tuenti_config_srv) of
        undefined ->
            ?DEFAULT_TABLE_OPTIONS;
        SrvPid ->
            % If the owner dies, the new owner will be the tuenti_config_srv process
            [{heir, SrvPid, undefined} | ?DEFAULT_TABLE_OPTIONS]
    end.

-spec config_version(atom()) -> integer().
config_version(Table) ->
      ets:lookup_element(Table, {metadata, config_version}, ?VALUE_POS).


%% @doc Converts from C format to Erlang format
%% For example:
%% YAML
%% ----
%% chatServer:
%%   key1: 56
%%   key2: 28
%%
%% will generate from C something like this:
%% #{chatServer => #{key1 => 56, key2 => 28}}
%%
%% and we transform that to
%% [{[chatServer, key1], 56}, {[chatServer, key2], 28}]
%%
%% With this format we can use the first element of the tuple (a list) as the key for retrieving hierarchical information
%%
%% If there is a map inside any list, the keys won't be joined, and a normal Erlang map will be returned. For example:
%% YAML
%% ----
%% chatServer:
%%   testdict: [{map1_a: "hi", map1_other: 4}, {map2_whatever: 14}]
%%
%% From C we receive:
%% #{chatServer => #{testdict => [#{map1_a => <<"hi">>, map1_other => 4}, #{map2_whatever => 14}]}}
%% And we transform it to:
%% [{[chatServer,testdict],
%%   [#{map1_a => <<"hi">>,map1_other => 4},#{map2_whatever => 14}]}]

-spec convert(map()) -> tuenti_config:flattened_config().
convert(Config) when is_map(Config) ->
    convert(Config, true, [], []).


-spec convert(ConfigFromC :: term(),
              AddDictNameToKey :: boolean(),
              Key :: list(atom()) | undefined,
              ConvertedConfig :: tuenti_config:flattened_config()) -> tuenti_config:flattened_config().
convert(M, true, Key, GlobalConfig) when is_map(M) ->
    maps:fold(fun(K, V, Acc) ->
                  convert(V, true, Key ++ [K], Acc)
              end,
              GlobalConfig,
              M);
convert(L, _, Key, GlobalConfig) when is_list(L) ->
    ConvertedElements = lists:foldl(fun(Elem, Acc) ->
                                        convert(Elem, false, undefined, Acc)
                                    end,
                                    [],
                                    L),
    [add_element(Key, lists:reverse(ConvertedElements)) | GlobalConfig];

convert(Value, _, Key, GlobalConfig) ->
    [add_element(Key, Value) | GlobalConfig].

add_element(undefined, Element) ->
    Element;
add_element(Key, Element) ->
    {Key, Element}.

