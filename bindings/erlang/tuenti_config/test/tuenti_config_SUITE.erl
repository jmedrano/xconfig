%%% Unit test for the tuenti_config module.
-module(tuenti_config_SUITE).

-compile(export_all).


%%% ===========================================================================
%%% Includes
%%% ===========================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("tu_eunit_macros.hrl").


%%% ===========================================================================
%%% Macros
%%% ===========================================================================

-define(TIMEOUT, 1000).
% compatible for linux & osx
-define(TMPFILECMD, "mktemp 2>/dev/null || mktemp -t 'mytmpfile'").
-define(TMPDIRCMD, "mktemp -d 2>/dev/null || mktemp -d -t 'mytmp'").
-define(CONFIG_FILENAME, "chatServer.yaml").

 %% MUST BE THE SAME AS DEFAULT VALUE IN .APP.SRC
-define(DEFAULT_ROOT, defaultRoot).

%%% ===========================================================================
%%% common_test callbacks
%%% ===========================================================================

all() ->
    [
     undefined_config,
     bad_config,
     get_while_updating,
     reload_config,
     reload_override,

     register_listener_simple_values,
     unregister_listener,

     register_listener_prefix,
     register_listener_map,

     monitor,

     config_override,
     linked_config,
     get,
     get_with_default,
     set,
     set_get_map,
     disable_config,
     disable_config_changes,

     delete_key,
     delete_all,

     safe_init,
     set_notify,
     failed_type_conversion
    ].

suite() -> [{timetrap, {seconds, 10}}].


init_per_suite(Conf) -> Conf.


end_per_suite(_Conf) -> ok.


init_per_testcase(_TestCase, Conf) ->
    ConfDir = store_terms(config_terms()),
    OverrideDir = store_terms(["l: m"]),
    application:load(tuenti_config),
    application:set_env(tuenti_config, xconfig_enabled, false),
    application:set_env(tuenti_config, xconfig_config_path, ConfDir ++ ":" ++ OverrideDir),
    {ok, _} = application:ensure_all_started(tuenti_config),
    tuenti_config:flush_cache(),
    [{conf_dir, ConfDir}, {override_dir, OverrideDir} |Conf].


end_per_testcase(_, Config) ->
    application:stop(tuenti_config),
    application:unload(tuenti_config),
    application:unset_env(tuenti_config, xconfig_enabled),
    ConfDir = proplists:get_value(conf_dir, Config),
    OverrideDir = proplists:get_value(override_dir, Config),
    remove_directory(ConfDir),
    remove_directory(OverrideDir),
    case flush_notifications() of
        [] -> ok;
        Messages -> ct:pal("Messages still in inbox: ~p", [Messages])
    end,
    ok.


%%% ===========================================================================
%%% Tests
%%% ===========================================================================

reload_config(Config) ->
    ConfigDir = proplists:get_value(conf_dir, Config),
    write_terms(ConfigDir, ["spam: 1", "bacon: a"]),
    SpamExpandedKey = tuenti_config:expand_key(spam),
    BaconExpandedKey = tuenti_config:expand_key(bacon),
    ?assertThrow({undefined_config, SpamExpandedKey}, tuenti_config:get_raw(spam)),
    ?assertThrow({undefined_config, BaconExpandedKey}, tuenti_config:get_raw(bacon)),
    tuenti_config:reload(),
    ?assertEqual(1, tuenti_config:get_raw(spam)),
    ?assertEqual(<<"a">>, tuenti_config:get_raw(bacon)).


reload_override(Config) ->
    OverrideDir = proplists:get_value(override_dir, Config),
    write_terms(OverrideDir, ["spam: 1", "bacon: a"]),
    SpamExpandedKey = tuenti_config:expand_key(spam),
    BaconExpandedKey = tuenti_config:expand_key(bacon),
    ?assertThrow({undefined_config, SpamExpandedKey}, tuenti_config:get_raw(spam)),
    ?assertThrow({undefined_config, BaconExpandedKey}, tuenti_config:get_raw(bacon)),
    tuenti_config:reload(),
    ?assertEqual(1, tuenti_config:get_raw(spam)),
    ?assertEqual(<<"a">>, tuenti_config:get_raw(bacon)).


get(_Config) ->
    SpamExpandedKey = tuenti_config:expand_key(spam),
    ?assertThrow({undefined_config, SpamExpandedKey}, tuenti_config:get_raw(spam)),
    ?assertMatch(45, tuenti_config:get_integer(integer)),
    ?assertMatch("spam", tuenti_config:get_string(string)),
    ?assertMatch("spam", tuenti_config:get_string(tuenti_config:transient_raw_config_key([<<"string">>]))),
    ?assertMatch([1, 2, 3], tuenti_config:get_raw(list)),
    ?assertMatch(<<"bacon">>, tuenti_config:get_binary(binary)),
    ?assertMatch(false, tuenti_config:get_bool(atom)),
    ?assertMatch(null, tuenti_config:get_raw(null_value)),
    ?assertMatch([[<<"a">>, [1, <<"toto">>], <<"foo">>],
                  [<<"b">>, [2, <<"tata">>], <<"bar">>]],
                 tuenti_config:get_raw(complex)),
    ?assertEqual(#{
                   something => [<<"a">>, <<"b">>, <<"c">>],
                   other => <<"whatever">>,
                   submap => #{a => 1, b => 2}
                  }, tuenti_config:get_raw(map)),
    ?assertEqual([#{a => 1}, #{b => 2, c => 3}], tuenti_config:get_raw(map_in_list)),
    ok.


get_with_default(_Config) ->
    ?assertMatch(42, tuenti_config:get_raw(spam, 42)),
    ?assertMatch(42, tuenti_config:get_raw(tuenti_config:transient_raw_config_key([<<"spam">>]), 42)),
    ?assertMatch(42, tuenti_config:get_raw(tuenti_config:transient_raw_config_key([non_atom_binary()]), 42)),
    ?assertMatch(45, tuenti_config:get_integer(integer, 18)),
    ?assertMatch("spam", tuenti_config:get_string(string, "foo")),
    ?assertMatch([1, 2, 3], tuenti_config:get_raw(list, [])),
    ?assertMatch(<<"bacon">>, tuenti_config:get_binary(binary, <<"egg">>)),
    ?assertMatch(false, tuenti_config:get_bool(atom, true)),
    ?assertMatch(null, tuenti_config:get_raw(null_value, undefined)),
    ?assertMatch([[<<"a">>, [1, <<"toto">>], <<"foo">>],
                  [<<"b">>, [2, <<"tata">>], <<"bar">>]],
                 tuenti_config:get_raw(complex, undefined)),
    ?assertEqual(#{
                   something => [<<"a">>, <<"b">>, <<"c">>],
                   other => <<"whatever">>,
                   submap => #{a => 1, b => 2}
                  }, tuenti_config:get_raw(map, undefined)),
    ?assertEqual([#{a => 1}, #{b => 2, c => 3}], tuenti_config:get_raw(map_in_list, undefined)),

    ?assertEqual(#{foo => bar}, tuenti_config:get_raw(not_existing_key, #{foo => bar})),
    ok.


set(_Config) ->
    SpamExpandedKey = tuenti_config:expand_key(spam),
    ?assertThrow({undefined_config, SpamExpandedKey}, tuenti_config:get_raw(spam)),
    ?assertMatch(ok, tuenti_config:set(spam, 66)),
    ?assertMatch(66, tuenti_config:get_raw(spam)),
    ?assertMatch(ok, tuenti_config:set(spam, "foo")),
    ?assertMatch("foo", tuenti_config:get_raw(spam)),
    ?assertMatch(ok, tuenti_config:set(spam, [a, b, c])),
    ?assertMatch([a, b, c], tuenti_config:get_raw(spam)),
    ?assertMatch(ok, tuenti_config:set(spam, <<"beans">>)),
    ?assertMatch(<<"beans">>, tuenti_config:get_raw(spam)),
    ?assertMatch(ok, tuenti_config:set(spam, undefined)),
    ?assertMatch(undefined, tuenti_config:get_raw(spam)),
    ?assertMatch(ok, tuenti_config:set(spam, #{map => #{submap => <<"something">>}, list => [1, 2, 3]})),
    ?assertEqual(#{map => #{submap => <<"something">>}, list => [1, 2, 3]}, tuenti_config:get_raw(spam)),
    ok.


set_get_map(_Config) ->
    ?assertEqual(undefined, tuenti_config:get_map(spam, undefined)),

    ?assertEqual(ok, tuenti_config:set(spam, #{
        eggs => #{
            yolk => 2,
            white => duh
        },
        bacon => <<"pork">>
    })),

    ?assertEqual(#{
        eggs => #{
            yolk => 2,
            white => duh
        },
        bacon => <<"pork">>
    }, tuenti_config:get_map([spam])),

    ?assertEqual(#{
        yolk => 2,
        white => duh
    }, tuenti_config:get_map([spam, eggs])),

    ?assertEqual(2, tuenti_config:get_raw([spam, eggs, yolk])),
    ?assertEqual(duh, tuenti_config:get_raw([spam, eggs, white])),
    ?assertEqual(<<"pork">>, tuenti_config:get_raw([spam, bacon])),

    ?assertEqual(ok, tuenti_config:set([spam, eggs], protein)),
    ?assertEqual(protein, tuenti_config:get_raw([spam, eggs])),
    ?assertEqual(<<"pork">>, tuenti_config:get_raw([spam, bacon])),

    ?assertEqual(#{
        eggs => protein,
        bacon => <<"pork">>
    }, tuenti_config:get_map([spam])),

    ?assertEqual(ok, tuenti_config:set(spam, #{})),
    ?assertEqual(#{}, tuenti_config:get_raw(spam)),

    ?assertEqual(ok, tuenti_config:delete_key(spam)),
    ?assertEqual(undefined, tuenti_config:get_raw(spam, undefined)),
    ok.


bad_config(_Config) ->
    application:load(tuenti_config),
    ConfDir = store_terms(["good: 18", "bad-66"]),
    application:set_env(tuenti_config, xconfig_config_path, ConfDir),
    ?assertMatch({error, _}, tuenti_config:start()),
    remove_directory(ConfDir),
    ok.


get_while_updating(_Config) ->
    application:stop(tuenti_config),
    application:unload(tuenti_config),
    ConfDir = store_terms(["spam: 1"]),
    application:load(tuenti_config),
    application:set_env(tuenti_config, xconfig_config_path, ConfDir),
    ok = tuenti_config:start(),

    tuenti_config_ets:update_step0_create_temporary_table(),
    % get_raw gets the value from the full_config
    % get_integer gets the value from flattened config
    % So we try both of them
    ?assertMatch(1, tuenti_config:get_raw(spam)),
    ?assertMatch(1, tuenti_config:get_integer(spam)),
    NewConfig = #{?DEFAULT_ROOT => #{spam => 2}},
    NewFlattenedConfig = tuenti_config_ets:convert(NewConfig),
    tuenti_config_ets:update_step1_populate_temp_table(NewConfig, NewFlattenedConfig, 0),
    ?assertMatch(1, tuenti_config:get_raw(spam)),
    ?assertMatch(1, tuenti_config:get_integer(spam)),
    tuenti_config_ets:update_step2_delete_backup_table(),
    ?assertMatch(1, tuenti_config:get_raw(spam)),
    ?assertMatch(1, tuenti_config:get_integer(spam)),
    tuenti_config_ets:update_step3_rename_temp_to_backup_table(),
    ?assertMatch(1, tuenti_config:get_raw(spam)),
    ?assertMatch(1, tuenti_config:get_integer(spam)),
    tuenti_config_ets:update_step4_create_backup_temp(),
    ?assertMatch(1, tuenti_config:get_raw(spam)),
    ?assertMatch(1, tuenti_config:get_integer(spam)),
    tuenti_config_ets:update_step5_copy_backup_to_backup_temp(),
    ?assertMatch(1, tuenti_config:get_raw(spam)),
    ?assertMatch(1, tuenti_config:get_integer(spam)),
    tuenti_config_ets:update_step6_delete_old_main_table(),
    ?assertMatch(2, tuenti_config:get_raw(spam)),
    ?assertMatch(2, tuenti_config:get_integer(spam)),
    tuenti_config_ets:update_step7_rename_backup_temp_to_main_table(),
    ?assertMatch(2, tuenti_config:get_raw(spam)),
    ?assertMatch(2, tuenti_config:get_integer(spam)),

    remove_directory(ConfDir),

    ok.


register_listener_simple_values(Config) ->
    % we get notifications for changes to a key
    ConfigDir = proplists:get_value(conf_dir, Config),
    write_terms(ConfigDir, ["spam: 3"]),
    tuenti_config:reload(),
    flush_notifications(),
    ?assertMatch({ok, _, {found, 3}}, tuenti_config:register_listener(spam, {?MODULE, update_callback, self()})),
    ?assertMatch({ok, _, not_found}, tuenti_config:register_listener(bacon, {?MODULE, update_callback, self()})),
    ?assertMatch({ok, _, not_found}, tuenti_config:register_listener(bacon, {?MODULE, update_callback, self()})),
    ?assertMatch({ok, _, not_found}, tuenti_config:register_listener(tuenti_config:transient_raw_config_key([<<"bacon">>]), {?MODULE, update_callback, self()})),
    % ^ Notice that Bacon registers only twice because the keys are different (binary
    % vs atom)
    write_terms(ConfigDir, ["spam: 18"]),
    tuenti_config:reload(),
    SpamExpandedKey = tuenti_config:expand_key(spam),
    BaconExpandedKey = tuenti_config:expand_key(bacon),
    ?assertRecv({changed, SpamExpandedKey, 3, 18}, ?TIMEOUT),
    write_terms(ConfigDir, ["bacon: 32"]),
    tuenti_config:reload(),
    ?assertRecv({deleted, SpamExpandedKey, 18, undefined}, ?TIMEOUT),
    ?assertRecv({added, BaconExpandedKey, undefined, 32}, ?TIMEOUT),
    ?assertRecv({added, BaconExpandedKey, undefined, 32}, ?TIMEOUT), % Bacon triggers twice
    write_terms(ConfigDir, ["bacon: 32", "spam: 33"]),
    tuenti_config:reload(),
    ?assertRecv({added, SpamExpandedKey, undefined, 33}, ?TIMEOUT),
    write_terms(ConfigDir, ["spam: 33"]),
    tuenti_config:reload(),
    ?assertRecv({deleted, BaconExpandedKey, 32, undefined}, ?TIMEOUT),
    ?assertRecv({deleted, BaconExpandedKey, 32, undefined}, ?TIMEOUT), % Bacon triggers twice

    NonAtomBinary = non_atom_binary(),
    OpaqueBinaryConfigKey = tuenti_config:transient_raw_config_key([NonAtomBinary]),
    ?assertMatch({ok, _, not_found}, tuenti_config:register_listener(OpaqueBinaryConfigKey, {?MODULE, update_callback, self()})),
    write_terms(ConfigDir, [binary_to_list(NonAtomBinary)++": 32"]),
    tuenti_config:reload(),
    ?assertRecv({deleted, SpamExpandedKey, 33, undefined}, ?TIMEOUT),
    NowExistingBinary = binary_to_existing_atom(NonAtomBinary, utf8),
    ?assertRecv({added, [_, NowExistingBinary], undefined, 32}, ?TIMEOUT),

    % No other updates are left
    ?assertNotRecvAnything(?TIMEOUT),
    ok.


unregister_listener(Config) ->
    % we are able to stop getting notifications for changes to a key
    ConfigDir = proplists:get_value(conf_dir, Config),
    write_terms(ConfigDir, ["spam: 3"]),
    tuenti_config:reload(),
    flush_notifications(),
    {ok, ListenerRef, {found, 3}} = tuenti_config:register_listener(spam, {?MODULE, update_callback, self()}),
    {ok, ListenerRef, {found, 3}} = tuenti_config:register_listener(spam, {?MODULE, update_callback, self()}), % double register is ignored
    write_terms(ConfigDir, ["spam: 18"]),
    tuenti_config:reload(),
    SpamExpandedKey = tuenti_config:expand_key(spam),
    ?assertRecv({changed, SpamExpandedKey, 3, 18}, ?TIMEOUT),
    tuenti_config:unregister_listener(ListenerRef),
    write_terms(ConfigDir, ["spam: 32"]),
    tuenti_config:reload(),
    ?assertNotRecv({changed, SpamExpandedKey, 32}, ?TIMEOUT),
    ok.


register_listener_prefix(Config) ->
    % we get notifications for changes to keys under a prefix
    ConfigDir = proplists:get_value(conf_dir, Config),
    write_terms(ConfigDir, ["food:", "  spam: 3"]),
    tuenti_config:reload(),
    flush_notifications(),
    ?assertMatch({ok, _, {found, #{spam := 3}}}, tuenti_config:register_listener(food, {?MODULE, update_callback, self()})),
    write_terms(ConfigDir, ["food:", "  spam: 5"]),
    tuenti_config:reload(),
    FoodExpandedKey = tuenti_config:expand_key([food]),
    ?assertRecv({changed, FoodExpandedKey, #{spam := 3}, #{spam := 5}}, ?TIMEOUT),
    write_terms(ConfigDir, ["food:", "  bacon: 2"]),
    tuenti_config:reload(),
    ?assertRecv({changed, FoodExpandedKey, #{spam := 5}, #{bacon := 2}}, ?TIMEOUT),
    write_terms(ConfigDir, ["food:", "  spam: 5", "  bacon: 2"]),
    tuenti_config:reload(),
    ?assertRecv({changed, FoodExpandedKey, #{bacon := 2}, #{spam := 5, bacon := 2}}, ?TIMEOUT),
    write_terms(ConfigDir, ["food:", "  spam: 5"]),
    tuenti_config:reload(),
    ?assertRecv({changed, FoodExpandedKey, #{spam := 5, bacon := 2}, #{spam := 5}}, ?TIMEOUT),
    write_terms(ConfigDir, []),
    tuenti_config:reload(),
    ?assertRecv({deleted, FoodExpandedKey, #{spam := 5}, undefined}, ?TIMEOUT),
    ok.


register_listener_map(Config) ->
    ConfigDir = proplists:get_value(conf_dir, Config),

    %%% base config
    write_terms(ConfigDir, [
        "spam:",
        "  eggs:",
        "    yolk: 2",
        "    white: duh",
        "  bacon: pork"
    ]),

    InitValue = #{
                  eggs => #{
                            yolk => 2,
                            white => <<"duh">>
                           },
                  bacon => <<"pork">>
                 },

    tuenti_config:reload(),
    {ok, ListenerRef, {found, InitValue}} = tuenti_config:register_listener([spam], {?MODULE, update_callback, self()}),

    %%% change a leaf value
    write_terms(ConfigDir, [
        "spam:",
        "  eggs:",
        "    yolk: 3", % <-- this is the change
        "    white: duh",
        "  bacon: pork"
    ]),
    tuenti_config:reload(),
    SpamExpandedKey = tuenti_config:expand_key(spam),
        ?assertRecv({changed, SpamExpandedKey, InitValue, #{
        eggs := #{
            yolk := 3,
            white := <<"duh">>
        },
        bacon := <<"pork">>
    }}, ?TIMEOUT),

    %%% remove a direct child
    write_terms(ConfigDir, [
        "spam:",
        % no eggs value anymore
        "  bacon: pork"
    ]),
    tuenti_config:reload(),
    SpamExpandedKey = tuenti_config:expand_key(spam),
    ?assertRecv({changed, SpamExpandedKey, _, #{
        bacon := <<"pork">>
    }}, ?TIMEOUT),

    %%% unregistering does not send messages anymore
    tuenti_config:unregister_listener(ListenerRef),

    write_terms(ConfigDir, [
        "spam:",
        "  bacon: forbidden"
    ]),
    tuenti_config:reload(),
    ?assertNotRecvAnything(?TIMEOUT),

    ok.


monitor(_Config) ->
    Ref = tuenti_config:monitor(),
    gen_server:stop(tuenti_config_srv, test, infinity),
    receive
        {'DOWN', Ref, process, {tuenti_config_srv, _Node}, test} ->
            ok
    after
        1000 ->
            throw(no_monitor_message)
    end.


config_override(Config) ->
    OverrideDir = proplists:get_value(override_dir, Config),
    ?assertNotRecvAnything(?TIMEOUT),
    StringExpandedKey = tuenti_config:expand_key(string),
    AtomExpandedKey = tuenti_config:expand_key(atom),
    NewExpandedKey = tuenti_config:expand_key(new),
    ?assertMatch({ok, _, {found, <<"spam">>}}, tuenti_config:register_listener(string, {?MODULE, update_callback, self()})),
    ?assertMatch({ok, _, {found, false}}, tuenti_config:register_listener(atom, {?MODULE, update_callback, self()})),
    ?assertMatch({ok, _, not_found}, tuenti_config:register_listener(new, {?MODULE, update_callback, self()})),
    tuenti_config:reload(),
    flush_notifications(),
    write_terms(OverrideDir, ["string: overridden",
                              "atom: true"]),
    tuenti_config:reload(),

    ?assertRecv({changed, StringExpandedKey, <<"spam">>, <<"overridden">>}, ?TIMEOUT),
    ?assertRecv({changed, AtomExpandedKey, false, true}, ?TIMEOUT),
    ?assertMatch(45, tuenti_config:get_integer(integer)),
    ?assertMatch("overridden", tuenti_config:get_string(string)),
    ?assertMatch(<<"bacon">>, tuenti_config:get_binary(binary)),
    ?assertMatch(true, tuenti_config:get_bool(atom)),

    write_terms(OverrideDir, ["string: overridden",
                              "atom: true",
                              "new: foo"]),

    tuenti_config:reload(),
    ?assertRecv({added, NewExpandedKey, undefined, <<"foo">>}, ?TIMEOUT),
    ?assertMatch(<<"foo">>, tuenti_config:get_binary(new)),
    ok.


linked_config(_Config) ->
    application:stop(tuenti_config),
    application:unload(tuenti_config),

    ConfDir = mktempdir(),
    LinkDir = mktempdir(),
    LinkFile = filename:join([LinkDir, ?CONFIG_FILENAME]),
    ConfFile = filename:join([ConfDir, ?CONFIG_FILENAME]),

    %% Create the configuration file
    write_terms(ConfDir, ["spam: 1"]),
    %% Create a link to the configuration file
    file:make_symlink(ConfFile, LinkFile),

    application:load(tuenti_config),
    application:set_env(tuenti_config, xconfig_config_path, LinkDir),
    ok = tuenti_config:start(),
    ?assertMatch(1, tuenti_config:get_integer(spam)),
    SpamExpandedKey = tuenti_config:expand_key(spam),
    {ok, _, {found, 1}} = tuenti_config:register_listener(spam, {?MODULE, update_callback, self()}),
    write_terms(ConfDir, ["spam: 2"]),
    flush_notifications(),
    tuenti_config:reload(),
    ?assertRecv({changed, SpamExpandedKey, 1, 2}, ?TIMEOUT),
    write_terms(ConfDir, ["spam: 2"]),
    tuenti_config:reload(),
    ?assertNotRecvAnything(?TIMEOUT),
    write_terms(ConfDir, ["spam: 3"]),
    tuenti_config:reload(),
    ?assertRecv({changed, SpamExpandedKey, 2, 3}, ?TIMEOUT),

    remove_directory(ConfDir),
    remove_directory(LinkDir),
    ok.


undefined_config(_Config) ->
    SpamExpandedKey = tuenti_config:expand_key(spam),
    ?assertThrow({undefined_config, SpamExpandedKey}, tuenti_config:get_raw(spam)),
    ?assertMatch(ok, tuenti_config:set(spam, 42)),
    ?assertMatch(42, tuenti_config:get_raw(spam)),

    ok = tuenti_config:stop(),
    application:set_env(tuenti_config, config, undefined),
    application:set_env(tuenti_config, override, undefined),
    ok = tuenti_config:start(),

    ?assertThrow({undefined_config, SpamExpandedKey}, tuenti_config:get_raw(spam)),
    ?assertMatch(ok, tuenti_config:set(spam, 42)),
    ?assertMatch(42, tuenti_config:get_raw(spam)).


disable_config(_Config) ->
    ?assertEqual(false, tuenti_config:is_feature_enabled(some_not_existing_feature, <<"1234">>)),

    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_disabled, <<"1234">>)),
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_disabled, <<"9876">>)),
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_disabled, <<"0">>)),
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_disabled, <<"">>)),

    % Enabled for everybody, (only good UIds)
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_enabled, <<"">>)),
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_enabled, <<"AA">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_enabled, <<"1234">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_enabled, <<"9876">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_enabled, <<"0">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_enabled, <<"10000">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_enabled, <<"0010000">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_enabled, <<"10001">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_enabled, <<"9999">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_enabled, <<"09999">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_10, <<"0">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_10, <<"1">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_10, <<"100999">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_10, <<"00100999">>)),
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_10, <<"101000">>)),
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_10, <<"00101000">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_10, <<"9876">>)), % Exception
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_10, <<"9875">>)),
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_10, <<"9877">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_10, <<"1456">>)), % Exception

    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_lab, <<"1234">>)), % Exception
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_lab, <<"1233">>)),
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_lab, <<"1235">>)),
    ?assertEqual(true, tuenti_config:is_feature_enabled(feature_lab, <<"4242">>)),  % Exception
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_lab, <<"4241">>)),
    ?assertEqual(false, tuenti_config:is_feature_enabled(feature_lab, <<"4243">>)),

    % Calculate rotation index, implementation from tuenti_config_disable
    Percent = 20,
    Seed = 30,

    TestUsers = [ integer_to_binary(rand:uniform(10000000)) || _ <-  lists:seq(1, 20)],

    lists:foreach(fun(UserId) ->
                      % Module "os" can't be mocked with meck
                      {Mega, Seconds, _ } = os:timestamp(),
                      HoursSinceEpoch = ((Mega * 1000000) + Seconds) div 3600,
                      Hour = HoursSinceEpoch rem 24,
                      RotateValue = round((Hour * 10000) / 24),

                      IntegerUser = binary_to_integer(UserId),
                      Index = IntegerUser + RotateValue + Seed,
                      ShouldBeEnabled = (Index rem 10000) < (Percent * 100),
                      ?assertEqual(ShouldBeEnabled, tuenti_config:is_feature_enabled(feature_rotated, UserId))
                   end,
                   TestUsers).


disable_config_changes(_Config) ->
  ?assertEqual(100, tuenti_config:get_feature_percent(feature_enabled)),
  ?assertEqual('LIVE', tuenti_config:get_feature_mode(feature_enabled)),
  ?assertEqual([], tuenti_config:get_feature_uids(feature_enabled)),
  ?assertEqual(lists:sort([<<"4242">>, <<"1234">>]), lists:sort(tuenti_config:get_feature_uids(feature_lab))),
  tuenti_config:set_feature_mode(feature_enabled, 'TESTING'),
  ?assertEqual('TESTING', tuenti_config:get_feature_mode(feature_enabled)),
  tuenti_config:set_feature_percent(feature_enabled, 87),
  ?assertEqual(87, tuenti_config:get_feature_percent(feature_enabled)),
  tuenti_config:set_feature_uids(feature_enabled, [<<"1234">>]),
  ?assertEqual([<<"1234">>], tuenti_config:get_feature_uids(feature_enabled)),
  tuenti_config:add_feature_uids(feature_enabled, [<<"5555">>, <<"4444">>]),
  ?assertEqual(lists:sort([<<"1234">>, <<"5555">>, <<"4444">>]),
               lists:sort(tuenti_config:get_feature_uids(feature_enabled))),
  tuenti_config:remove_feature_uids(feature_enabled, [<<"5555">>, <<"8888">>]),
  ?assertEqual(lists:sort([<<"1234">>, <<"4444">>]),
               lists:sort(tuenti_config:get_feature_uids(feature_enabled))),

  tuenti_config:set_feature_percent(feature_enabled, 65, true, 12),
  ?assertEqual(#{value => 65, rotate => true, seed => 12},
               tuenti_config:get_feature_percent(feature_enabled)),
  ok.


delete_key(_Config) ->
    ?assertMatch(ok, tuenti_config:set(platform, #{
        profiles => #{
            profile1 => #{
                param1 => 1,
                param2 => 2
            }
        }
    })),

    ?assertMatch(#{
        profile1 := #{
            param1 := 1,
            param2 := 2
        }
    }, tuenti_config:get_map([platform, profiles])),

    ok = tuenti_config:delete_key(platform),

    PlatformExpandKey = tuenti_config:expand_key([platform, profiles]),
    ?assertThrow({undefined_config, PlatformExpandKey}, tuenti_config:get_map([platform, profiles])),

    ok.


delete_all(_Config) ->
    ?assertMatch(ok, tuenti_config:set(platform, #{
        profiles => #{
            profile1 => #{
                param1 => 1,
                param2 => 2
            }
        }
    })),

    ?assertMatch(#{
        profile1 := #{
            param1 := 1,
            param2 := 2
        }
    }, tuenti_config:get_map([platform, profiles])),

    ok = tuenti_config:delete_all(),

    PlatformExpandKey = tuenti_config:expand_key(platform),
    ?assertThrow({undefined_config, PlatformExpandKey}, tuenti_config:get_map(platform)),

    ok.


safe_init(_Config) ->
    % repeating the init does not crash on table creation
    tuenti_config_ets:init().


set_notify(_Config) ->
    % changes through :set API trigger notify callbacks
    tuenti_config:set([spam], 3),
    flush_notifications(),
    {ok, _, {found, 3}} = tuenti_config:register_listener(spam, {?MODULE, update_callback, self()}),
    {ok, _, not_found} = tuenti_config:register_listener([spam, bacon], {?MODULE, update_callback, self()}),
    tuenti_config:set([spam], 18),
    tuenti_config:set([spam], 19),
    SpamExpandedKey = tuenti_config:expand_key(spam),
    BaconExpandedKey = tuenti_config:expand_key([spam, bacon]),
    ?assertRecv({changed, SpamExpandedKey, 3, 18}, ?TIMEOUT),
    ?assertRecv({changed, SpamExpandedKey, 18, 19}, ?TIMEOUT),
    tuenti_config:set([spam], #{
        bacon => <<"yes">>,
        spinach => <<"no">>
    }),
    ?assertRecv({changed, SpamExpandedKey, 19, #{bacon := <<"yes">>, spinach := <<"no">>}}, ?TIMEOUT),
    ?assertRecv({added, BaconExpandedKey, undefined, <<"yes">>}, ?TIMEOUT),
    tuenti_config:set([spam], #{
        spinach => <<"no">>
    }),
    ?assertRecv({changed, SpamExpandedKey, #{bacon := <<"yes">>, spinach := <<"no">>}, #{spinach := <<"no">>}}, ?TIMEOUT),
    ?assertRecv({deleted, BaconExpandedKey, <<"yes">>, undefined}, ?TIMEOUT),

    CheeseburgerExpandedKey = tuenti_config:expand_key([cheeseburger]),
    {ok, _, not_found} = tuenti_config:register_listener([cheeseburger], {?MODULE, update_callback, self()}),
    tuenti_config:set([cheeseburger], <<"yummy">>),
    ?assertRecv({added, CheeseburgerExpandedKey, undefined, <<"yummy">>}, ?TIMEOUT),
    tuenti_config:delete_key([cheeseburger]),
    ?assertRecv({deleted, CheeseburgerExpandedKey, <<"yummy">>, undefined}, ?TIMEOUT),
    ok.


failed_type_conversion(_Config) ->
    ?assertError({invalid_config_type_conversion, {_, integer, _}}, tuenti_config:get_integer(list)),
    ?assertError({invalid_config_type_conversion, {_, float, _}}, tuenti_config:get_float(list)),
    ?assertError({invalid_config_type_conversion, {_, boolean, _}}, tuenti_config:get_bool(list)),
    ?assertError({invalid_config_type_conversion, {_, binary, _}}, tuenti_config:get_binary(list)),
    ?assertError({invalid_config_type_conversion, {_, string, _}}, tuenti_config:get_string(null_value)),
    ok.


%%% ===========================================================================
%%% Helper functions
%%% ===========================================================================

dbg() ->
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(tuenti_config, x),
    %dbg:tpl(tuenti_config_yaml_srv, x),
    %dbg:tpl(ets, lookup, x),
    %dbg:tpl(application, get_env, x),
    %dbg:tpl(lookup, config, x),
    %dbg:tpl(tuenti_config, get_raw, x),
    %dbg:tpl(tuenti_config_ets, get_config, x),
    dbg:tpl(tuenti_config_disable, x),
    %dbg:tpl(gb_trees, x),
    %dbg:tpl(gen_server, handle_call, x),
    %dbg:tpl(tuenti_config_srv, x),
    %dbg:tpl(tuenti_config, expand_key, x),
    ok.


store_terms(Lines) ->
    store_terms(?DEFAULT_ROOT, Lines). % default prefix as in .app.src

store_terms(Prefix, Lines) ->
    Config = lines_to_config(Prefix, Lines),
    Directory = mktempdir(),
    write_config(Directory, Config),
    Directory.


mktempdir() ->
    string:strip(os:cmd(?TMPDIRCMD), both, $\n).


write_config(Directory, Config) ->
    ok = file:write_file(filename:join(Directory, ?CONFIG_FILENAME), Config).


write_terms(Directory, Terms) ->
   write_config(Directory, lines_to_config(Terms)).

write_terms(Directory, Prefix, Terms) ->
    write_config(Directory, lines_to_config(Prefix, Terms)).


lines_to_config(Lines) ->
    lines_to_config(?DEFAULT_ROOT, Lines). % default prefix as in .app.src

lines_to_config(Prefix, Lines) ->
    atom_to_list(Prefix) ++ ":\n" ++ lists:reverse(lists:foldl(fun(Term, Acc) -> [io_lib:fwrite("  ~s~n", [Term]) |Acc] end, [], Lines)).

remove_directory(Directory) ->
  os:cmd("rm -rf " ++ Directory).

config_terms() ->
    ["integer: 45",
     "string: \"spam\"",
     "list: [1, 2, 3]",
     "binary: \"bacon\"",
     "atom: false",
     "complex: [[a, [1, toto], foo], [b, [2, \"tata\"], bar]]",
     "null_value: null",
     "map:",
     "  something:",
     "    [a, b, c]",
     "  other: whatever",
     "  submap: {\"a\": 1, \"b\": 2}",
     "map_in_list:",
     "  [{\"a\": 1}, {\"b\": 2, \"c\": 3}]",
     "disable_config:",
     "  feature_disabled:",
     "    mode: DISABLED",
     "    percent: 100",
     "    uids: []",
     "  feature_enabled:",
     "    mode: LIVE",
     "    percent: 100",
     "    uids: []",
     "  feature_10:",
     "    mode: LIVE",
     "    percent:",
     "      value: 10",
     "      rotate: false",
     "    uids: [9876, 1456]",
     "  feature_lab:",
     "    mode: LIVE",
     "    uids: [\"4242\", 1234]",
     "  feature_rotated:",
     "    mode: LIVE",
     "    percent:",
     "      value: 20",
     "      rotate: true",
     "      seed: 30",
     "    uids: [4242, 1234]"].


update_callback({_Op, _Key, _OldValue, _NewValue} = Change, Pid) ->
    Pid ! Change.


non_atom_binary() ->
    non_atom_binary(100).

non_atom_binary(0) ->
    error("Cannot generate binary");
non_atom_binary(N) ->
    RandomBin = list_to_binary([ ($a - 1) + rand:uniform($z - $a + 1) || _ <- lists:seq(1,8) ]),
    try binary_to_existing_atom(RandomBin, utf8) of
        _ -> non_atom_binary(N - 1)
    catch _:_ -> RandomBin
    end.


flush_notifications() ->
    lists:reverse(flush_notifications([])).


flush_notifications(Acc) ->
    receive M -> flush_notifications([M | Acc])
    after 0 -> Acc
    end.
