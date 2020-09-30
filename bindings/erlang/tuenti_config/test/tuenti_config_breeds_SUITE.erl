%%% Unit test for the tuenti_config module, separated for breeds
-module(tuenti_config_breeds_SUITE).

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
-define(TMPDIRCMD, "mktemp -d 2>/dev/null || mktemp -d -t 'mytmp'").

%% MUST BE THE SAME AS DEFAULT VALUE IN .APP.SRC
-define(DEFAULT_ROOT, defaultRoot).
-define(DEFAULT_ROOT_BREEDS, defaultRoot_breeds).

%%% ===========================================================================
%%% common_test callbacks
%%% ===========================================================================

all() ->
    [
     get_no_breeds,
     get_one_breed,
     get_two_breeds,
     register_with_breeds,
     version_mismatch
    ].

suite() -> [{timetrap, {seconds, 10}}].


init_per_testcase(version_mismatch, Conf) ->
    ok = meck:new(tuenti_config_ets, [passthrough]),
    TestDriver = self(),
    WaitToContinue = fun(A, B) ->
                             ContinueRef = make_ref(),
                             TestDriver ! {wait_to_continue, self(), ContinueRef},
                             receive ContinueRef -> ok end,
                             meck:passthrough([A, B])
                     end,
    meck:expect(tuenti_config_ets, get_from_full_config, WaitToContinue),
    init_per_testcase(undefined, Conf);
init_per_testcase(_TestCase, Conf) ->
    ConfDir = store_terms(),
    application:load(tuenti_config),
    application:set_env(tuenti_config, xconfig_enabled, false),
    application:set_env(tuenti_config, xconfig_config_path, ConfDir),
    {ok, _} = application:ensure_all_started(tuenti_config),
    tuenti_config:flush_cache(),
    [{conf_dir, ConfDir} |Conf].


end_per_testcase(version_mismatch, Conf) ->
    ok = meck:unload(tuenti_config_ets),
    end_per_testcase(undefined, Conf);
end_per_testcase(_, Config) ->
    application:stop(tuenti_config),
    application:unload(tuenti_config),
    application:unset_env(tuenti_config, xconfig_enabled),
    remove_directory(proplists:get_value(conf_dir, Config)),
    case flush_notifications() of
        [] -> ok;
        Messages -> ct:pal("Messages still in inbox: ~p", [Messages])
    end,
    ok.


%%% ===========================================================================
%%% Tests
%%% ===========================================================================

get_no_breeds(_Config) ->
    VeggieExpandedKey = tuenti_config:expand_key(vegetable),

    ?assertMatch(#{other := <<"whatever">>}, get_breeded([map], [])),
    ?assertMatch(#{map := #{other := <<"whatever">>}}, get_breeded([], [])),
    ?assertThrow({undefined_config, VeggieExpandedKey}, get_breeded([vegetable], [])),

    ok.


get_one_breed(_Config) ->
    VeggieExpandedKey = tuenti_config:expand_key(vegetable),

    OneBreedES = [{key_country, 'ES'}],
    ?assertMatch(#{other := 1, inner_country := <<"ES">>}, get_breeded([map], OneBreedES)),
    ?assertMatch(#{map := #{other := 1, inner_country := <<"ES">>}, country := <<"ES">>}, get_breeded([], OneBreedES)),
    ?assertMatch(<<"ES">>, get_breeded([country], OneBreedES)),
    ?assertThrow({undefined_config, VeggieExpandedKey}, get_breeded([vegetable], OneBreedES)),
    OneBreedJP = [{key_country, 'JP'}],
    ?assertMatch(#{other := <<"whatever">>, inner_country := <<"JP">>}, get_breeded([map], OneBreedJP)),
    ?assertMatch(#{map := #{other := <<"whatever">>, inner_country := <<"JP">>}, country := <<"JP">>}, get_breeded([], OneBreedJP)),
    ?assertMatch(<<"JP">>, get_breeded([country], OneBreedJP)),
    ?assertThrow({undefined_config, VeggieExpandedKey}, get_breeded([vegetable], OneBreedJP)),

    % The breed is missing
    OneBreedFR = [{key_country, 'FR'}],
    ?assertMatch(#{other := <<"whatever">>}, get_breeded([map], OneBreedFR)),
    ?assertMatch(#{map := #{other := <<"whatever">>}}, get_breeded([], OneBreedFR)),
    ?assertThrow({undefined_config, VeggieExpandedKey}, get_breeded([vegetable], OneBreedFR)),

    ok.


get_two_breeds(_Config) ->
    TwoBreedsResult1 = get_breeded([], [{key_country, 'ES'}, {key_vegetable, potato}]),
    ?assertMatch(#{map := #{other := 3, inner_country := <<"ES">>, inner_vegetable := <<"yummy_potato">>}}, TwoBreedsResult1),
    TwoBreedsResult2 = get_breeded([], [{key_vegetable, potato}, {key_country, 'JP'}]),
    ?assertMatch(#{map := #{other := 3, inner_country := <<"JP">>, inner_vegetable := <<"yummy_potato">>}}, TwoBreedsResult2),

    % Highest priority breed (last) clears
    TwoBreedsResult3 = get_breeded([], [{key_country, 'ES'}, {key_cleanup, empty}]), % Empty map gets merged
    TwoBreedsResult4 = get_breeded([], [{key_country, 'JP'}, {key_cleanup, inner_empty}]), % Empty map gets merged
    ?assertMatch(#{map := #{other := 1, inner_country := <<"ES">>}, country := <<"ES">>}, TwoBreedsResult3),
    ?assertMatch(#{map := #{other := <<"whatever">>, inner_country := <<"JP">>}, country := <<"JP">>}, TwoBreedsResult4),
    TwoBreedsResult5 = get_breeded([], [{key_country, 'ES'}, {key_cleanup, null}]), % null clears
    ?assertMatch(null, TwoBreedsResult5),
    TwoBreedsResult6 = get_breeded([], [{key_country, 'ES'}, {key_cleanup, overwrite1}]),
    ?assertMatch(<<"overwritten">>, TwoBreedsResult6),
    TwoBreedsResult7 = get_breeded([], [{key_country, 'ES'}, {key_cleanup, overwrite2}]),
    ?assertMatch(#{map := <<"overwritten">>}, TwoBreedsResult7),

    % Lowest priority breed (first) clears, so the non-breed values are hidden
    CleanInnerMap = #{inner_country => <<"JP">>}, % No other!
    TwoBreedsResult8 = get_breeded([], [{key_cleanup, null}, {key_country, 'JP'}]),
    ?assertMatch(#{map := CleanInnerMap, country := <<"JP">>}, TwoBreedsResult8),
    TwoBreedsResult9 = get_breeded([], [{key_cleanup, overwrite1}, {key_country, 'JP'}]),
    ?assertMatch(#{map := CleanInnerMap, country := <<"JP">>}, TwoBreedsResult9),
    TwoBreedsResult10 = get_breeded([], [{key_cleanup, overwrite2}, {key_country, 'JP'}]),
    ?assertMatch(#{map := #{inner_country := <<"JP">>, other := <<"whatever">>}, country := <<"JP">>}, TwoBreedsResult10),
    % ^ This is a mismatch between xconfig merge algorithm and breeds merge
    % algorithm.
    % Although the design was for both algorithms to have the same result, the
    % proper xconfig merge would instead yield
    %
    %   ?assertMatch(#{map := CleanInnerMap, country := <<"JP">>}, TwoBreedsResult10),
    %
    % ... But the at the moment of integrating Erlang with breeds, other languages
    % have the breeds merge behaviour already. It's been decided that Erlang should
    % rather be consistent with the other languages than match the xconfig merge
    % behaviour exactly.
    %
    % The issue has its roots in the fact that in order to improve efficiency, the
    % breeds list is traversed in inverse order, from highest priority to lowest
    % priority, merging when required and with an early return if it's not
    % possible to merge anything else.
    % The problem triggers with a map with an overwritten subkey, because with
    % the breeds merge algorithm it's not recorded anywhere that some subkey was
    % overwritten. XConfig merge would overwrite the key and that's it, but
    % breeds merge doesn't, just ignores it.

    % One of the two breeds is missing
    TwoBreedsResult11 = get_breeded([], [{key_country, 'ES'}, {key_cleanup, doesnt_exist}]),
    ?assertMatch(#{map := #{other := 1, inner_country := <<"ES">>}}, TwoBreedsResult11),
    TwoBreedsResult12 = get_breeded([], [{key_cleanup, doesnt_exist}, {key_country, 'ES'}]),
    ?assertMatch(#{map := #{other := 1, inner_country := <<"ES">>}}, TwoBreedsResult12),

    ok.


register_with_breeds(Config) ->
    Key = tuenti_config:expand_key(map),
    ConfigDir = proplists:get_value(conf_dir, Config),
    Callback = {?MODULE, update_callback, self()},

    % Original values
    ?assertMatch({ok, _, {found, #{other := <<"whatever">>}}, _}, tuenti_config_srv:register_listener(Key, Callback)),
    ?assertMatch({ok, _, {found, #{other := 1}}, _}, tuenti_config_srv:register_listener(Key, [{key_country, 'ES'}], Callback)),
    ?assertMatch({ok, _, {found, #{other := <<"whatever">>}}, _}, tuenti_config_srv:register_listener(Key, [{key_country, 'JP'}], Callback)),
    ?assertMatch({ok, _, {found, #{other := <<"whatever">>}}, _}, tuenti_config_srv:register_listener(Key, [{key_country, 'FR'}], Callback)),

    % A change in the base that is not reflected in the breeded config does not
    % send a notification
    lines_to_config(ConfigDir, ?DEFAULT_ROOT, ["map:", "  other: 4"]),
    tuenti_config:reload(),
    % Base, JP and FR
    ?assertRecv({changed, Key, #{other := <<"whatever">>}, #{other := 4}}, ?TIMEOUT),
    ?assertRecv({changed, Key, #{other := <<"whatever">>}, #{other := 4}}, ?TIMEOUT),
    ?assertRecv({changed, Key, #{other := <<"whatever">>}, #{other := 4}}, ?TIMEOUT),

    % A change in the breed only affects the breed
    NewBreeds = [
                 "key_country:",
                 "  ES:",
                 "    map:",
                 "      inner_country: ES",
                 "      other: 1",
                 "  JP:",
                 "    map:",
                 "      other: 5",
                 "  FR:",
                 "    map:",
                 "      other: 6"
                ],
    lines_to_config(ConfigDir, ?DEFAULT_ROOT_BREEDS, NewBreeds),
    tuenti_config:reload(),
    ?assertRecv({changed, Key, #{other := 4}, #{other := 5}}, ?TIMEOUT), % JP
    ?assertRecv({changed, Key, #{other := 4}, #{other := 6}}, ?TIMEOUT), % FR

    % No other updates are left
    ?assertNotRecvAnything(?TIMEOUT),
    ok.


version_mismatch(Config) ->
    process_flag(trap_exit, true),
    Worker = spawn_link(fun() -> exit(get_breeded([map], [{key_country, 'ES'}])) end),

    ContinueRef1 = receive {wait_to_continue, Worker, Ref1} -> Ref1
                  after ?TIMEOUT -> ct:fail(missing_message)
                  end,
    Worker ! ContinueRef1,
    ContinueRef2 = receive {wait_to_continue, Worker, Ref2} -> Ref2
                  after ?TIMEOUT -> ct:fail(missing_message)
                  end,
    % Here, Worker has calculated the 'ES' breed for map, and it's currently
    % waiting to fetch the base config to merge. Let's change the
    % base just before the worker obtains it

    ConfigDir = proplists:get_value(conf_dir, Config),
    lines_to_config(ConfigDir, ?DEFAULT_ROOT, ["map:", "  random: 42"]),
    tuenti_config:reload(),

    % Have the worker continue
    Worker ! ContinueRef2,

    % Worker detects that there's been a config change in the middle, so it
    % needs to recalculate the whole merge again.
    ContinueRef3 = receive {wait_to_continue, Worker, Ref3} -> Ref3
                  after ?TIMEOUT -> ct:fail(missing_message)
                  end,
    Worker ! ContinueRef3,
    ContinueRef4 = receive {wait_to_continue, Worker, Ref4} -> Ref4
                  after ?TIMEOUT -> ct:fail(missing_message)
                  end,
    Worker ! ContinueRef4,

    % And Worker continues and exits with the value:
    ?assertRecv({'EXIT', Worker, #{inner_country := <<"ES">>, other := 1, random := 42}}, ?TIMEOUT),

    % No other updates are left
    ?assertNotRecvAnything(?TIMEOUT),
    process_flag(trap_exit, false),
    ok.


%%% ===========================================================================
%%% Helper functions
%%% ===========================================================================

update_callback({_Op, _Key, _OldValue, _NewValue} = Change, Pid) ->
    Pid ! Change.

get_breeded(Key, Breeds) ->
    tuenti_config:global_get_raw_with_breeds([?DEFAULT_ROOT] ++ Key, Breeds).

store_terms() ->
    Directory = string:strip(os:cmd(?TMPDIRCMD), both, $\n),
    lines_to_config(Directory, ?DEFAULT_ROOT, config_terms()),
    lines_to_config(Directory, ?DEFAULT_ROOT_BREEDS, breed_terms()),
    Directory.

lines_to_config(Directory, Prefix, Lines) ->
    PrefixString = atom_to_list(Prefix),
    Config = PrefixString ++ ":\n" ++ lists:reverse(lists:foldl(fun(Term, Acc) -> [io_lib:fwrite("  ~s~n", [Term]) |Acc] end, [], Lines)),
    ok = file:write_file(filename:join(Directory, PrefixString ++ ".yaml"), Config).

remove_directory(Directory) ->
    os:cmd("rm -rf " ++ Directory).

config_terms() ->
    [
     "map:",
     "  other: whatever"
    ].

breed_terms() ->
    [
     "key_country:",
     "  ES:",
     "    country: ES",
     "    map:",
     "      inner_country: ES",
     "      other: 1",
     "  JP:",
     "    country: JP",
     "    map:",
     "      inner_country: JP",
     "key_cleanup:",
     "  empty: {}",
     "  inner_empty:",
     "    map: {}",
     "  'null': null # same as having no value at all",
     "  overwrite1: overwritten",
     "  overwrite2:",
     "    map: overwritten",
     "key_vegetable:",
     "  potato:",
     "    vegetable: yummy_potato",
     "    map:",
     "      inner_vegetable: yummy_potato",
     "      other: 3"
    ].

flush_notifications() ->
    lists:reverse(flush_notifications([])).

flush_notifications(Acc) ->
    receive M -> flush_notifications([M | Acc])
    after 0 -> Acc
    end.
