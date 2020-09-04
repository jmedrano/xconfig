%%% Unit test for the exconfig port
-module(tuenti_xconfig_SUITE).

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
-define(TMPDIRCMD, "mktemp -d 2>/dev/null || mktemp -d -t 'mytmp'").
-define(XCONFIG_WAIT_TIME, 1000).

%%% ===========================================================================
%%% common_test callbacks
%%% ===========================================================================

linux_tests() ->
    [
     empty_config,
     simple_config,
     complex_values,
     override,
     notifications,
     xconfigd_stopped
    ].

all() ->
    OnlyLinux = linux_tests(),
    case os:type() of
      {unix, linux} ->
          OnlyLinux;
      _ ->
          []
    end.

suite() -> [{timetrap, {seconds, 60}}].

init_per_suite(Conf) ->
  TmpDir = mktempdir(),
  XmlFileDst = filename:join([TmpDir, "log4cxx.xml"]),
  % Try to find the test directory using 2 paths
  PrivDirListTmp = filename:split(code:priv_dir(tuenti_config)),
  PrivDir = filename:join(lists:sublist(PrivDirListTmp, length(PrivDirListTmp) - 1) ++ ["test"]),
  XmlFileSrc1 = filename:join([PrivDir, "log4cxx.xml"]),
  case file:copy(XmlFileSrc1, XmlFileDst) of
    {ok, _} ->
        ok;
    {error, _} ->
        DataDirList = filename:split(proplists:get_value(data_dir, Conf)),
        TestDir = filename:join(lists:sublist(DataDirList, length(DataDirList) - 1)),
        XmlFileSrc2 = filename:join([TestDir, "log4cxx.xml"]),
        {ok, _} = file:copy(XmlFileSrc2, XmlFileDst)
  end,
  {IniPath, SocketPath} = generate_xconfigd_ini(TmpDir),

  [
    {xconfigd_dir, TmpDir},
    {xconfigd_socket_path, SocketPath},
    {xconfigd_xml_file, XmlFileDst},
    {xconfigd_ini_file, IniPath}
   | Conf].


end_per_suite(Conf) ->
  ok = file:delete(proplists:get_value(xconfigd_socket_path, Conf)),
  ok = file:delete(proplists:get_value(xconfigd_xml_file, Conf)),
  ok = file:delete(proplists:get_value(xconfigd_ini_file, Conf)),
  ok = file:del_dir(proplists:get_value(xconfigd_dir, Conf)),
  ok.


init_per_testcase(_Name, Conf) ->
  Port = start_xconfigd(proplists:get_value(xconfigd_dir, Conf), proplists:get_value(xconfigd_socket_path, Conf)),
  YamlDir = mktempdir(),
  YamlOverrideDir = filename:join([YamlDir, "override"]),
  ok = file:make_dir(YamlOverrideDir),
  application:load(tuenti_config),
  ok = meck:new(tuenti_config_xconfig_srv, [passthrough]),
  ok = meck:new(tuenti_config_ets, [passthrough]),
  application:set_env(tuenti_config, xconfig_enabled, true),
  application:set_env(tuenti_config, exconfig_quiet, true),
  application:set_env(tuenti_config, xconfig_socket_path, proplists:get_value(xconfigd_socket_path, Conf)),
  application:set_env(tuenti_config, xconfig_config_path, YamlDir ++ ":" ++ YamlOverrideDir),
  {ok, _} = application:ensure_all_started(tuenti_config),
  [{xconfigd_port, Port}, {yaml_dir, YamlDir}, {yaml_override_dir, YamlOverrideDir} | Conf].

end_per_testcase(_, Conf) ->
  ok = application:stop(tuenti_config),
  application:unload(tuenti_config),
  ok = meck:unload(tuenti_config_xconfig_srv),
  ok = meck:unload(tuenti_config_ets),
  application:unset_env(tuenti_config, xconfig_enabled),
  application:unset_env(tuenti_config, xconfig_socket_path),
  application:unset_env(tuenti_config, xconfig_config_path),
  Port = proplists:get_value(xconfigd_port, Conf),
  stop_xconfigd(Port),
  del_dir_not_empty(proplists:get_value(yaml_override_dir, Conf)),
  del_dir_not_empty(proplists:get_value(yaml_dir, Conf)),
  ok.


%%% ===========================================================================
%%% Tests
%%% ===========================================================================

empty_config(_Config) ->
  WhateverExpandedKey = tuenti_config:expand_key(whatever),
  ?assertThrow({undefined_config, WhateverExpandedKey}, tuenti_config:get_raw(whatever)),
  ok.

simple_config(Config) ->
  ConfigFile = filename:join([proplists:get_value(yaml_dir, Config), "test.yaml"]),
  write_config_and_wait(ConfigFile, "whatever: 42"),
  ?assertEqual(42, tuenti_config:global_get_integer([whatever])),
  write_config_and_wait(ConfigFile, [["something: \"hello world\"\n"],
                                     ["whatever: 64\n"],
                                     ["testing: true\n"],
                                     ["number: 5.43\n"],
                                     ["null_value: null\n"],
                                     ["lst: [a, b, c, 4 ]\n"]]),
  ?assertEqual(<<"hello world">>, tuenti_config:global_get_binary([something])),
  ?assertEqual("hello world", tuenti_config:global_get_string([something])),
  ?assertEqual(64, tuenti_config:global_get_integer([whatever])),
  ?assertEqual(true, tuenti_config:global_get_bool([testing])),
  ?assertEqual(5.43, tuenti_config:global_get_float([number])),
  ?assertEqual(null, tuenti_config:global_get_raw([null_value])),
  ?assertEqual([<<"a">>, <<"b">>, <<"c">>, 4], tuenti_config:global_get_raw([lst])),
  write_config_and_wait(ConfigFile, [["something: \"hello world\"\n"],
                                     ["testing: false\n"]]),
  ?assertThrow({undefined_config, [whatever]}, tuenti_config:global_get_raw([whatever])),
  ?assertThrow({undefined_config, [lst]}, tuenti_config:global_get_raw([lst])),
  ?assertEqual(false, tuenti_config:global_get_bool([testing])),
  ok.

complex_values(Config) ->
  ConfigFile = filename:join([proplists:get_value(yaml_dir, Config), "test.yaml"]),
  write_config_and_wait(ConfigFile, [["myboolean_atom: true\n"],
                                     ["otherboolean: false\n"],
                                     ["IDontKnow: undefined\n"],
                                     ["onelist: [ 1, a, b, [c, d], 5]\n"],
                                     ["one_binary: \"This is binary\"\n"],
                                     ["\"{ key_with_special_characters } []\": Hello Complex\n"],
                                     ["another_complex: [ one, two, true, [inside_one, [inception], inside_three], [\"hello\", World]]\n"],
                                     ["a_map: {a: 1, b: 2, submap: {\"hello\": \"world\"}}\n"]]),
  ?assertEqual(true, tuenti_config:global_get_bool([myboolean_atom])),
  ?assertEqual(false, tuenti_config:global_get_bool([otherboolean])),
  ?assertEqual("undefined", tuenti_config:global_get_string(['IDontKnow'])),
  ?assertEqual([1, <<"a">>, <<"b">>, [<<"c">>, <<"d">>], 5], tuenti_config:global_get_raw([onelist])),
  ?assertEqual(<<"This is binary">>, tuenti_config:global_get_binary([one_binary])),
  ?assertEqual("Hello Complex", tuenti_config:global_get_string(['{ key_with_special_characters } []'])),
  ?assertEqual([<<"one">>, <<"two">>, true, [<<"inside_one">>, [<<"inception">>],<<"inside_three">>], [<<"hello">>,<<"World">>]], tuenti_config:global_get_raw([another_complex])),
  ?assertEqual(#{a => 1, b => 2, submap => #{hello => <<"world">>}}, tuenti_config:global_get_raw(a_map)),
  ok.

override(Config) ->
  ConfigFile1 = filename:join([proplists:get_value(yaml_dir, Config), "test1.yaml"]),
  write_config_and_wait(ConfigFile1, [["whatever: 42\n"],
                                      ["fruits: [\"apple\", orange]\n"]]),

  ?assertEqual(42, tuenti_config:global_get_integer([whatever])),
  ?assertEqual([<<"apple">>, <<"orange">>], tuenti_config:global_get_raw([fruits])),
  ConfigFile2 = filename:join([proplists:get_value(yaml_override_dir, Config), "test2.yaml"]),
  write_config_and_wait(ConfigFile2, [["fruits: [\"pear\", pineapple]\n"],
                                      ["newvar: hello\n"]]),

  ?assertEqual(42, tuenti_config:global_get_integer([whatever])),
  ?assertEqual([<<"pear">>, <<"pineapple">>], tuenti_config:global_get_raw([fruits])),
  ?assertEqual("hello", tuenti_config:global_get_string([newvar])),
  file:delete(ConfigFile1),
  timer:sleep(?XCONFIG_WAIT_TIME),
  ?assertThrow({undefined_config, [whatever]}, tuenti_config:global_get_integer([whatever])),
  ?assertThrow({undefined_config, [whatever]}, tuenti_config:global_get_raw([whatever])),
  ?assertEqual([<<"pear">>, <<"pineapple">>], tuenti_config:global_get_raw([fruits])),
  ?assertEqual("hello", tuenti_config:global_get_string([newvar])),

  write_config_and_wait(ConfigFile1, [["whatever: undefined\n"],
                                      ["fruits: [lemon, watermelon]\n"],
                                      ["nested:\n"],
                                      ["  first: 1\n"],
                                      ["  second: 2\n"]]),

  ?assertEqual(<<"undefined">>, tuenti_config:global_get_binary([whatever])),
  ?assertEqual([<<"pear">>, <<"pineapple">>], tuenti_config:global_get_raw([fruits])),
  ?assertEqual("hello", tuenti_config:global_get_string([newvar])),

  file:delete(ConfigFile2),
  timer:sleep(?XCONFIG_WAIT_TIME),
  ?assertEqual([<<"lemon">>, <<"watermelon">>], tuenti_config:global_get_raw([fruits])),

  write_config_and_wait(ConfigFile2, "newvar: \"the newest one\"\n"),
  ?assertEqual("the newest one", tuenti_config:global_get_string([newvar])),

  % Test map override
  ?assertEqual(1, tuenti_config:global_get_raw([nested, first])),
  ?assertEqual(2, tuenti_config:global_get_raw([nested, second])),
  write_config_and_wait(ConfigFile2, [["nested:\n"],
                                      ["  third: 3\n"]]),

  ?assertEqual(1, tuenti_config:global_get_raw([nested, first])),
  ?assertEqual(2, tuenti_config:global_get_raw([nested, second])),
  ?assertEqual(3, tuenti_config:global_get_raw([nested, third])),

  write_config_and_wait(ConfigFile2, [["nested: !mapoverride\n"],
                                      ["  fourth: 4\n"]]),

  ?assertThrow({undefined_config, [nested, first]}, tuenti_config:global_get_raw([nested, first])),
  ?assertThrow({undefined_config, [nested, second]}, tuenti_config:global_get_raw([nested, second])),
  ?assertThrow({undefined_config, [nested, third]}, tuenti_config:global_get_raw([nested, third])),
  ?assertEqual(4, tuenti_config:global_get_raw([nested, fourth])),
  ok.


notifications(Config) ->
  ConfigFile = filename:join([proplists:get_value(yaml_dir, Config), "test.yaml"]),
  write_config_and_wait(ConfigFile, [["spam: 3\n"]]),
  flush_notifications(),
  {ok, _, {found, 3}} = tuenti_config:global_register_listener([spam], {?MODULE, update_callback, self()}),
  {ok, _, not_found} = tuenti_config:global_register_listener([bacon], {?MODULE, update_callback, self()}),
  write_config_and_wait(ConfigFile, [["spam: 18\n"]]),
  ?assertRecv({changed, [spam], 3, 18}, ?TIMEOUT),
  write_config_and_wait(ConfigFile, [["bacon: 32\n"]]),
  ?assertRecv({deleted, [spam], 18, undefined}, ?TIMEOUT),
  ?assertRecv({added, [bacon], undefined, 32}, ?TIMEOUT),
  write_config_and_wait(ConfigFile, [["bacon: 32\n"],
                                       ["spam: 33\n"]]),
  ?assertRecv({added, [spam], undefined, 33}, ?TIMEOUT),
  write_config_and_wait(ConfigFile, [["spam: 33\n"]]),
  ?assertRecv({deleted, [bacon], 32, undefined}, ?TIMEOUT),
  ok.


xconfigd_stopped(Config) ->
  ConfigFile = filename:join([proplists:get_value(yaml_dir, Config), "test.yaml"]),

  write_config_and_wait(ConfigFile, "whatever: 42"),
  ?assertEqual(42, tuenti_config:global_get_integer([whatever])),

  Port = proplists:get_value(xconfigd_port, Config),
  stop_xconfigd(Port),
  ok = meck:wait(tuenti_config_xconfig_srv, terminate, [xconfig_closed, '_'], 10000),

  % The config is still available
  ?assertEqual(42, tuenti_config:global_get_integer([whatever])),

  % But it will contain old values (XConfigd is not running)
  write_config_and_wait(ConfigFile, "whatever: 58"),
  ?assertEqual(42, tuenti_config:global_get_integer([whatever])),

  % Should be working even after a very long time without xconfigd
  % We've seen restarting times up to 30 seconds
  timer:sleep(30 * 1000),
  ?assertEqual(42, tuenti_config:global_get_integer([whatever])),

  meck:reset(tuenti_config_ets),
  % Start xconfigd again
  NewPort = start_xconfigd(proplists:get_value(xconfigd_dir, Config), proplists:get_value(xconfigd_socket_path, Config)),
  ok = meck:wait(tuenti_config_ets, set_full_config, '_', 5000),

  % Now we are able to read the updated value
  ?assertEqual(58, tuenti_config:global_get_integer([whatever])),
  stop_xconfigd(NewPort),

  ok.


%%% ===========================================================================
%%% Helper functions
%%% ===========================================================================

dbg() ->
  dbg:tracer(),
  dbg:p(all, c),
  dbg:tpl(tuenti_config_xconfig_srv, x),
  dbg:tpl(tuenti_config_srv, x),
  ok.


mktempdir() ->
  string:strip(os:cmd(?TMPDIRCMD), both, $\n).

del_dir_not_empty(Path) ->
  {ok, Filenames} = file:list_dir(Path),
  [ file:delete(filename:join([Path, X])) || X <- Filenames ],
  ok = file:del_dir(Path).

generate_xconfigd_ini(Path) ->
  IniPath = filename:join([Path, "xconfigd.ini"]),
  SocketPath = filename:join([Path, "xconfig.socket"]),
  Content = ["[General]\nServerSocketPath=", SocketPath, "\n"],
  ok = file:write_file(IniPath, Content),
  {IniPath, SocketPath}.


start_xconfigd(XConfigdConfPath, SocketPath) ->
  Port = open_port({spawn, "xconfigd -c " ++ XConfigdConfPath ++ " 2> /dev/null"}, [stream, exit_status]),
  % Check if the xconfigd exist and is successfully started
  receive
    {Port, {exit_status, _}} ->
      throw(error_starting_xconfigd)
  after 0 ->
    ok
  end,
  % Wait until the file socket is created
  ok = wait_for_file_socket(SocketPath, 5),
  Port.

wait_for_file_socket(SocketFile, Timeout) ->
  wait_for_file_socket_count(SocketFile, Timeout * 2).

wait_for_file_socket_count(SocketFile, Count) ->
  case file:read_file_info(SocketFile) of
    {ok, #file_info{ size = 0, type = other} } ->
      ok;
    { ok, _ } ->
      {error, incorrect_file};
    {error, enoent} when Count > 0 ->
      timer:sleep(500),
      wait_for_file_socket(SocketFile, Count - 1);
    {error, Reason} ->
      {error, Reason}
  end.


stop_xconfigd(Port) ->
  case erlang:port_info(Port, os_pid) of 
    undefined -> % closed already
      ok;
    {os_pid, Pid} ->
      os:cmd("kill " ++ integer_to_list(Pid)),
      catch port_close(Port)
  end.

write_config_and_wait(Filename, Content) ->
  file:write_file(Filename, Content),

  % Wait for xconfigd to notice the new configuration
  timer:sleep(?XCONFIG_WAIT_TIME).


update_callback({_Op, _Key, _OldValue, _NewValue} = Change, Pid) ->
  Pid ! Change.


flush_notifications() ->
  receive
    {_Op, _Key, _Value} -> flush_notifications()
  after 0 -> ok
  end.
