%% the eunit.hrl must be included before this file


%% NOTE: this is the same content as
%% chat/test_lib/test_util/include/tu_eunit_macros.hrl
%% if there comes a day we have a tuenti_common lib
%% they can probably both read from it

%% Assert that Msg is received within Timeout millseconds
-define(assertRecv(Msg, Timeout),
        ?assertEqual(ok, receive
                              Msg ->
                                  ok
                          after Timeout ->
                                  throw({timeout, 
                                         [{assertRecv, ??Msg},
                                          {queue, erlang:process_info(self(), messages)}]})
                          end)).


%% Assert that Msg is NOT received within Timeout milliseconds
-define(assertNotRecv(Msg, Timeout),
        ?assertThrow(timeout,
                     receive
                         Msg ->
                             Msg
                     after Timeout ->
                             throw(timeout)
                     end)).


%% Assert that whatever msg is NOT received within Timeout milliseconds
-define(assertNotRecvAnything(Timeout),
        ?assertThrow(timeout,
                     receive
                        _ ->
                             true
                      after Timeout ->
                              throw(timeout)
                      end)).

%% Variant of assertMatch macro that returns result of expression
-ifdef(NOASSERT).
-define(assertMatchR(Guard,Expr), Expr).
-else.
-define(assertMatchR(Guard, Expr),
	((fun () ->
        Result = (Expr),
	    case Result of
		Guard -> Result;
		__V -> .erlang:error({assertMatch_failed,
				      [{module, ?MODULE},
				       {line, ?LINE},
				       {expression, (??Expr)},
				       {expected, (??Guard)},
				       {value, __V}]})
	    end
	  end)())).
-endif.
