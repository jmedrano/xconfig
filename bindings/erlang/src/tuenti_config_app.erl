%%% @doc tuenti_config application behaviour's callback module.

-module(tuenti_config_app).

-behaviour(application).


%%% ===========================================================================
%%% Exports
%%% ===========================================================================

%%% application behaviour
-export([start/2,
         stop/1]).


%%% ===========================================================================
%%% Behaviour application Callbacks
%%% ===========================================================================

start(normal, _StartArgs) ->
    tuenti_config_sup:start_link().


stop(_State) ->
    ok.
