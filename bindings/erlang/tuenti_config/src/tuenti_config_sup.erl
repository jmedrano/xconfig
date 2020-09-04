%%% @doc Callback module of tuenti_config application root supervisor behaviour.

-module(tuenti_config_sup).

-behaviour(supervisor).

%%% ===========================================================================
%%% Exports
%%% ===========================================================================

%%% Start/Stop
-export([start_link/0]).

%%% Behaviour's callbacks
-export([init/1]).


%%% ===========================================================================
%%% Macros
%%% ===========================================================================

-define(PROCESS_NAME, ?MODULE).


%%% ===========================================================================
%%% Start/Stop Functions
%%% ===========================================================================

%%% @doc Starts and links a supervisor process.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?PROCESS_NAME}, ?MODULE, []).


%%% ===========================================================================
%%% Behaviour supervisor's Callbacks
%%% ===========================================================================

%% @doc
%% tuenti_config_srv: Creates ETS and manages callback subscriptions
%% tuenti_config_xconfig_srv: Read the configuration from XConfig
%% tuenti_config_yaml_srv: Read the configuration files from the files (only if XConfig is disabled)
init([]) ->
    Children = [tuenti_config_srv |
        case tuenti_config:xconfig_enabled() of
          true ->
                [tuenti_config_xconfig_srv];
            _ ->
                [tuenti_config_yaml_srv]
        end
    ],
    % exconfig tries to connect to xconfigd daemon for 5 seconds before failing
    % So if xconfigd is not available, with 10/20 restart values we make that
    % the supervisor will try to connect continuously to xconfigd forever
    {ok, {{one_for_one, 10, 20}, [tuenti_config_spec(X) || X <- Children]}}.


%%% ===========================================================================
%%% Internal Functions
%%% ===========================================================================

tuenti_config_spec(Module) ->
    {Module, {Module, start_link, []}, permanent, 10, worker, [Module]}.
