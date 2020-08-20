%%% @doc Module for disable_config

-module(tuenti_config_disable).

%% API
-export([disable_config_decision/2]).

%% Record definition
-record(dconfig, { mode = 'DISABLED' :: atom(),
                  percent = 0 :: integer(),
                  rotate = false :: boolean(),
                  seed = 0 :: integer(),
                  uids = [] :: list(integer())}).

%%% ===========================================================================
%%% API Functions
%%% ===========================================================================

%% @doc Take the decision to enable of not a feature according to the configuration
-spec disable_config_decision(atom(), list()) -> boolean()
              ;              (atom(), binary()) -> boolean()
              ;              (atom(), integer()) -> boolean().
disable_config_decision(Feature, UserId) when is_binary(UserId) ->
  disable_config_decision(Feature, binary_to_integer(UserId));
disable_config_decision(Feature, UserId) when is_list(UserId) ->
  disable_config_decision(Feature, list_to_integer(UserId));
disable_config_decision(Feature, UserId) when is_atom(Feature),
                                              is_integer(UserId) ->

  Config = get_config_values(Feature),

  case Config#dconfig.mode of
    'DISABLED' ->
      false;

    Mode when Mode =:= 'LIVE' orelse Mode =:= 'TESTING' ->
      case lists:member(UserId, Config#dconfig.uids) of
        true ->
          true;
        false when Config#dconfig.percent =:= 0 ->
          false;
        false when Config#dconfig.percent >= 100 ->
          true;
        _ ->
          % In Testing mode there is no rotation
          RotateValue = if Mode =:= 'LIVE' andalso Config#dconfig.rotate ->
                            {Mega, Seconds, _ } = os:timestamp(),
                            HoursSinceEpoch = ((Mega * 1000000) + Seconds) div 3600,
                            Hour = HoursSinceEpoch rem 24,
                            round((Hour * 10000) / 24);
                         true ->
                              0
                         end,

          Index = Config#dconfig.seed + UserId + RotateValue,
          (Index rem 10000) < (Config#dconfig.percent * 100)
      end;
    _ ->
      false
  end.


%%% ===========================================================================
%%% Internal Functions
%%% ===========================================================================

%% @doc Returns the mode, percentage and list of Uids of the feature configuration
%% If there is no mode, the default value of 'DISABLED' will be used
%% If there is no percentage, it'll return 0
%% If there is no list of Uids, it'll return an empty list
-spec get_config_values(atom()) -> #dconfig{}.
get_config_values(Feature) ->
    case tuenti_config_ets:disable_config_get_cached_config_values(Feature) of
        {found, ConfigValues} ->
            ConfigValues;
        not_found ->
            calculate_config_values(Feature)
    end.

-spec calculate_config_values(atom()) -> #dconfig{}.
calculate_config_values(Feature) ->
    ConfigVersion = tuenti_config_ets:get_current_config_version(),
    Parameters = tuenti_config:get_map([disable_config, Feature]),

    Mode = case maps:get(mode, Parameters, undefined) of
        <<"DISABLED">> -> 'DISABLED';
        <<"LIVE">> -> 'LIVE';
        <<"TESTING">> -> 'TESTING';
        _ -> 'DISABLED'
    end,

    {Percent, Rotate, Seed} = get_percent_values(Parameters),

    Uids = case maps:get(uids, Parameters, undefined) of
                List when is_list(List) ->
                    % Convert all Uids to integers
                    lists:map(fun
                                (UserId) when is_binary(UserId) ->
                                    binary_to_integer(UserId);
                                (UserId) when is_list(UserId) ->
                                    list_to_integer(UserId);
                                (UserId) when is_integer(UserId) ->
                                    UserId
                              end,
                              List);
                _ ->
                    []
            end,

    Config = #dconfig{mode = Mode, percent = Percent, rotate = Rotate, seed = Seed, uids = Uids},
    tuenti_config_ets:disable_config_set_cached_config_values(ConfigVersion, Feature, Config),
    Config.


%% @doc Returns the tuple {Percent, ShouldRotate, Seed}
%% Provide conservative default values if there is any error in the configuration
-spec get_percent_values(number() | list() | term()) -> { number(), boolean(), number()}.
get_percent_values(#{percent := Value}) when is_number(Value) ->
    { Value, false, 0};
get_percent_values(#{percent := Parameters}) when is_map(Parameters) ->
    Percent = case maps:get(value, Parameters, undefined) of
                  Num when is_integer(Num) andalso Num >= 0 -> Num;
                  _ -> 0
              end,
    Rotate = case maps:get(rotate, Parameters, undefined) of
                 Value when is_boolean(Value) -> Value;
                 _ -> false
             end,
    Seed = case maps:get(seed, Parameters, undefined) of
               SeedNum when is_integer(SeedNum) -> SeedNum;
               _ -> 0
           end,
    {Percent, Rotate, Seed };
get_percent_values(_) ->
    {0, false, 0}.
