-module(tuenti_config_yamerl_node_helper).

%% This module is only to allow YAMERL parser to ignore the !mapoverride tag

-include_lib("yamerl/include/yamerl_nodes.hrl").

%% Public API.
-export([
    tags/0,
    try_construct_token/3,
    construct_token/3,
    node_pres/1
  ]).

-define(TAG_OVERRIDE, "!mapoverride").

tags() ->
  [?TAG_OVERRIDE].

try_construct_token(Constr, Node, Token) ->
  yamerl_node_map:try_construct_token(Constr, Node, Token).

construct_token(Constr, Node, Token) ->
  yamerl_node_map:construct_token(Constr, Node, Token).

node_pres(Node) ->
  yamerl_node_map:node_pres(Node).

