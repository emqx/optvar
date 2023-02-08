%%%-------------------------------------------------------------------
%% @doc optvar public API
%% @end
%%%-------------------------------------------------------------------

-module(optvar_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  optvar:init(),
  optvar_sup:start_link().

stop(_State) ->
  optvar:stop(),
  ok.

%% internal functions
