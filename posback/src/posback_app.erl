%%%-------------------------------------------------------------------
%% @doc posback public API
%% @end
%%%-------------------------------------------------------------------

-module(posback_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    posback_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
