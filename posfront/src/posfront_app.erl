%%%-------------------------------------------------------------------
%% @doc posfront public API
%% @end
%%%-------------------------------------------------------------------

-module(posfront_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    posfront_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
