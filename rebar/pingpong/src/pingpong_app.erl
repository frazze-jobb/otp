%%%-------------------------------------------------------------------
%% @doc pingpong public API
%% @end
%%%-------------------------------------------------------------------

-module(pingpong_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pingpong_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
