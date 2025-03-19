%%%-------------------------------------------------------------------
%% @doc cluedo public API
%% @end
%%%-------------------------------------------------------------------

-module(cluedo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cluedo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
