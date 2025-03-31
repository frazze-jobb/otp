%%%-------------------------------------------------------------------
%% @doc trace_reader public API
%% @end
%%%-------------------------------------------------------------------

-module(trace_reader_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting trace_reader application...~n"),
    trace_reader_sup:start_link(). % Start the supervisor

stop(_State) ->
    io:format("Stopping trace_reader application.~n"),
    ok.

%% internal functions
