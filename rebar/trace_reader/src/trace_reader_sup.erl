%%%-------------------------------------------------------------------
%% @doc trace_reader top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(trace_reader_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
        #{id => trace_reader_server, % Child ID
          start => {trace_reader_server, start_link, []}, % How to start the child
          restart => permanent, % Restart policy
          shutdown => 5000, % Shutdown timeout
          type => worker, % Child type
          modules => [trace_reader_server] % Modules associated with the child
         }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
