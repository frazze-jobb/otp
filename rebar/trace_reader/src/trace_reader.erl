-module(trace_reader).

-export([start/0, stop/0]).
-export([load_traces/1]).
                                                % Add focus/1 export
-export([breakpoint/3, focus/1, continue/0, reverse_continue/0]).
-export([step/0, reverse_step/0, next/0, reverse_next/0]).
-export([current_term/0]).

%%====================================================================
%% Application Management (Keep as is)
%%====================================================================
start() ->
    case application:ensure_all_started(trace_reader) of
        {ok, _} -> io:format("Trace reader application started.~n"), ok; {ok, _, _} -> io:format("Trace reader application started.~n"), ok; {error, {already_started, trace_reader}} -> io:format("Trace reader application already started.~n"), ok; {error, Reason} -> io:format(standard_error, "Failed to start trace_reader: ~p~n", [Reason]), {error, Reason}
    end.
stop() -> application:stop(trace_reader).

%%====================================================================
%% Trace Loading (Keep as is)
%%====================================================================
load_traces(Dir) ->
    case filelib:is_dir(Dir) of
        true -> AbsDir = filename:absname(Dir), io:format("Loading traces from: ~s~n", [AbsDir]),
                case trace_reader_server:load_traces(AbsDir) of
                    {ok, FirstTerm} = Reply when is_tuple(FirstTerm), element(1, FirstTerm) == trace -> io:format("First trace term (PID: ~p): ~p~n", [element(2, FirstTerm), FirstTerm]), Reply;
                    {ok, FirstOutput} = Reply-> io:format("First trace output: ~p~n", [FirstOutput]), Reply; % Handle no_terms etc.
                    Error -> io:format(standard_error, "Error loading traces: ~p~n", [Error]), Error
                end;
        false -> io:format(standard_error, "Error: '~s' is not a directory.~n", [Dir]), {error, not_a_directory}
    end.

%%====================================================================
%% Debugging Commands
%%====================================================================

%% Breakpoint (Keep as is)
-spec breakpoint(Pid :: pid() | 'all', MFA :: {atom(), atom(), arity()}, MatchSpec :: any()) -> ok | {error, any()}.
breakpoint(Pid, MFA = {M,F,A}, MatchSpec) when is_atom(M), is_atom(F), is_integer(A), A >= 0 ->
    case trace_reader_server:breakpoint(Pid, MFA, MatchSpec) of ok -> ok; Other -> Other end;
breakpoint(Pid, MFA, MatchSpec) -> io:format(standard_error, "Error: Invalid breakpoint specification. Pid=~p, MFA=~p, MatchSpec=~p~n", [Pid, MFA, MatchSpec]), {error, bad_breakpoint_spec}.

%% @doc Sets the focus to a specific PID for subsequent navigation commands.
%%      Returns {ok, focused, Pid} | {error, pid_not_found_in_trace, Pid}.
-spec focus(Pid :: pid()) -> {ok, focused, pid()} | {error, pid_not_found_in_trace, pid()} | {error, any()}.
focus(Pid) when is_pid(Pid) ->
    Result = trace_reader_server:focus(Pid),
    case Result of
        {ok, focused, P} -> io:format("Focus set to PID: ~p~n", [P]);
        {error, pid_not_found_in_trace, P} -> io:format(standard_error, "Error: PID ~p not found in loaded trace.~n", [P]);
        Error -> io:format(standard_error, "Error setting focus: ~p~n", [Error])
    end,
    Result;
focus(Other) ->
    io:format(standard_error, "Error: Invalid argument to focus/1. Expected PID, got: ~p~n", [Other]),
    {error, badarg}.

%% Continue (Keep as is - server logic changed)
-spec continue() -> {breakpoint_hit, term()} | end_of_trace | {error, any()}.
continue() -> Result = trace_reader_server:continue(), print_result("Continue", Result), Result.

%% Reverse Continue (Keep as is - server logic changed)
-spec reverse_continue() -> {breakpoint_hit, term()} | start_of_trace | {error, any()}.
reverse_continue() -> Result = trace_reader_server:reverse_continue(), print_result("Reverse Continue", Result), Result.

%% Step (Keep as is - server logic changed)
-spec step() -> {stepped_to, term()} | end_of_trace | {no_step_on_exit, term()} | {error, any()}.
step() -> Result = trace_reader_server:step(), print_result("Step", Result), Result.

%% Reverse Step (Keep as is - server logic changed)
-spec reverse_step() -> {stepped_to, term()} | start_of_trace | {no_step_on_spawned, term()} | {error, any()}.
reverse_step() -> Result = trace_reader_server:reverse_step(), print_result("Reverse Step", Result), Result.

%% Next (Keep as is - server logic changed)
-spec next() -> {next_returned_to, term()} | {stepped_to, term()} | end_of_trace | {error, any()}.
next() -> Result = trace_reader_server:next(), print_result("Next", Result), Result.

%% Reverse Next (Keep as is - server logic changed)
-spec reverse_next() -> {reverse_next_called_from, term()} | {stepped_to, term()} | start_of_trace | {error, any()}.
reverse_next() -> Result = trace_reader_server:reverse_next(), print_result("Reverse Next", Result), Result.

%% Current Term (Keep as is)
-spec current_term() -> {ok, term() | start_of_trace | end_of_trace}.
current_term() ->
    Result = trace_reader_server:get_current_term(),
    case Result of {ok, Term} when is_tuple(Term) -> io:format("Current: ~p~n", [Term]); _ -> io:format("Current: ~p~n", [Result]) end,
    Result.

%%====================================================================
%% Internal helper (Keep as is)
%%====================================================================
print_result(Cmd, {Type, Term}) when Type == breakpoint_hit; Type == stepped_to; Type == next_returned_to; Type == reverse_next_called_from; Type == no_step_on_exit; Type == no_step_on_spawned -> io:format("~s Result: [~p] ~p~n", [Cmd, Type, Term]);
print_result(Cmd, end_of_trace) -> io:format("~s Result: [end_of_trace]~n", [Cmd]);
print_result(Cmd, start_of_trace) -> io:format("~s Result: [start_of_trace]~n", [Cmd]);
print_result(Cmd, {ok, focused, Pid}) -> io:format("~s Result: [~p] ~p~n", [Cmd, focused, Pid]); % Added for focus reply
print_result(Cmd, Error = {error, _}) -> io:format(standard_error, "~s Error: ~p~n", [Cmd, Error]);
print_result(Cmd, Other) -> io:format("~s Result: ~p~n", [Cmd, Other]).
