-module(trace_reader).

-export([start/0, stop/0]).
-export([load_traces/1]).
-export([breakpoint/3, continue/0, reverse_continue/0]).
-export([step/0, reverse_step/0, next/0, reverse_next/0]).
-export([current_term/0]).

%%====================================================================
%% Application Management
%%====================================================================

%% Starts the trace_reader application
start() ->
    case application:ensure_all_started(trace_reader) of
        {ok, _} ->
            io:format("Trace reader application started.~n"),
             ok;
        {ok, _, _} -> % For newer OTP versions
             io:format("Trace reader application started.~n"),
             ok;
        {error, {already_started, trace_reader}} ->
             io:format("Trace reader application already started.~n"),
             ok;
        {error, Reason} ->
            io:format(standard_error, "Failed to start trace_reader: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Stops the trace_reader application
stop() ->
    application:stop(trace_reader).

%%====================================================================
%% Trace Loading
%%====================================================================

%% @doc Loads trace files from the specified directory.
%%      Outputs the first trace term found.
%%      Returns {ok, FirstTerm} | {ok, no_terms} | {error, Reason}.
-spec load_traces(Dir :: string()) -> {ok, term() | no_terms} | {error, atom() | tuple()}.
load_traces(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
             AbsDir = filename:absname(Dir),
             io:format("Loading traces from: ~s~n", [AbsDir]),
             case trace_reader_server:load_traces(AbsDir) of
                 {ok, FirstTerm} = Reply when is_tuple(FirstTerm) ->
                    io:format("First trace term: ~p~n", [FirstTerm]),
                    Reply;
                 {ok, no_terms} = Reply ->
                     io:format("No trace terms found in directory.~n"),
                     Reply;
                Error ->
                     io:format(standard_error, "Error loading traces: ~p~n", [Error]),
                     Error
             end;
        false ->
             io:format(standard_error, "Error: '~s' is not a directory.~n", [Dir]),
            {error, not_a_directory}
    end.


%%====================================================================
%% Debugging Commands
%%====================================================================

%% @doc Sets a breakpoint on {M, F, Arity}.
%%      Pid: The process ID to match, or 'all' for any process.
%%      MFA: The {Module, Function, Arity} tuple.
%%      MatchSpec: Currently ignored, placeholder for future enhancement.
%%      Returns ok | {error, Reason}.
-spec breakpoint(Pid :: pid() | 'all', MFA :: {atom(), atom(), arity()}, MatchSpec :: any()) -> ok | {error, any()}.
breakpoint(Pid, MFA = {M,F,A}, MatchSpec) when is_atom(M), is_atom(F), is_integer(A), A >= 0 ->
    case trace_reader_server:breakpoint(Pid, MFA, MatchSpec) of
        ok -> ok;
        Other -> Other % Propagate potential errors from gen_server
    end;
breakpoint(Pid, MFA, MatchSpec) ->
     io:format(standard_error, "Error: Invalid breakpoint specification. Pid=~p, MFA=~p, MatchSpec=~p~n",
               [Pid, MFA, MatchSpec]),
     {error, bad_breakpoint_spec}.


%% @doc Continues execution until a breakpoint is hit or the end of the trace.
%%      Outputs the term where it stops.
%%      Returns {breakpoint_hit, Term} | end_of_trace | {error, Reason}.
-spec continue() -> {breakpoint_hit, term()} | end_of_trace | {error, any()}.
continue() ->
    Result = trace_reader_server:continue(),
    print_result("Continue", Result),
    Result.

%% @doc Continues execution backwards until a breakpoint is hit or the start of the trace.
%%      Outputs the term where it stops.
%%      Returns {breakpoint_hit, Term} | start_of_trace | {error, Reason}.
-spec reverse_continue() -> {breakpoint_hit, term()} | start_of_trace | {error, any()}.
reverse_continue() ->
    Result = trace_reader_server:reverse_continue(),
    print_result("Reverse Continue", Result),
    Result.


%% @doc Steps one trace message forward. Does nothing if current term is an 'exit' trace.
%%      Outputs the new current term.
%%      Returns {stepped_to, NewTerm} | end_of_trace | {no_step_on_exit, CurrentTerm} | {error, Reason}.
-spec step() -> {stepped_to, term()} | end_of_trace | {no_step_on_exit, term()} | {error, any()}.
step() ->
    Result = trace_reader_server:step(),
    print_result("Step", Result),
    Result.

%% @doc Steps one trace message backward. Does nothing if the destination term is a 'spawned' trace.
%%      Outputs the new current term.
%%      Returns {stepped_to, NewTerm} | start_of_trace | {no_step_on_spawned, CurrentTerm} | {error, Reason}.
-spec reverse_step() -> {stepped_to, term()} | start_of_trace | {no_step_on_spawned, term()} | {error, any()}.
reverse_step() ->
    Result = trace_reader_server:reverse_step(),
    print_result("Reverse Step", Result),
    Result.

%% @doc Steps forward. If on a 'call' trace, steps until the corresponding 'return_to'. Otherwise, behaves like step().
%%      Outputs the term where it stops.
%%      Returns {next_returned_to, ReturnTerm} | {stepped_to, NewTerm} | end_of_trace | {error, Reason}.
-spec next() -> {next_returned_to, term()} | {stepped_to, term()} | end_of_trace | {error, any()}.
next() ->
     Result = trace_reader_server:next(),
     print_result("Next", Result),
     Result.

%% @doc Steps backward. If on a 'return_to' trace, steps backward until the corresponding 'call'. Otherwise, behaves like reverse_step().
%%      Outputs the term where it stops.
%%      Returns {reverse_next_called_from, CallTerm} | {stepped_to, NewTerm} | start_of_trace | {error, Reason}.
-spec reverse_next() -> {reverse_next_called_from, term()} | {stepped_to, term()} | start_of_trace | {error, any()}.
reverse_next() ->
     Result = trace_reader_server:reverse_next(),
     print_result("Reverse Next", Result),
     Result.

%% @doc Returns the current trace term without moving position.
%%      Returns {ok, Term} | {ok, start_of_trace} | {ok, end_of_trace}.
-spec current_term() -> {ok, term() | start_of_trace | end_of_trace}.
current_term() ->
     Result = trace_reader_server:get_current_term(),
     case Result of
         {ok, Term} -> io:format("Current: ~p~n", [Term]);
         _ -> io:format("Current: ~p~n", [Result]) % e.g., {ok, start_of_trace}
     end,
     Result.


%%====================================================================
%% Internal helper
%%====================================================================
print_result(Cmd, {Type, Term}) when Type == breakpoint_hit;
                                     Type == stepped_to;
                                     Type == next_returned_to;
                                     Type == reverse_next_called_from;
                                     Type == no_step_on_exit;
                                     Type == no_step_on_spawned ->
    io:format("~s Result: [~p] ~p~n", [Cmd, Type, Term]);
print_result(Cmd, end_of_trace) ->
    io:format("~s Result: [end_of_trace]~n", [Cmd]);
print_result(Cmd, start_of_trace) ->
     io:format("~s Result: [start_of_trace]~n", [Cmd]);
print_result(Cmd, Error = {error, _}) ->
     io:format(standard_error, "~s Error: ~p~n", [Cmd, Error]);
print_result(Cmd, Other) -> % Catch-all for unexpected results
     io:format("~s Result: ~p~n", [Cmd, Other]).
