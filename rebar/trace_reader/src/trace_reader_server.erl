-module(trace_reader_server).
-behaviour(gen_server).

-export([start_link/0, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Public API via trace_reader module will call this server
-export([load_traces/1, breakpoint/3, continue/0, reverse_continue/0]).
-export([step/0, reverse_step/0, next/0, reverse_next/0, get_current_term/0]).

-record(state, {
    trace_dir :: string() | undefined,
    all_terms :: [{integer(), term()}], % GlobalIndex, Term
    current_index :: integer(), % Index in all_terms (-1 = before start, N = at term N)
    breakpoints :: [{BPid :: pid() | 'all', MFA :: {atom(), atom(), arity()}, MatchSpec :: any()}],
    last_output :: term() | {info, string()} | undefined
}).

% Define a default chunk size for reading
-define(CHUNK_SIZE, 65536). % 64KB

%%%===================================================================
%%% Public API Functions (called by trace_reader.erl)
%%%===================================================================

load_traces(Dir) ->
    gen_server:call(?MODULE, {load_traces, Dir}).

breakpoint(Pid, MFA, MatchSpec) ->
    gen_server:call(?MODULE, {breakpoint, Pid, MFA, MatchSpec}).

continue() ->
    gen_server:call(?MODULE, continue).

reverse_continue() ->
    gen_server:call(?MODULE, reverse_continue).

step() ->
    gen_server:call(?MODULE, step).

reverse_step() ->
    gen_server:call(?MODULE, reverse_step).

next() ->
    gen_server:call(?MODULE, next).

reverse_next() ->
    gen_server:call(?MODULE, reverse_next).

get_current_term() ->
     gen_server:call(?MODULE, get_current_term).


%%%===================================================================
%%% Server Start/Init
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = #state{
        trace_dir = undefined,
        all_terms = [],
        current_index = -1, % Before the first term
        breakpoints = [],
        last_output = {info, "Server initialized. Load a trace directory."}
    },
    {ok, State}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

handle_call({load_traces, Dir}, _From, State) ->
    case filelib:is_dir(Dir) of
        true ->
            % Use the NEW chunked reading function
            case read_all_terms_chunked(Dir) of
                {ok, Terms} when is_list(Terms), Terms =/= [] ->
                    FirstTermTuple = lists:nth(1, Terms),
                    {_Index, FirstTerm} = FirstTermTuple,
                    NewState = State#state{
                        trace_dir = Dir,
                        all_terms = Terms,
                        current_index = 0, % Positioned at the first term
                        breakpoints = State#state.breakpoints, % Keep existing breakpoints
                        last_output = FirstTerm
                    },
                    {reply, {ok, FirstTerm}, NewState};
                {ok, []} ->
                    NewState = State#state{
                        trace_dir = Dir,
                        all_terms = [],
                        current_index = -1,
                        breakpoints = State#state.breakpoints,
                         last_output = {info, "Trace directory loaded, but no terms found."}
                    },
                    {reply, {ok, no_terms}, NewState};
                {error, Reason} ->
                    {reply, {error, {loading_failed, Reason}}, State}
            end;
        false ->
            {reply, {error, not_a_directory}, State}
    end;

% ... (keep the other handle_call clauses for breakpoint, continue, step, etc. AS THEY WERE) ...
% They operate on the `all_terms` list which is now populated differently, but
% their logic for navigating that list remains the same.

handle_call(continue, _From, State = #state{all_terms = Terms, current_index = CurrentIdx}) ->
    case find_breakpoint_forward(Terms, State#state.breakpoints, CurrentIdx + 1) of
        {found, NewIndex, Term} ->
            NewState = State#state{current_index = NewIndex, last_output = Term},
            {reply, {breakpoint_hit, Term}, NewState};
        not_found ->
            LastIndex = length(Terms) - 1,
            NewState = State#state{current_index = LastIndex, last_output = {info, "End of trace reached"}},
            {reply, end_of_trace, NewState}
    end;

handle_call(reverse_continue, _From, State = #state{all_terms = Terms, current_index = CurrentIdx}) ->
    case find_breakpoint_backward(Terms, State#state.breakpoints, CurrentIdx - 1) of
         {found, NewIndex, Term} ->
            NewState = State#state{current_index = NewIndex, last_output = Term},
            {reply, {breakpoint_hit, Term}, NewState};
        not_found ->
             NewState = State#state{current_index = -1, last_output = {info, "Start of trace reached"}},
             {reply, start_of_trace, NewState}
    end;

handle_call(step, _From, State = #state{all_terms = Terms, current_index = CurrentIdx}) ->
    MaxIndex = length(Terms) - 1,
    case get_term_at(Terms, CurrentIdx) of
        {ok, {trace, _, exit, _}} ->
            % Do nothing when step is called on an exit trace
            {reply, {no_step_on_exit, State#state.last_output}, State};
        {ok, _CurrentTerm} -> % Ok to step
            NewIndex = CurrentIdx + 1,
            if
                NewIndex > MaxIndex ->
                     NewState = State#state{current_index = MaxIndex, last_output = {info, "End of trace reached"}},
                     {reply, end_of_trace, NewState};
                true ->
                    {ok, NewTerm} = get_term_at(Terms, NewIndex),
                    NewState = State#state{current_index = NewIndex, last_output = NewTerm},
                    {reply, {stepped_to, NewTerm}, NewState}
            end;
         not_found -> % Should not happen if index is managed correctly, but handle defensively
             {reply, end_of_trace, State#state{last_output = {info, "End of trace reached"}}}
    end;

handle_call(reverse_step, _From, State = #state{all_terms = Terms, current_index = CurrentIdx}) ->
    NewIndex = CurrentIdx - 1,
    if
        NewIndex < 0 ->
            NewState = State#state{current_index = -1, last_output = {info, "Start of trace reached"}},
            {reply, start_of_trace, NewState};
        true ->
            {ok, NewTerm} = get_term_at(Terms, NewIndex),
            case NewTerm of
                 {trace, _, spawned, _, _} ->
                      % Do nothing when reverse_step lands on a spawned trace
                      {reply, {no_step_on_spawned, State#state.last_output}, State};
                 _ ->
                    NewState = State#state{current_index = NewIndex, last_output = NewTerm},
                    {reply, {stepped_to, NewTerm}, NewState}
            end
    end;

handle_call(next, _From, State = #state{all_terms = Terms, current_index = CurrentIdx}) ->
    MaxIndex = length(Terms) - 1,
    case get_term_at(Terms, CurrentIdx) of
        {ok, {trace, Pid, call, {_M, _F, _Args}, CallerMFA}} ->
            % We are on a call, step until corresponding return_to
            ExpectedReturn = {trace, Pid, return_to, CallerMFA}, % Match Pid and Caller's MFA
            case find_matching_term_forward(Terms, CurrentIdx + 1, fun(T) -> T =:= ExpectedReturn end) of
                {found, NewIndex, ReturnTerm} ->
                     NewState = State#state{current_index = NewIndex, last_output = ReturnTerm},
                     {reply, {next_returned_to, ReturnTerm}, NewState};
                not_found -> % Reached end before finding return
                     NewState = State#state{current_index = MaxIndex, last_output = {info, "End of trace reached during next"}},
                     {reply, end_of_trace, NewState}
            end;
        {ok, _OtherTerm} -> % Not on a call, behave like step
             handle_call(step, _From, State); % Reuse step logic
        not_found -> % At start or end
             {reply, {error, cannot_next_at_edge}, State}
    end;

handle_call(reverse_next, _From, State = #state{all_terms = Terms, current_index = CurrentIdx}) ->
     case get_term_at(Terms, CurrentIdx) of
        {ok, {trace, Pid, return_to, CallerMFA}} ->
             % We are on a return_to, step backwards until corresponding call
             case find_matching_term_backward(Terms, CurrentIdx - 1, fun({trace, P, call, _, C}) -> P =:= Pid andalso C =:= CallerMFA; (_) -> false end) of
                  {found, NewIndex, CallTerm} ->
                     NewState = State#state{current_index = NewIndex, last_output = CallTerm},
                     {reply, {reverse_next_called_from, CallTerm}, NewState};
                  not_found -> % Reached start before finding call
                     NewState = State#state{current_index = -1, last_output = {info, "Start of trace reached during reverse_next"}},
                     {reply, start_of_trace, NewState}
             end;
        {ok, _OtherTerm} -> % Not on a return_to, behave like reverse_step
             handle_call(reverse_step, _From, State); % Reuse reverse_step logic
         not_found -> % At start or end
             {reply, {error, cannot_reverse_next_at_edge}, State}
    end;

handle_call(get_current_term, _From, State = #state{all_terms = Terms, current_index = CurrentIdx}) ->
    case get_term_at(Terms, CurrentIdx) of
        {ok, Term} -> {reply, {ok, Term}, State};
        not_found when CurrentIdx == -1 -> {reply, {ok, start_of_trace}, State};
        not_found -> {reply, {ok, end_of_trace}, State} % Should ideally not happen if index is correct
    end;


handle_call(_Request, _From, State) ->
    {reply, {error, invalid_request}, State}.

%%%===================================================================
%%% Optional gen_server callbacks
%%%===================================================================

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

%% <<< NEW CHUNKED READING LOGIC >>>

%% Parses as many terms as possible from a binary buffer.
%% Returns {ParsedTerms :: [term()], Remainder :: binary()}
parse_terms_from_buffer(<<>>, AccTerms) -> % Empty buffer, finished
    {lists:reverse(AccTerms), <<>>};
parse_terms_from_buffer(Buffer, AccTerms) ->
    try binary_to_term(Buffer, [used]) of
        {Term, UsedBytes} ->
                <<_ParsedTermBin:UsedBytes/binary, RestBinary/binary>> = Buffer,
                parse_terms_from_buffer(RestBinary, [Term | AccTerms])    
    catch
        error:Reason:Stacktrace ->
            % Log unexpected errors from binary_to_term itself
            error_logger:error_msg("Unexpected error in binary_to_term: ~p, Buffer Size: ~p~nStack: ~p",
                                   [Reason, byte_size(Buffer), Stacktrace]),
             % Treat as unparseable
            {lists:reverse(AccTerms), Buffer}
    end.

%% Reads terms from all trace files in a directory using chunking.
%% Returns {ok, [{GlobalIndex, Term}]} or {error, Reason}
read_all_terms_chunked(Dir) ->
    try
        Files = find_trace_files(Dir), % Reuse existing helper
        io:format("Found trace files: ~p~n", [Files]),
        process_files_chunked(Files, Dir, 0, [], <<>>) % Start with index 0, no terms, empty buffer
    catch
        error:Reason:Stacktrace ->
             io:format(standard_error, "Error loading terms: ~p~nStacktrace: ~p~n", [Reason, Stacktrace]),
             {error, Reason};
         throw:Error ->
             io:format(standard_error, "Aborted loading terms: ~p~n", [Error]),
              Error % Propagate thrown errors (like file open error)
    end.

%% Processes a list of files, accumulating terms and managing the buffer across files.
process_files_chunked([], _Dir, GlobalIndex, AccTerms, <<>>) ->
     % All files processed, no remaining buffer
     io:format("Finished processing files. Total terms: ~p~n", [GlobalIndex]),
    {ok, AccTerms};
process_files_chunked([], _Dir, GlobalIndex, AccTerms, Remainder) ->
     % All files processed, but leftover bytes couldn't form a term
     io:format(standard_error, "Warning: Trace loading finished with ~p unparsed bytes left in buffer.~n", [byte_size(Remainder)]),
     io:format("Finished processing files. Total terms: ~p~n", [GlobalIndex]),
     {ok, AccTerms};
process_files_chunked([File | RestFiles], Dir, GlobalIndex, AccTerms, Buffer) ->
    FullPath = filename:join(Dir, File),
    io:format("Processing file: ~s~n", [FullPath]),
    case file:open(FullPath, [read, binary]) of
        {ok, Handle} ->
            try read_file_chunks(Handle, GlobalIndex, AccTerms, Buffer) of
                {NewIndex, NewAccTerms, NewBuffer} ->
                     process_files_chunked(RestFiles, Dir, NewIndex, NewAccTerms, NewBuffer)
            after
                 file:close(Handle) % Ensure file is closed
             end;
         {error, Reason} ->
             io:format(standard_error, "Error opening file ~p: ~p~n", [FullPath, Reason]),
             throw({error, {file_open_error, FullPath, Reason}}) % Abort by throwing
     end.

%% Reads chunks from a single file handle, parses terms, accumulates results.
read_file_chunks(Handle, GlobalIndex, AccTerms, Buffer) ->
    case file:read(Handle, ?CHUNK_SIZE) of
        {ok, Chunk} ->
            CombinedBuffer = case Buffer of % Efficiently combine buffer and chunk
                                 <<>> -> Chunk;
                                 _ -> <<Buffer/binary, Chunk/binary>>
                             end,
            {ParsedTerms, Remainder} = parse_terms_from_buffer(CombinedBuffer, []),
            {NewIndex, IndexedTerms} = index_terms(ParsedTerms, GlobalIndex),
            % Append newly indexed terms (order matters for final list)
            read_file_chunks(Handle, NewIndex, AccTerms ++ IndexedTerms, Remainder);
        eof ->
            % End of this file, return final state for this file
             {GlobalIndex, AccTerms, Buffer}; % Pass remaining buffer to next file's processing
         {error, Reason} ->
             io:format(standard_error, "Error reading from file handle: ~p~n", [Reason]),
             throw({error, {file_read_error, Reason}}) % Abort by throwing
     end.

%% Helper to add global indices to newly parsed terms.
%% Returns {NextIndex, [{Index, Term}]}
index_terms([], StartIndex) ->
    {StartIndex, []};
index_terms(ParsedTerms, StartIndex) ->
    EndIndex = StartIndex + length(ParsedTerms) - 1,
    Indices = lists:seq(StartIndex, EndIndex),
    IndexedTerms = lists:zip(Indices, ParsedTerms),
    {EndIndex + 1, IndexedTerms}. % Return the index for the *next* term


%% Finds and sorts trace files (traces, traces.1, traces.2, ..., traces.10)
find_trace_files(Dir) ->
    {ok, AllFiles} = file:list_dir(Dir),
    TraceFiles = [F || F <- AllFiles, is_trace_file(F)],
    lists:sort(fun compare_trace_files/2, TraceFiles).

is_trace_file("traces") -> true;
is_trace_file("traces." ++ Rest) ->
    case string:to_integer(Rest) of
        {Int, ""} when Int >= 0 -> true;
        _ -> false
    end;
is_trace_file(_) -> false.

compare_trace_files("traces", "traces." ++ _) -> true; % traces comes first
compare_trace_files("traces." ++ _, "traces") -> false;
compare_trace_files("traces." ++ A, "traces." ++ B) ->
    {IntA, _} = string:to_integer(A),
    {IntB, _} = string:to_integer(B),
    IntA =< IntB;
compare_trace_files(A, B) -> % Should not happen if filtered correctly, but provide default
    A =< B.

%% Get term at a specific global index from the state list [{Index, Term}]
get_term_at(Terms, Index) ->
    % Assuming indices are contiguous 0..N-1 after loading
    MaxIdx = length(Terms) - 1,
    if
        Index >= 0, Index =< MaxIdx ->
            {_StoredIdx, Term} = lists:nth(Index + 1, Terms), % lists:nth is 1-based
            {ok, Term};
        true ->
            not_found
    end.

%% Find breakpoint forward from StartIndex
find_breakpoint_forward(Terms, Breakpoints, StartIndex) ->
    MaxIndex = length(Terms) - 1,
    find_breakpoint_forward(Terms, Breakpoints, StartIndex, MaxIndex).

find_breakpoint_forward(_Terms, _Breakpoints, CurrentIndex, MaxIndex) when CurrentIndex > MaxIndex ->
    not_found;
find_breakpoint_forward(Terms, Breakpoints, CurrentIndex, MaxIndex) ->
     {ok, Term} = get_term_at(Terms, CurrentIndex), % Assuming get_term_at handles not_found if index is bad
    case check_breakpoint(Term, Breakpoints) of
        true ->
            {found, CurrentIndex, Term};
        false ->
            find_breakpoint_forward(Terms, Breakpoints, CurrentIndex + 1, MaxIndex)
    end.

%% Find breakpoint backward from StartIndex
find_breakpoint_backward(Terms, Breakpoints, StartIndex) ->
    MinIndex = 0, % Assuming 0 is the lowest valid index
    find_breakpoint_backward(Terms, Breakpoints, StartIndex, MinIndex).

find_breakpoint_backward(_Terms, _Breakpoints, CurrentIndex, _MinIndex) when CurrentIndex < 0 ->
     not_found;
find_breakpoint_backward(Terms, Breakpoints, CurrentIndex, MinIndex) ->
     {ok, Term} = get_term_at(Terms, CurrentIndex),
     case check_breakpoint(Term, Breakpoints) of
         true ->
             {found, CurrentIndex, Term};
         false ->
             find_breakpoint_backward(Terms, Breakpoints, CurrentIndex - 1, MinIndex)
     end.

%% Check if a term matches any breakpoint
%% NOTE: Ignores MatchSpec for now, only checks Pid and MFA
check_breakpoint({trace, Pid, call, {M, F, Args}, _Caller}, Breakpoints) ->
    Arity = length(Args),
    MFA = {M, F, Arity},
    lists:any(fun({BPid, BMFA, _MatchSpec}) ->
                    PidMatch = (BPid == 'all') orelse (BPid == Pid),
                    MFAMatch = (BMFA == MFA),
                    PidMatch andalso MFAMatch
              end, Breakpoints);
check_breakpoint(_OtherTerm, _Breakpoints) ->
    false.


%% Find a term matching a predicate function, searching forward
find_matching_term_forward(Terms, StartIndex, PredFun) ->
     MaxIndex = length(Terms) - 1,
     find_matching_term_forward(Terms, StartIndex, MaxIndex, PredFun).

find_matching_term_forward(_Terms, CurrentIndex, MaxIndex, _PredFun) when CurrentIndex > MaxIndex ->
    not_found;
find_matching_term_forward(Terms, CurrentIndex, MaxIndex, PredFun) ->
    case get_term_at(Terms, CurrentIndex) of
        {ok, Term} ->
            case PredFun(Term) of
                true -> {found, CurrentIndex, Term};
                false -> find_matching_term_forward(Terms, CurrentIndex + 1, MaxIndex, PredFun)
            end;
        not_found -> % Should not happen in normal flow if MaxIndex is correct
             not_found
    end.


%% Find a term matching a predicate function, searching backward
find_matching_term_backward(Terms, StartIndex, PredFun) ->
     MinIndex = 0,
     find_matching_term_backward(Terms, StartIndex, MinIndex, PredFun).

find_matching_term_backward(_Terms, CurrentIndex, _MinIndex, _PredFun) when CurrentIndex < 0 ->
     not_found;
find_matching_term_backward(Terms, CurrentIndex, MinIndex, PredFun) ->
     case get_term_at(Terms, CurrentIndex) of
         {ok, Term} ->
             case PredFun(Term) of
                 true -> {found, CurrentIndex, Term};
                 false -> find_matching_term_backward(Terms, CurrentIndex - 1, MinIndex, PredFun)
             end;
         not_found -> % Should not happen in normal flow if MinIndex is correct
             not_found
     end.