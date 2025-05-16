-module(trace_reader_server).
-behaviour(gen_server).

-export([start_link/0, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Public API
-export([load_traces/1]).
-export([breakpoint/3, breakpoint/4, list_breakpoints/0, toggle_breakpoint/1, clear_breakpoints/0, clear_breakpoint/1]).
-export([list_active_pids/0, focus/1, get_current_term/0, continue/0, reverse_continue/0, step/0, reverse_step/0, next/0, reverse_next/0]).
%-export([step/0, step/1, reverse_step/0, reverse_step/1, next/0, next/1, reverse_next/0, reverse_next/1, get_current_term/0]).
-record(state, {
    trace_dir :: string() | undefined,
    sorted_files :: [string()], % List of ALL sorted full file paths
    current_file_index :: integer(), % Index into sorted_files for the current file, -1 if before start
    current_term_index :: integer(), % Index within the terms of the current file, -1 if before start of file
    % Map holding terms for currently loaded files (max 3: Prev, Current, Next)
    loaded_files :: #{FileIndex :: integer() => [{TermInFileIndex :: non_neg_integer(), Term :: term()}]},
    focused_pid :: pid() | undefined, % The PID currently being tracked
    active_pids :: [pid()], % List of all PIDs currently active
    breakpoint_next_index :: non_neg_integer(), % Next index for new breakpoints
    breakpoints :: [{non_neg_integer(), BPid :: pid() | 'all', MFA :: {atom(), atom(), arity()}, StopFun :: break_fun(), SkipCount :: non_neg_integer()}], % List of breakpoints
    callstack :: [{MFA :: {atom(),atom(),arity()}, {File :: string() | undefined, Line :: non_neg_integer() | undefined}}],
    last_reply :: {atom(), term()} | atom() | undefined % Store last reply type for context
}).
-type break_fun() :: fun((MFACallstack :: [{atom(), atom(), arity()}], Args :: [term()]) -> boolean()).
%-type watch_fun() :: fun((MFACallstack :: [{atom(), atom(), arity()}], Args :: [term()]) -> term()).
% Define a default chunk size for reading files when loading them
-define(CHUNK_SIZE, 65536). % 64KB

%%%===================================================================
%%% Public API Functions (remain the same externally)
%%%===================================================================
load_traces(Dir) -> gen_server:call(?MODULE, {load_traces, Dir}).
%% breakpoint should
breakpoint(Pid, MFA, BreakFun) -> gen_server:call(?MODULE, {breakpoint, Pid, MFA, BreakFun, 0}).
breakpoint(Pid, MFA, BreakFun, SkipCount) -> gen_server:call(?MODULE, {breakpoint, Pid, MFA, BreakFun, SkipCount}).
toggle_breakpoint(Index) -> gen_server:call(?MODULE, {toggle_breakpoint, Index}).
clear_breakpoint(Index) -> gen_server:call(?MODULE, {clear_breakpoint, Index}).
clear_breakpoints() -> gen_server:call(?MODULE, clear_breakpoints).
%watchpoint(Pid, MFA, WatchFun) -> gen_server:call(?MODULE, {watchpoint, Pid, MFA, WatchFun}).
list_breakpoints() -> gen_server:call(?MODULE, list_breakpoints).
%list_watchpoints() -> gen_server:call(?MODULE, list_watchpoints).
focus(Pid) -> gen_server:call(?MODULE, {focus, Pid}).
list_active_pids() -> gen_server:call(?MODULE, list_active_pids).
continue() -> gen_server:call(?MODULE, continue).
reverse_continue() -> gen_server:call(?MODULE, reverse_continue).
step() -> gen_server:call(?MODULE, step).
reverse_step() -> gen_server:call(?MODULE, reverse_step).
next() -> gen_server:call(?MODULE, next).
reverse_next() -> gen_server:call(?MODULE, reverse_next).
get_current_term() -> gen_server:call(?MODULE, get_current_term).

%%%===================================================================
%%% Server Start/Init
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = #state{
        trace_dir = undefined,
        sorted_files = [],
        current_file_index = -1,
        current_term_index = -1,
        loaded_files = #{},
        focused_pid = undefined,
        active_pids = #{},
        breakpoint_next_index = 1,
        breakpoints = [],
        callstack = [],
        last_reply = undefined
    },
    {ok, State}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

handle_call({load_traces, Dir}, _From, State) ->
    case filelib:is_dir(Dir) of
        true ->
            FullDir = filename:absname(Dir),
            % **FIX:** Call the now re-added find_trace_files/1
            case find_trace_files(FullDir) of % Returns sorted full paths {ok, [string()]}
                {ok, []} -> % No trace files
                     NewState = State#state{
                        trace_dir = FullDir, sorted_files = [], current_file_index = -1, current_term_index = -1,
                        loaded_files = #{}, focused_pid = undefined, breakpoints = #{}, active_pids = #{}
                    },
                    {reply, {ok, no_terms}, NewState#state{last_reply = no_terms}};

                {ok, SortedFiles = [FirstFile | _]} ->
                    % Load initial window (file 0 and maybe file 1)
                    case load_file_terms(0, FirstFile) of
                        {ok, Terms0} when Terms0 =/= [] ->
                             % Try loading file 1 as well
                             NextFilePath = case SortedFiles of [_, File1Path | _] -> File1Path; _ -> undefined end,
                             Loaded1 = case NextFilePath of
                                          undefined -> #{}; % No next file
                                          File1Path1 ->
                                              case load_file_terms(1, File1Path1) of
                                                  {ok, Terms1} -> #{1 => Terms1};
                                                  _ -> #{} % Failed to load file 1, proceed without it
                                              end
                                       end,
                            InitialLoaded = maps:put(0, Terms0, Loaded1),
                            {_FirstTermInFileIdx, FirstTerm} = hd(Terms0), % Get first term
                            FirstPid = get_pid_from_term(FirstTerm), % Get PID safely
                            NewState = State#state{
                                trace_dir = FullDir, sorted_files = SortedFiles, current_file_index = 0, current_term_index = 0,
                                loaded_files = InitialLoaded, focused_pid = FirstPid, active_pids = #{FirstPid => {0, 0}}, breakpoints = #{}
                            },
                            {reply, {ok, FirstTerm}, NewState#state{last_reply = {loaded, FirstTerm}}};
                        {ok, []} -> % First file empty
                            % TODO: Could try loading file 1 recursively here
                            {reply, {ok, no_terms}, State#state{last_reply = no_terms}};
                         {error, Reason} ->
                             {reply, {error, {load_file_error, 0, Reason}}, State}
                     end;
                {error, Reason} ->
                     {reply, {error, {find_files_error, Reason}}, State}
            end;
        false ->
            {reply, {error, not_a_directory}, State}
    end;

handle_call({breakpoint, PidSpec, MFA, StopFun, SkipCount}, _From, State) ->
        Map = State#state.breakpoints,
        NewBreakpoints = Map#{State#state.breakpoint_next_index => {PidSpec, MFA, StopFun, SkipCount}},
        NewState = State#state{breakpoints = NewBreakpoints,
                              breakpoint_next_index = State#state.breakpoint_next_index + 1},
        {reply, ok, NewState}; % Keep last_reply as is
handle_call({toggle_breakpoint, Index}, _From, State) ->
        Map = State#state.breakpoints,
        case maps:get(Index, Map, false) of
            false -> % Not found
                {reply, {error, breakpoint_not_found}, State};
            {disabled, Breakpoint} -> % Found disabled breakpoint
                NewBreakpoints = maps:put(Index, Breakpoint, Map),
                NewState = State#state{breakpoints = NewBreakpoints},
                {reply, {enabled_breakpoint, Breakpoint}, NewState};
            Breakpoint ->
                NewBreakpoints = maps:put(Index, {disabled, Breakpoint}, Map),
                NewState = State#state{breakpoints = NewBreakpoints},
                {reply, {disabled_breakpoint, Breakpoint}, NewState}
        end;
handle_call({clear_breakpoint, Index}, _From, State) ->
        Map = State#state.breakpoints,
        case maps:get(Index, Map, false) of
            false -> % Not found
                {reply, {error, breakpoint_not_found}, State};
            Breakpoint ->
                NewBreakpoints = maps:remove(Index, Map),
                NewState = State#state{breakpoints = NewBreakpoints},
                {reply, {removed_breakpoint, Breakpoint}, NewState}
        end;
handle_call(clear_breakpoints, _From, State) ->
        NewState = State#state{breakpoints = #{}, breakpoint_next_index = 1},
        {reply, {removed_breakpoints, State#state.breakpoints}, NewState};
handle_call(list_breakpoints, _From, State) ->
        {reply, State#state.breakpoints, State};
handle_call(list_active_pids, _From, State) ->
        {reply, State#state.active_pids, State};
handle_call({focus, Pid}, _From, #state{active_pids = ActivePids} = State) ->
    case maps:get(Pid, ActivePids, false) of
        false when is_pid(Pid) ->
                {reply, {error, pid_not_active}, State};
        false ->
                {reply, {error, invalid_pid}, State};
        {FileIdx,Idx} -> 
                NewState = State#state{focused_pid = Pid, current_file_index = FileIdx, current_term_index = Idx},
                {reply, {ok, focused, Pid}, NewState#state{last_reply = {focused, Pid}}}
       
    end;

handle_call(step, _From, State = #state{focused_pid = FPid}) ->
    if FPid == undefined -> {reply, {error, no_pid_focused}, State}; true ->
        case get_current_term_from_state(State) of
             {ok, {trace, FPidCheck, exit, _}} when FPidCheck == FPid -> % Check PID matches focus
                  {reply, {no_step_on_exit, State}, State};
             {ok, _} ->
                 find_next_matching(State, fun(Term) -> get_pid_from_term(Term) == FPid end, step);
             eof ->
                 {reply, end_of_trace, State#state{last_reply = end_of_trace}};
             start_of_trace -> % Stepping forward from start
                  find_next_matching(State, fun(Term) -> get_pid_from_term(Term) == FPid end, step)
        end
    end;

handle_call(reverse_step, _From, State = #state{focused_pid = FPid}) ->
     if FPid == undefined -> {reply, {error, no_pid_focused}, State}; true ->
         % Special case check is done *after* finding the term
          find_prev_matching(State, fun(Term) -> get_pid_from_term(Term) == FPid end, reverse_step)
     end;

handle_call(continue, _From, State = #state{focused_pid = FPid, breakpoints = BPs}) ->
     if FPid == undefined -> {reply, {error, no_pid_focused}, State}; true ->
          find_next_matching(State, fun(T) -> check_breakpoint_for_pid(T, BPs, FPid) end, continue)
     end;

handle_call(reverse_continue, _From, State = #state{focused_pid = FPid, breakpoints = BPs}) ->
      if FPid == undefined -> {reply, {error, no_pid_focused}, State}; true ->
          find_prev_matching(State, fun(T) -> check_breakpoint_for_pid(T, BPs, FPid) end, reverse_continue)
      end;

handle_call(next, _From, State = #state{focused_pid = FPid}) ->
     if FPid == undefined -> {reply, {error, no_pid_focused}, State}; true ->
          case get_current_term_from_state(State) of
              {ok, {trace, FPidCheck, call, _, CallerMFA}} when FPidCheck == FPid -> % Check PID matches focus
                   ExpectedReturn = {trace, FPid, return_to, CallerMFA},
                   find_next_matching(State, fun(T) -> T =:= ExpectedReturn end, next)
              ;
              _ -> % Not on a call for focused PID, or not at a term -> step
                   handle_call(step, _From, State)
          end
      end;

handle_call(reverse_next, _From, State = #state{focused_pid = FPid}) ->
     if FPid == undefined -> {reply, {error, no_pid_focused}, State}; true ->
         case get_current_term_from_state(State) of
             {ok, {trace, FPidCheck, return_to, CallerMFA}} when FPidCheck == FPid -> % Check PID matches focus
                  % **FIX:** Use '_' for shadowed/unused FPid in fun pattern
                  CallPredicate = fun({trace, _, call, _, C}) -> C =:= CallerMFA; (_) -> false end,
                  find_prev_matching(State, CallPredicate, reverse_next)
             ;
              _ -> % Not on a return_to for focused PID, or not at a term -> reverse_step
                   handle_call(reverse_step, _From, State)
         end
     end;

handle_call(get_current_term, _From, State) ->
    case get_current_term_from_state(State) of
        {ok, Term} -> {reply, {ok, Term}, State};
        Other -> {reply, {ok, Other}, State} % Other is eof or start_of_trace
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, invalid_request}, State}.

%%%===================================================================
%%% Optional gen_server callbacks
%%%===================================================================
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok. % File handles managed by load_file_terms
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

% **FIX:** Re-add find_trace_files and its helpers
%% Finds and sorts trace files, returning full paths
%% Returns {ok, [FullPath :: string()]} | {error, Reason}
find_trace_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, AllFiles} ->
            TraceFiles = [F || F <- AllFiles, is_trace_file(F)],
            Sorted = lists:sort(fun compare_trace_files/2, TraceFiles),
            FullPaths = [filename:join(Dir, F) || F <- Sorted],
            {ok, FullPaths};
        {error, Reason} ->
            {error, Reason}
    end.

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

%% Loads all terms from a single file path.
%% Returns {ok, [{TermInFileIndex, Term}]} | {error, Reason}
load_file_terms(FileIndexForLog, FilePath) ->
    io:format("Loading file ~p: ~s~n", [FileIndexForLog, FilePath]),
    case file:open(FilePath, [read, binary]) of
        {ok, Handle} ->
            try read_all_terms_from_handle(Handle, 0, <<>>) of
                {ok, Terms} -> {ok, Terms};
                {error, Reason} -> {error, Reason}
            after
                file:close(Handle)
            end;
        {error, Reason} ->
            {error, {file_open_error, Reason}}
    end.

%% Helper to read all terms from an open handle (uses chunking internally via modified parser)
%% Returns {ok, TermsList} | {error, Reason}
read_all_terms_from_handle(Handle, StartIndex, Buffer) ->
    case file:read(Handle, ?CHUNK_SIZE) of
        {ok, <<>>} -> % Empty read, proper EOF
            handle_eof_buffer(Buffer, StartIndex);
        {ok, Chunk} ->
            Combined = case Buffer of <<>> -> Chunk; _ -> <<Buffer/binary, Chunk/binary>> end,
            case parse_terms_from_buffer(Combined, []) of % Use the [used] only parser
                {ParsedTerms, <<>>} -> % Buffer fully parsed
                    {_NextIdx, Indexed} = index_terms(ParsedTerms, StartIndex),
                     % Recursive call expects {ok, Terms}, so wrap result
                     case read_all_terms_from_handle(Handle, StartIndex + length(ParsedTerms), <<>>) of
                         {ok, NextTerms} -> {ok, Indexed ++ NextTerms};
                         Error -> Error % Propagate error
                     end;
                 {ParsedTerms, Remainder} -> % Buffer partially parsed, need more data
                     {_NextIdx, Indexed} = index_terms(ParsedTerms, StartIndex),
                     case read_all_terms_from_handle(Handle, StartIndex + length(ParsedTerms), Remainder) of
                         {ok, NextTerms} -> {ok, Indexed ++ NextTerms};
                         Error -> Error % Propagate error
                     end
            end;
        eof -> % Direct eof from file:read
            handle_eof_buffer(Buffer, StartIndex);
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

handle_eof_buffer(<<>>, _StartIndex) -> {ok, []}; % EOF and empty buffer, success empty list
handle_eof_buffer(Buffer, StartIndex) ->
     % EOF with remaining buffer, try final parse
     case parse_terms_from_buffer(Buffer, []) of
         {ParsedTerms, <<>>} -> % Final buffer parsed cleanly
              {_NextIdx, Indexed} = index_terms(ParsedTerms, StartIndex),
              {ok, Indexed};
         {ParsedTerms, Remainder} -> % Still unparseable remainder at EOF
              error_logger:warning_msg("EOF reached with unparseable remainder (~p bytes)", [byte_size(Remainder)]),
              {_NextIdx, Indexed} = index_terms(ParsedTerms, StartIndex),
               {ok, Indexed} % Return what could be parsed
     end.

% parse_terms_from_buffer using [used] only (as finalized before)
parse_terms_from_buffer(<<>>, AccTerms) -> {lists:reverse(AccTerms), <<>>};
parse_terms_from_buffer(Buffer, AccTerms) ->
    try {Term, UsedBytes} = binary_to_term(Buffer, [used]),
        <<_ParsedTermBin:UsedBytes/binary, RestBinary/binary>> = Buffer,
        parse_terms_from_buffer(RestBinary, [Term | AccTerms])
    catch error:Reason:Stacktrace ->
        ShortBuffer = if byte_size(Buffer) > 128 -> <<(binary:part(Buffer, {0, 128}))/binary, "...">> ; true -> Buffer end,
        error_logger:warning_msg("Exception during binary_to_term (opt: [used]): ~p. Buffer(sample): ~p~nStack: ~p", [Reason, ShortBuffer, Stacktrace]),
        {lists:reverse(AccTerms), Buffer}
    end.

% index_terms (same as before)
index_terms([], StartIndex) -> {StartIndex, []};
index_terms(ParsedTerms, StartIndex) -> EndIndex = StartIndex + length(ParsedTerms) - 1, Indices = lists:seq(StartIndex, EndIndex), IndexedTerms = lists:zip(Indices, ParsedTerms), {EndIndex + 1, IndexedTerms}.

%% Ensures files for the window around TargetFileIndex are loaded, unloads others.
%% Window: Target-1, Target, Target+1
%% Returns {ok, NewState} | {error, Reason}
ensure_window_loaded(TargetFileIndex, State = #state{loaded_files = Loaded, sorted_files = Files}) ->
    MaxFileIndex = length(Files) - 1,
    IndicesToLoad = lists:usort(lists:filter(fun(I) -> I >= 0 andalso I =< MaxFileIndex end,
                                 [TargetFileIndex - 1, TargetFileIndex, TargetFileIndex + 1])),
    IndicesToKeep = IndicesToLoad, % Indices we absolutely want to have loaded
    CurrentIndices = maps:keys(Loaded),

    % Determine which files to unload (those loaded but not in IndicesToKeep)
    IndicesToUnload = lists:filter(fun(Idx) -> not lists:member(Idx, IndicesToKeep) end, CurrentIndices),

    % Unload first
    UnloadedMap = lists:foldl(fun(Idx, AccMap) ->
                                 io:format("Unloading file ~p~n", [Idx]), % Optional debug log
                                 maps:remove(Idx, AccMap)
                              end, Loaded, IndicesToUnload),

    % Load missing ones from IndicesToLoad
    LoadFun = fun(Idx, {ok, AccMap}) ->
                      case maps:is_key(Idx, AccMap) of
                           true -> {ok, AccMap}; % Already there
                           false ->
                               FilePath = lists:nth(Idx + 1, Files), % lists:nth is 1-based
                               case load_file_terms(Idx, FilePath) of
                                   {ok, Terms} -> {ok, maps:put(Idx, Terms, AccMap)};
                                   {error, Reason} -> {error, {load_file_error, Idx, Reason}} % Abort on error
                               end
                       end;
                 (_Idx, Error = {error, _}) -> Error % Propagate error
              end,

    case lists:foldl(LoadFun, {ok, UnloadedMap}, IndicesToLoad) of
        {ok, FinalLoadedMap} -> {ok, State#state{loaded_files = FinalLoadedMap}};
        {error, Reason} -> {error, Reason}
    end.


%% Get current term tuple {ok, Term} | eof | start_of_trace from state
get_current_term_from_state(#state{current_file_index = CFIdx, current_term_index = CTIdx, loaded_files = Loaded}) ->
     if CFIdx == -1 -> start_of_trace;
        CTIdx == -1 -> start_of_trace; % Logically before first term of file
        true ->
             case maps:get(CFIdx, Loaded, undefined) of
                 undefined -> error({internal_error, file_not_loaded, CFIdx}); % Should be loaded by ensure_window
                 TermsList ->
                     % TermInFileIndex in list is 0-based, matching CTIdx
                     case lists:keyfind(CTIdx, 1, TermsList) of
                         {CTIdx, Term} -> {ok, Term};
                         false when CTIdx == length(TermsList) -> eof; % Index is exactly one past the end
                         false -> error({internal_error, term_index_not_found, CFIdx, CTIdx}) % Should exist
                     end
             end
     end.


%% Finds the next term matching PredFun, starting AFTER the current position.
%% Handles window loading and file transitions.
%% OpType is for creating specific reply tuples.
find_next_matching(State, PredFun, OpType) ->
    find_next_matching_loop(State#state.current_file_index, State#state.current_term_index + 1, State, PredFun, OpType).

find_next_matching_loop(FileIdx, TermIdx, State = #state{sorted_files = Files}, PredFun, OpType) ->
    MaxFileIndex = length(Files) - 1,
    if FileIdx > MaxFileIndex -> % Base case: Past the last file
         {reply, end_of_trace, State#state{last_reply = end_of_trace}};
       true ->
          case ensure_window_loaded(FileIdx, State) of
              {ok, StateAfterLoad = #state{loaded_files = Loaded}} ->
                   case maps:get(FileIdx, Loaded, undefined) of
                       undefined -> % File index valid but failed to load or missing?
                           error_logger:error_msg("Internal Error: File ~p not loaded in find_next_matching_loop~n", [FileIdx]),
                           {reply, {error, internal_file_load_issue}, StateAfterLoad};
                       TermsList ->
                            case lists:dropwhile(fun({Idx, _Term}) -> Idx < TermIdx end, TermsList) of
                                [] -> % Reached end of this file's terms, try next file
                                      find_next_matching_loop(FileIdx + 1, 0, StateAfterLoad, PredFun, OpType);
                                Candidates -> % Check remaining terms in this file
                                    find_first_match_in_list(Candidates, FileIdx, StateAfterLoad, PredFun, OpType)
                            end
                   end;
               {error, Reason} ->
                   error_logger:error_msg("Error ensuring window loaded: ~p~n", [Reason]),
                   {reply, {error, window_load_error, Reason}, State}
           end
      end.

%% Helper for find_next_matching_loop: searches within the rest of a file's term list
find_first_match_in_list([], FileIdx, State, PredFun, OpType) ->
    % End of list, move to next file
    find_next_matching_loop(FileIdx + 1, 0, State, PredFun, OpType);
find_first_match_in_list([{FoundTermIdx, FoundTerm} | Rest], FileIdx, State, PredFun, OpType) ->
        Pid = element(2,FoundTerm),
        NewActivePids = (State#state.active_pids)#{Pid => {FileIdx,FoundTermIdx}},

        #{Pid := PidCallStack}= CallStack = #state.callstack,
        %% Is Call
        {File, Line, CallStackNew} = case element(3, FoundTerm) of
            %% TODO: do we need Caller argument on calls ?
            
            call -> {_,_,_,{M,F,Args}, {M1,F1,A1, {CallerFile,CallerLine}}} = FoundTerm,
                File1 = proplists:get_value(source, M:module_info(compile), none), %% TODO: should get source file from a dumped file
                case find_clause_line_eval_records:get_matching_clause_line(File, F, Args) of
                    {ok, Line1, Body} -> 
                        %% TODO: Using the succeeding traces to this call trace, we should be able to determine which line number we are at
                        %% by traversing the Body and matching return_from values in clauses or calls in the body 
                        {File1, Line1, CallStack#{Pid => [{FoundTerm, File1, Line1, Body, Body} | PidCallStack]}};
                    _ -> {error, #state.callstack}
                end;
            return_from ->
                %% TODO: Callstack may be a singleton or an empty list, we need to handle those
                [{{_,_,_,{M,F,Args}}, _, _, _, _} = ExitedCall, {_, File1, _Line1, _Body, EvalAST}=ParentCall | RestCallstack] = PidCallStack,
                Value = element(5, FoundTerm),
                %% TODO: if the EvalAST is at a branch, then try to determine where we will jump using the Value from the return_from
                {File1, Line1, #state.callstack};
            %% return_to may jump several places in the callstack (but only if return_from is disabled for those calls)
            %return_to -> {error, #state.callstack};
                %% is send part of the body?
            %send -> {error, #state.callstack};
                 %% is spawn part of the body?
            %spawn -> {error, #state.callstack};
            % spawned -> do this count as a call?
            _ -> [{{_,_,_,{M,F,Args}}, File1, Line1, _, _} = CurrentCall| _RestCallstack] = PidCallStack,
                {File1, Line1, #state.callstack}
        end,
     case PredFun(FoundTerm) of
         true -> % Found it!
             NewState = State#state{current_file_index = FileIdx, current_term_index = FoundTermIdx, callstack = CallStackNew},
             Reply = case OpType of
                         step -> {stepped_to, FoundTerm, File, Line};
                         continue -> {breakpoint_hit, FoundTerm, File, Line};
                         next -> {next_stepped_to, FoundTerm, File, Line}
                     end,
             {reply, Reply, NewState#state{last_reply = Reply, active_pids = NewActivePids}};
         false -> % Didn't match, check next term in this file
              find_first_match_in_list(Rest, FileIdx, State#state{active_pids = NewActivePids}, PredFun, OpType)
     end.


%% Finds the previous term matching PredFun, starting BEFORE the current position.
%% Handles window loading and file transitions.
%% OpType is for creating specific reply tuples.
find_prev_matching(State, PredFun, OpType) ->
    find_prev_matching_loop(State#state.current_file_index, State#state.current_term_index - 1, State, PredFun, OpType).

% **FIX:** Use _Files instead of Files in head
find_prev_matching_loop(FileIdx, TermIdx, State = #state{sorted_files = _Files}, PredFun, OpType) ->
    if FileIdx < 0 -> % Base case: Before the first file
         {reply, start_of_trace, State#state{last_reply = start_of_trace}};
       true ->
          case ensure_window_loaded(FileIdx, State) of
              {ok, StateAfterLoad = #state{loaded_files = Loaded}} ->
                   case maps:get(FileIdx, Loaded, undefined) of
                       undefined when FileIdx < 0 -> % Should be caught by outer if, but safe check
                           {reply, start_of_trace, StateAfterLoad#state{last_reply = start_of_trace}};
                       undefined -> % File index valid but failed to load?
                           error_logger:error_msg("Internal Error: File ~p not loaded in find_prev_matching_loop~n", [FileIdx]),
                           {reply, {error, internal_file_load_issue}, StateAfterLoad};
                       TermsList ->
                           if TermIdx < 0 -> % Need to go to previous file's end
                               PrevFileIdx = FileIdx - 1,
                               find_prev_matching_loop(PrevFileIdx, -2, StateAfterLoad, PredFun, OpType); % Use -2 to signal "find last" in next iteration

                              TermIdx == -2 -> % Signal from previous iteration: find last term of FileIdx
                                   case TermsList of
                                       [] -> find_prev_matching_loop(FileIdx, -1, StateAfterLoad, PredFun, OpType); % Empty file, try previous
                                       _ -> LastTermIdx = element(1, lists:last(TermsList)),
                                            find_prev_matching_loop(FileIdx, LastTermIdx, StateAfterLoad, PredFun, OpType) % Start search from last term
                                   end;
                              true -> % Check term at TermIdx and earlier in current file
                                  % Get terms with index <= TermIdx, check from highest index downwards
                                  
                                  RelevantTerms = lists:filter(fun({Idx, _}) -> Idx =< TermIdx end, TermsList),
                                  find_last_match_in_list(lists:reverse(RelevantTerms), FileIdx, StateAfterLoad, PredFun, OpType)
                           end
                   end;
               {error, Reason} ->
                   error_logger:error_msg("Error ensuring window loaded: ~p~n", [Reason]),
                   {reply, {error, window_load_error, Reason}, State}
           end
      end.

%% Helper for find_prev_matching_loop: searches backward within a list of candidates
find_last_match_in_list([], FileIdx, State, PredFun, OpType) ->
    % End of list for this file reached without match, trigger jump to previous file's end
    find_prev_matching_loop(FileIdx, -1, State, PredFun, OpType);
find_last_match_in_list([{FoundTermIdx, FoundTerm} | RestReversed], FileIdx, State, PredFun, OpType) ->
        Pid = element(2,FoundTerm),
        NewActivePids = (State#state.active_pids)#{Pid => {FileIdx,FoundTermIdx}},
     case PredFun(FoundTerm) of
         true -> % Found it! Check special reverse_step case
             NewState0 = State#state{current_file_index = FileIdx, current_term_index = FoundTermIdx},
             FPid = State#state.focused_pid, % Need focused PID for check
             TPid = get_pid_from_term(FoundTerm), %% TODO is this not already done in PredFun?
             if OpType == reverse_step, element(3, FoundTerm) == spawned, TPid =:= FPid ->
                  {reply, {reverse_stepped_to, FoundTerm}, State};
                true ->
                   Reply = case OpType of
                              reverse_step -> {reverse_stepped_to, FoundTerm};
                              reverse_continue -> {breakpoint_hit, FoundTerm};
                              reverse_next -> {reverse_next_stepped_to, FoundTerm}
                           end,
                   {reply, Reply, NewState0#state{last_reply = Reply, active_pids = NewActivePids}}
             end;
         false -> % Didn't match, check previous term (next in reversed list)
              find_last_match_in_list(RestReversed, FileIdx, State#state{active_pids = NewActivePids}, PredFun, OpType)
     end.


% get_pid_from_term and check_breakpoint_for_pid remain the same
get_pid_from_term({trace, Pid, _, _, _}) -> Pid;
get_pid_from_term({trace, Pid, _, _}) -> Pid;
get_pid_from_term(_) -> undefined.

check_breakpoint_for_pid({trace, Pid, call, {M, F, Args}, _Caller}, Breakpoints, FocusedPid) ->
    _Callstack = [],
    Pid == FocusedPid andalso begin % Must match focused PID
        Arity = length(Args), MFA = {M, F, Arity},
        lists:any(fun({BPidSpec, BMFA, BreakFun}) -> PidMatch = (BPidSpec == 'all') orelse (BPidSpec == Pid), MFAMatch = (BMFA == MFA), PidMatch andalso MFAMatch andalso BreakFun(_Callstack, Args) end, Breakpoints) end;
check_breakpoint_for_pid(_, _, _) -> false.