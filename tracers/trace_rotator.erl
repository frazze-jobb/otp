-module(trace_rotator).
-behaviour(gen_server).

%% API
-export([start_link/2, stop/0, trace/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). % Use the module name as the registered name

-record(state, {
    fh :: file:io_device() | undefined, % Current file handle
    base_name :: string(),            % Base filename ("traces")
    index :: non_neg_integer(),       % Current file index (0, 1, 2...)
    count :: non_neg_integer(),       % Messages written to current file
    max_msgs :: pos_integer()         % Max messages per file (10000)
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the trace rotator server.
%% BaseName: The base filename (e.g., "traces").
%% MaxMsgs: The maximum number of trace messages per file.
-spec start_link(BaseName :: string(), MaxMsgs :: pos_integer()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(BaseName, MaxMsgs) when is_list(BaseName), is_integer(MaxMsgs), MaxMsgs > 0 ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [BaseName, MaxMsgs], []).

%% @doc Stops the trace rotator server.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

trace(Msg) ->
    gen_server:cast(?SERVER, {trace_msg, Msg}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([BaseName, MaxMsgs]) ->
    process_flag(trap_exit, true), % Important for clean shutdown via stop/0
    FileName = make_filename(BaseName, 0),
    case file:open(FileName, [write, binary]) of
        {ok, FH} ->
            io:format("Trace rotator started. Writing initial traces to ~s~n", [FileName]),
            State = #state{
                fh = FH,
                base_name = BaseName,
                index = 0,
                count = 0,
                max_msgs = MaxMsgs
            },
            {ok, State};
        {error, Reason} ->
            io:format(standard_error, "Error opening initial trace file ~s: ~p~n", [FileName, Reason]),
            {stop, {cannot_open_file, FileName, Reason}}
    end.

%% @hidden
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}. % No calls expected

%% @hidden
handle_cast({trace_msg, Msg}, State) ->
    handle_trace_message(TraceMsg, State);
handle_cast(_Msg, State) ->
    {noreply, State}. % No casts expected

%% @hidden
terminate(Reason, State = #state{fh = FH}) ->
    io:format("Trace rotator terminating (~p).~n", [Reason]),
    close_file(FH),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Handles writing/rotating for an incoming trace message.
handle_trace_message(TraceMsg, State = #state{fh = FH, count = Count, max_msgs = MaxMsgs}) ->
    NewCount = Count + 1,
    case NewCount > MaxMsgs of
        true ->
            % Time to rotate
            close_file(FH),
            {ok, NewState} = rotate_to_next_file(State),
            ok = write_trace(TraceMsg, NewState#state.fh),
            {noreply, NewState#state{count = 1}}; % Reset count for the new file
        false ->
            % Write to current file
            ok = write_trace(TraceMsg, FH),
            {noreply, State#state{count = NewCount}}
    end.

%% @private Writes a trace message (as binary) to the file handle.
-spec write_trace(TraceMsg :: tuple(), FH :: file:io_device()) -> ok | {error, term()}.
write_trace(TraceMsg, FH) ->
    try term_to_binary(TraceMsg) of
        BinaryMsg -> file:write(FH, BinaryMsg)
    catch
        error:Reason ->
             io:format(standard_error, "Error converting trace msg to binary: ~p~nTraceMsg: ~p~n", [Reason, TraceMsg]),
             {error, Reason} % Decide if you want to stop or just log
    end.


%% @private Closes the current file handle if it's open.
-spec close_file(FH :: file:io_device() | undefined) -> ok.
close_file(undefined) -> ok;
close_file(FH) ->
    io:format("Closing trace file.~n"),
    file:close(FH).

%% @private Rotates to the next file index and opens it.
-spec rotate_to_next_file(State :: #state{}) -> {ok, NewState :: #state{}} | {stop, Reason :: term()}.
rotate_to_next_file(State = #state{base_name = BaseName, index = OldIndex}) ->
    NewIndex = OldIndex + 1,
    NewFileName = make_filename(BaseName, NewIndex),
    io:format("Rotating trace file. Opening ~s~n", [NewFileName]),
    case file:open(NewFileName, [write, binary]) of
        {ok, NewFH} ->
            {ok, State#state{fh = NewFH, index = NewIndex, count = 0}}; % Count will be set to 1 by caller
        {error, Reason} ->
            io:format(standard_error, "Error opening rotated trace file ~s: ~p~n", [NewFileName, Reason]),
            {stop, {cannot_open_rotated_file, NewFileName, Reason}} % Stop the server if rotation fails
    end.

%% @private Generates the filename based on the base name and index.
%% Index 0 -> BaseName
%% Index > 0 -> BaseName.Index
-spec make_filename(BaseName :: string(), Index :: non_neg_integer()) -> string().
make_filename(BaseName, 0) ->
    BaseName;
make_filename(BaseName, Index) when Index > 0 ->
    BaseName ++ "." ++ integer_to_list(Index).