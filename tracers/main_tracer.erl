-module(main_tracer).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, trace/1, trace_session/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    session,
    dir,
    mode = pass1,
    tracers = #{}
}).
%% TODO 
%% - session should be unique in case we use several main_tracers
%% - integrate with dbg/ttb for node-wide tracing
%% - 
%%% API Functions

%% Starts the MainTracer under the registered name main_tracer.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Alternative start link with options
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

trace_session(Pid) ->
    gen_server:call(Pid, get_session).




%% Public API to send a trace message to the MainTracer.
trace(Msg) ->
    gen_server:cast(?SERVER, {trace_msg, Msg}).

%%% gen_server Callbacks

init(_Args) ->
    message_tracer:start_link(),
    Tracer = spawn(fun F() -> receive M -> trace(M), F() end end),
    Session = trace:session_create(?MODULE, Tracer, []),
    Timestamp = erlang:system_time(millisecond),
        DirName = io_lib:format("main_tracer/~p", [Timestamp]),
        file:make_dir("main_tracer/"),
        ok = file:make_dir(DirName),
        {ok, #state{session = Session, dir = filename:absname(DirName), mode = pass1}}.

handle_call(get_session, _From, State) ->
        {reply, State#state.session, State};
handle_call(stop, _From, State) ->
        trace:session_destroy(State#state.session),
        Tracers = State#state.tracers,
        lists:foreach(fun(Pid) -> process_tracer:stop(Pid) end, maps:values(Tracers)),
        {stop, normal, State};
handle_call(_Request, _From, State) ->
        {reply, ok, State}.


pid_to_filename(Pid) when is_atom(Pid) ->
        PidStr = case whereis(Pid) of
                undefined -> <<"undefined">>;
                ActualPid when is_pid(ActualPid) -> erlang:pid_to_list(ActualPid)
        end,
        CleanStr = string:replace(PidStr, "<", "", all),
        string:replace(CleanStr, ">", "", all);
        

pid_to_filename(Pid) when is_pid(Pid) ->
        PidStr = erlang:pid_to_list(Pid),
        CleanStr = string:replace(PidStr, "<", "", all),
        string:replace(CleanStr, ">", "", all).


handle_cast({trace_msg, Msg}, #state{mode = pass1, dir = Dir} = State) ->
        Pid = element(2, Msg),
        Tracers = State#state.tracers,
        case maps:get(Pid, Tracers, undefined) of
                undefined ->
                        %% TODO: if a new process appears but with the same pid as an old process that has exited, then we
                        %% should get the process info and add a number to the pid {Pid, 1} to be able to distinguish it from the old process
                        %% send will be a little bit trickier, since we need to keep track when the send was made in relation to the
                        %% exit and start of the new process. During the first pass, we are able to make this distinction, and should tag the send
                        %% with the correct TO pid,
                        FileName = filename:join([Dir, pid_to_filename(Pid) ++ ".trace"]),
                        {ok, File} = file:open(FileName, [append]),
                        Term = {Pid, process_info(Pid)},
                        file:write_file(filename:join([Dir, "process_info"], term_to_binary(Term)), [append]),
                        file:write(File, term_to_binary(Msg)),
                        {noreply, State#state{tracers = Tracers#{Pid => File}}};
                File ->
                        %% TODO: if we have received an exit trace, we should stop the ProcessTracer
                        %% and remove it from the tracers map
                        file:write(File, term_to_binary(Msg)),
                        {noreply, State}
        end;
handle_cast({trace_msg, Msg}, #state{mode = postprocess} = State) ->
    %% Look up if a ProcessTracer already exists for Pid
    Pid = element(2, Msg),
    Tracers = State#state.tracers,
    case maps:get(Pid, Tracers, undefined) of
        undefined ->
            %% No ProcessTracer exists; spawn a new one
            %% TODO: process_tracer should be a postprocessing step, instead we should start a separate
            %% tracer? or just output to a separate file, keep track on call chain
            %% disable {return_trace} for this pid on this specific call,
            {ok, ProcTracerPid} = process_tracer:start_link(Pid, State#state.dir),
            NewTracers = Tracers#{Pid => ProcTracerPid},
            %% Forward current trace message to the new ProcessTracer
            process_tracer:trace(ProcTracerPid, {trace, Msg}),
            {noreply, State#state{tracers = NewTracers}};
        ProcTracerPid ->
            %% Reuse existing ProcessTracer and forward the message
            process_tracer:trace(ProcTracerPid, {trace, Msg}),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.