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
    Tracer = spawn(fun F() -> receive M -> trace(M), F() end end),
    Session = trace:session_create(?MODULE, Tracer, []),
    Timestamp = erlang:system_time(millisecond),
        DirName = io_lib:format("main_tracer/~p", [Timestamp]),
        file:make_dir("main_tracer/"),
        ok = file:make_dir(DirName),
        {ok, #state{session = Session, dir = DirName}}.

handle_call(get_session, _From, State) ->
        {reply, State#state.session, State};
handle_call(stop, _From, State) ->
        trace:session_destroy(State#state.session),
        Tracers = State#state.tracers,
        lists:foreach(fun(Pid) -> process_tracer:stop(Pid) end, maps:values(Tracers)),
        {stop, normal, State};
handle_call(_Request, _From, State) ->
        {reply, ok, State}.
        
handle_cast({trace_msg, Msg}, State) ->
    %% Look up if a ProcessTracer already exists for Pid
    Pid = element(2, Msg),
    case element(3, Msg) of
        MPOP when MPOP =:= send; MPOP =:= send_to_non_existing_process, MPOP =:= 'receive' ->
                %% TODO: send to a message passing tracing process
                ok;
        _ -> ok
    end,
    Tracers = State#state.tracers,
    case maps:get(Pid, Tracers, undefined) of
        undefined ->
            %% No ProcessTracer exists; spawn a new one
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