-module(main_tracer).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, trace/1, trace_session/1, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    session,
    dir,
    tracer_rotator,
    pid_callstacks = #{}
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
stop(Pid) ->
    gen_server:call(Pid, stop).



%% Public API to send a trace message to the MainTracer.
trace(Msg) ->
    gen_server:cast(?SERVER, {trace_msg, Msg}).

%%% gen_server Callbacks

init(_Args) ->
    message_tracer:start_link(),
    Tracer = spawn(fun F() -> receive M -> trace(M), F() end end),
    TraceRotator = trace_rotator:start_link("traces", 10000),
    Session = trace:session_create(?MODULE, Tracer, []),
    Timestamp = erlang:system_time(millisecond),
    DirName = io_lib:format("main_tracer/~p", [Timestamp]),
    file:make_dir("main_tracer/"),
    ok = file:make_dir(DirName),
    os:cmd("ln -snf "++ filename:absname(DirName) ++ " " ++ filename:absname("main_tracer/latest")),
    %% Any process_info or module_info after this point is tracked by reading the traces
    file:write_file(filename:join([DirName, "process_info"]), term_to_binary(collect_active_processes()), [binary]),
    file:write_file(filename:join([DirName, "module_info"]), term_to_binary(collect_loaded_modules()), [binary]),
    {ok, #state{session = Session, trace_rotator = TraceRotator, dir = filename:absname(DirName), mode = pass1}}.

%% @private Collects loaded module info into a list.
collect_loaded_modules() ->
    Loaded = code:all_loaded(),
    lists:map(
      fun({Module, BeamPath}) ->
              SourcePath = get_source_path(Module),
              #{module => Module, beam_path => BeamPath, source_path => SourcePath}
      end, Loaded).

%% @private Gets the source file path from module compile info.
get_source_path(Module) ->
    try
        case Module:module_info(compile) of
            undefined -> undefined;
            CompileInfo ->
                case proplists:get_value(source, CompileInfo) of
                    undefined -> undefined;
                    Source -> Source
                end
        end
    catch
        _:_ -> undefined
    end.

%% @private Collects active process info into a list.
collect_active_processes() ->
    Pids = erlang:processes(),
    % Use foldl to build the list, skipping dead processes
    lists:foldl(
      fun(Pid, Acc) ->
              try
                  % Specify desired fields
                  Keys = [dictionary,messages,parent,links,registered_name,current_stacktrace],
                %[registered_name, initial_call, current_function, message_queue_len, total_heap_size, links, monitors],
                  case erlang:process_info(Pid, Keys) of
                      undefined -> % Process died between processes() and process_info()
                         Acc;
                      InfoList ->
                         InfoMap = maps:from_list(InfoList),
                         % Add pid itself to the map for clarity
                         ProcessData = InfoMap#{pid => Pid},
                         [ProcessData | Acc] % Prepend to accumulator
                  end
              catch
                  error:badarg -> % Process died (alternative way it fails)
                      Acc;
                  Type:Reason:Stacktrace -> % Log unexpected errors but continue
                      io:format(standard_error, "Error getting process info for ~p: ~p:~p~nStacktrace: ~p~n", [Pid, Type, Reason, Stacktrace]),
                      Acc
              end
      end, [], Pids).

handle_call(get_session, _From, State) ->
        {reply, State#state.session, State};
handle_call(stop, _From, State) ->
        trace:session_destroy(State#state.session),
        Tracers = State#state.tracers,
        trace_rotator:stop(),
        {stop, normal, State};
handle_call(_Request, _From, State) ->
        {reply, ok, State}.

%% TODO: keep track on the call stack for each pid, if the same function exists in the same callstack more than 16 times
%% then we want to disable return_trace from this particular function and output to a file that this function probably never returns
%% some functions are actually tail recursive, and do return, but in those cases we just disable it anyways, to not run into
%% out of stack issues, recursing over large data. In many cases you can derive the actual return value, by looking in the AST of the caller
%% hopefully the value is used somehow in a succeeding call, or return.
%% These functions that recurse over large data, producing several call traces, but only returns a return_to, should be possible
%% to fold those so that stepping over them is easier
handle_cast({trace_msg, Msg}, State) ->
        trace_rotator:trace(Msg),
        {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.