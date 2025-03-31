-module(process_tracer). %% TODO: this should be a post processing step instead
-behaviour(gen_server).

%% API
-export([start_link/3, trace/2, stop/1]).
%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2, handle_call/3, terminate/2, code_change/3]).

-record(state, {
    pid,         %% The traced process' PID
    file,        %% File handle for output
    line = 0,        %% The current line in the file
    dir,
        process_info,
    call_stack = [] %% Stack to accumulate nested call events
}).
-define(UNDEF, <<"undefined">>).



%%% API Functions

%% Starts a ProcessTracer for a given traced process (Pid)
start_link(TracedPid, Dir, ProcessInfo) ->
    gen_server:start_link(?MODULE, [TracedPid, Dir, ProcessInfo], []).

trace(ProcessTracer, Message) ->
        gen_server:cast(ProcessTracer, Message).

stop(ProcessTracer) ->
        gen_server:cast(ProcessTracer, stop).
%%% gen_server Callbacks

init([TracedPid, Dir, ProcessInfo]) ->
    %% Convert Pid to a filename-friendly string and open a trace file (in append mode)
    FileName = filename:join([Dir, pid_to_filename(TracedPid) ++ ".json"]),
    {ok, File} = file:open(FileName, [write]),
    {ok, #state{pid = TracedPid, file = File, dir = filename:absname(Dir), process_info=ProcessInfo}}.

handle_cast({trace, Msg}, #state{line = Line} = State) ->
    %% Process and translate the trace message to JSON-like format.
    {NewState, JsonStr} = process_trace(Msg, State),
    Line1 = Line + length(string:split(JsonStr, "\n", all))-1,
    file:write(State#state.file, JsonStr),
    {noreply, NewState#state{line = Line1}};
handle_cast(stop, State) ->
    erlang:display(stop),
    {NewState, JsonStr} = process_trace(stop,State),
    file:write(State#state.file, JsonStr),
    file:close(State#state.file),
    {stop, normal, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    file:close(State#state.file),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Helper Functions

%% pid_to_filename/1
%% Converts an Erlang PID into a string suitable for a filename.
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

we2bin(P)->
        list_to_binary(io_lib:format("~p",[P])).

%% process_trace/2
process_trace(stop, State) ->
        #state{call_stack = CallStack} = State,
        C = length(CallStack),
        Close = lists:flatten([indent(L)++"  ]"++"\n"++indent(L)++"}" || L <- lists:seq(C,1,-1)]),
        Json = list_to_binary(Close),
        {State#state{call_stack = []}, Json};
%% Pattern-matches on the incoming trace message and returns an updated state plus a JSON string.
process_trace({trace, _Pid, send, Msg, To}, #state{line = Line, dir = Dir, call_stack = CallStack} = State) ->
    %% Send messages: include a link to the PID-specific trace file.
    message_tracer:create_sender_link({trace, _Pid, send, Msg, To}, Line),
    Link = list_to_binary("file://" ++Dir++"/"++ pid_to_filename(To) ++ ".json"), %% We can't put a line number, we don't know if the receiver will handle the message
    Send = #{send => we2bin(To), message => we2bin(Msg), link => Link},
    #{send := Receiver, message := Message, link := Link1} = Send,
    Indent = indent(State),
    Json = list_to_binary(io_lib:format("~s{\"send\": \"~p\",~s  \"message\": \"~p\",~s  \"link\": \"~s\"},",
    [Indent, To, Indent, Msg, Indent, Link1])),
    {State#state{call_stack = CallStack}, Json};

process_trace({trace, _Pid, send_to_non_existing_process, Msg, To}, #state{call_stack = CallStack} = State) ->
    %% Send to non-existing process messages.
    Send = #{send => we2bin(To), message => we2bin(Msg), link => ?UNDEF},
    #{send := Receiver, message := Message, link := Link1} = Send,
    Indent = indent(State),
    Json = list_to_binary(io_lib:format("~s{\"send\": \"~p\",~s  \"message\": \"~p\",~s  \"link\": \"~s\"},",
    [Indent, To, Indent, Msg, Indent, Link1])),
      {State#state{call_stack = CallStack}, Json};

process_trace({trace, _Pid, 'receive', Msg}, #state{dir = Dir, call_stack = CallStack} = State) ->
    %% Receive messages: include a link back to sender's trace.
    case message_tracer:get_sender_link({trace, _Pid, 'receive', Msg}) of
        {unknown, undefined} -> Recv = #{'receive' => we2bin(unknown), message => we2bin(Msg), link => ?UNDEF};
        {SenderPid, Line} -> Link = list_to_binary("file://" ++ Dir ++ "/" ++ pid_to_filename(SenderPid) ++ ".json#" ++ integer_to_list(Line)),
                Recv = #{'receive' => we2bin(SenderPid), message => we2bin(Msg), link => Link}
    end,
    #{'receive' := Sender, message := Message, link := Link1} = Recv,
    Indent = indent(State),
    Json = list_to_binary(io_lib:format("~s{\"receive\": \"~s\",~s  \"message\": \"~p\",~s  \"link\": \"~s\"},",
        [Indent, Sender, Indent, Msg, Indent, Link1])),
    {State#state{call_stack = CallStack}, Json};


process_trace({trace, _Pid, call, {M, F, Args}}, State) ->
        process_trace({trace, _Pid, call, {M, F, Args}, undefined}, State);
process_trace({trace, _Pid, call, {M, F, Args}, Caller}, State) ->
    %% TODO: detect in the call stack if this m:f/a == earlier m:f/a, its probably an idefinite loop
    %% -> take the session and call trace:function(sesssion, {m,f,a}, {'_', [], [{message, caller_line}]})
    %% For function calls, push a new call event on the call stack.
    MFA = {M,F,A} = {M, F, length(Args)},
    CallEvent = #{call => MFA,
                  args => Args,
                  caller => Caller,
                  return => undefined},
    Indent = indent(State),
    NewStack = [CallEvent | State#state.call_stack],
    NewState = State#state{call_stack = NewStack},
    Caller1 = case Caller of
        {M1, _, _, {_, Line}} -> 
                case proplists:get_value(source, M1:module_info(compile), undefined) of
                        undefined -> ?UNDEF;
                        File -> list_to_binary("file://" ++ File ++ "#" ++ integer_to_list(Line))
                end;
        {M1, _, _, undefined} -> 
                case code:which(M1) of
                        {ok, BeamFile} ->
                                {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(BeamFile, [abstract_code]),
                                FileLine = case lists:keyfind({F, A}, 1, erl_syntax_lib:analyze_forms(AC)) of
                                        {_, _, {file, File2}, Line2} -> {File2, Line2};
                                        _ -> {undefined, undefined}
                                end,
                                case FileLine of
                                        {undefined, undefined} -> ?UNDEF;
                                        {File1, Line1} -> list_to_binary("file://" ++ File1 ++ "#" ++ integer_to_list(Line1))
                                end;
                        preloaded -> we2bin(preloaded)
                end;
        undefined -> ?UNDEF
    end,
    %% Output half completed json
    Json = list_to_binary(io_lib:format("~s{\"call\": \"~p:~p/~p\",~s  \"args\": \"~p\",~s  \"file\": \"~s\",~s  \"body\": [",
      [Indent, M,F,A, Indent, Args, Indent, Caller1, Indent])),
    {NewState, Json};


%% If return trace is enabled for functions in the call stack, then return_to is sent from some returns
%% 
process_trace({trace, _Pid, return_to, {_M, _F, _Arity}=MFA0}, #state{call_stack = CallStack} = State) ->
        %% TODO: this is sent once per tailcall chain, if return_trace is enabled tailcalls are eliminated
        %% 
        %% TODO: may jump several calls in the stack, identify closest M, F, Arity
        %% in the call stack, store every call event up to  them in the body of the parent
        {NewCallStack, Output} = fold_call_stack(MFA0, CallStack, []),
        {State#state{call_stack = NewCallStack}, Output};
process_trace({trace, _Pid, return_from, {M, F, Arity}, ReturnValue}, State) ->
    %% End the most recent call event and log its complete JSON.
    case State#state.call_stack of
        [_ | Rest] ->
            %CompletedCall = CallEvent#{return => ReturnValue},
            NewState = State#state{call_stack = Rest},
            Indent = indent(length(Rest)),
            Json = list_to_binary(io_lib:format("~s  ],~s  \"return\": \"~p\"~s},",[Indent, Indent, ReturnValue, Indent])),
            {NewState,Json};
        [] ->
            %% No active call event: log as a standalone return_from.
            Json = io_lib:format(
              "\n{\"return_from\":\"~p\",\n  \"value\":\"~p\"},",
              [{M, F, Arity}, ReturnValue]),
            {State, lists:flatten(Json)}
    end;

process_trace({trace, _Pid, exception_from, {M, F, Arity}, {Class, Value}}, State) ->
    %% Exception messages from function calls.
    case State#state.call_stack of
    [_ | Rest] ->
        %CompletedCall = CallEvent#{return => ReturnValue},
        NewState = State#state{call_stack = Rest},
        Indent = indent(length(Rest)),
        Json = list_to_binary(io_lib:format("~s  ],~s  \"exception\": \"~p\"~s},",[Indent, Indent, {Class, Value}, Indent])),
        {NewState,Json};
    [] ->
        %% No active call event: log as a standalone return_from.
        Json = io_lib:format(
            "\n{\"exception_from\":\"~p\",\n  \"value\":\"{\"exception\": {\"class\": \"~p\", \"value\": \"~p\"}}\"",
                    [{M, F, Arity},Class, Value]),
        {State, lists:flatten(Json)}
    end;

process_trace({trace, _Pid, spawn, Pid2, {M, F, Args}}, State) ->
    %% Process creation (spawn) events.
    Indent = indent(State),
    Json = io_lib:format(
      "~s{\"spawn\": \"~p\",~s\"fun\": {\"call\": \"~p:~p/~p\", \"args\": \"~p\"},~s\"link\": \"file://~s/~s.json\"}",
      [Indent, Pid2, Indent, M, F, length(Args), Args, Indent, State#state.dir, pid_to_filename(Pid2)]),
    {State, lists:flatten(Json)};

process_trace({trace, _Pid, spawned, Pid2, {M, F, Args}}, State) ->
    %% Spawned event indicating which process spawned the current PID.
    Indent = indent(State),
    Json = io_lib:format(
      "{\"spawned\": \"~p\",~s\"fun\": {\"call\": \"~p:~p/~p\", \"args\": \"~p\"},~s\"link\": \"file://~s/~s.json\"}",
      [Pid2, Indent, M, F, length(Args), Args, Indent, State#state.dir, pid_to_filename(Pid2)]),
    {State, lists:flatten(Json)};

process_trace({trace, _Pid, exit, Reason}, State) ->
    %% Process exit events.
    Json = list_to_binary(io_lib:format(
      "~s{\"exit\": \"~p\"}",
      [indent(State), Reason])),
    {State1, Json1} = process_trace(stop, State),
    {State1, <<Json/binary,Json1/binary>>};

% process_trace({trace, _Pid, register, RegName}, State) ->
%     %% Registration events; also (conceptually) create a symbolic link.
%     %Json = io_lib:format(
%    %   "{\"register\": \"~p\", \"comment\": \"Symbolic link created: ~p_~s.trace\"}",
%    %   [RegName, RegName, pid_to_filename(Pid)]),
%     {State, []};

% process_trace({trace, _Pid, unregister, RegName}, State) ->
%     %% Unregistration events.
%     Json = io_lib:format(
%       "{\"unregister\": \"~p\"}",
%       [RegName]),
%     {State, lists:flatten(Json)};

% %% Linking events (link, unlink, getting_linked, getting_unlinked)
process_trace({trace, _Pid, LinkEvent, Pid2}, State)
  when LinkEvent == link; LinkEvent == unlink;
       LinkEvent == getting_linked; LinkEvent == getting_unlinked ->
    Key = atom_to_list(LinkEvent),
    Json = io_lib:format(
      "{\"~s\": \"~p\"}",
      [Key, Pid2]),
    {State, lists:flatten(Json)};

%% Catch-all clause for unknown trace messages.
process_trace(Other, State) ->
    Json = io_lib:format(
      "{\"unknown_trace\": \"~p\"}",
      [Other]),
    {State, lists:flatten(Json)}.


indent(Callstack) when is_list(Callstack) ->
    "\n"++string:copies("  ",length(Callstack)*2);
indent(Length) when is_integer(Length) ->
    "\n"++string:copies("  ",Length*2);
indent(State) ->
    "\n"++string:copies("  ",length(State#state.call_stack)*2).

fold_call_stack(MFA, [#{call := MFA} | _]=CallStack1, Acc) ->
    C = length(CallStack1),
    Close = lists:flatten([indent(C+L)++"  ]"++"\n"++indent(C+L)++"}" || L <- lists:seq(length(Acc),1,-1)]),
    Json = list_to_binary(Close),
    {CallStack1, Json};
fold_call_stack(MFA, [CallEvent1| CallStack1], Acc) ->
    fold_call_stack(MFA, CallStack1, [CallEvent1|Acc]);
fold_call_stack(_, [], Acc) ->
    Close = lists:flatten([indent(L)++"  ]"++"\n"++indent(L)++"}" || L <- lists:seq(length(Acc),1,-1)]),
    Json = list_to_binary(Close),
    {[], Json}.
%% call_event_to_json/1
%% Helper to convert a completed call event (a map) into a JSON-like string.
call_event_to_json(#{call:=CallStr, body:=Body, args:= Args, return := ReturnVal}) ->
    io_lib:format(
      "{\"call\": \"~s\", \"args\": \"~p\", \"body\": \"~p\", \"return\": \"~p\"}",
      [CallStr, Args, Body, ReturnVal]).