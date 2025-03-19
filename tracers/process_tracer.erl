-module(process_tracer).
-behaviour(gen_server).

%% API
-export([start_link/2, trace/2]).
%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2, handle_call/3, terminate/2, code_change/3]).

-record(state, {
    pid,         %% The traced process' PID
    file,        %% File handle for output
    dir,
    call_stack = [] %% Stack to accumulate nested call events
}).
-define(UNDEF, <<"undefined">>).
%%% API Functions

%% Starts a ProcessTracer for a given traced process (Pid)
start_link(TracedPid, Dir) ->
    gen_server:start_link(?MODULE, [TracedPid, Dir], []).

trace(ProcessTracer, Message) ->
        gen_server:cast(ProcessTracer, Message).
%%% gen_server Callbacks

init([TracedPid, Dir]) ->
    %% Convert Pid to a filename-friendly string and open a trace file (in append mode)
    FileName = filename:join([Dir, pid_to_filename(TracedPid) ++ ".trace"]),
    {ok, File} = file:open(FileName, [append]),
    {ok, #state{pid = TracedPid, file = File, dir = Dir}}.

handle_cast({trace, Msg}, State) ->
    %% Process and translate the trace message to JSON-like format.
    {NewState, JsonStr} = process_trace(Msg, State),
    file:write(State#state.file, JsonStr),
    {noreply, NewState};
handle_cast(stop, State) ->
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
pid_to_filename(Pid) ->
    PidStr = erlang:pid_to_list(Pid),
    CleanStr = string:replace(PidStr, "<", "", all),
    string:replace(CleanStr, ">", "", all).

we2bin(P)->
        list_to_binary(io_lib:format("~p",[P])).
%% process_trace/2
process_trace(stop, State) ->
        #state{call_stack = CallStack} = State,
        C = length(CallStack),
        Close = lists:flatten(lists:reverse(["\n"++string:copies("  ", L*2+1)++"]"++"\n"++string:copies("  ", L*2)++"}" || L <- lists:seq(1,C)])),
        Json = list_to_binary(Close),
        {State#state{call_stack = []}, Json};
%% Pattern-matches on the incoming trace message and returns an updated state plus a JSON string.
process_trace({trace, _Pid, send, Msg, To}, #state{dir = Dir, call_stack = [#{body:=Body} = CallEvent| CallStack]} = State) ->
    %% Send messages: include a link to the PID-specific trace file.
    %% TODO: Add a timestamp, send this to a message_handler process who receivers can call to get the sender
    Send = #{send => we2bin(To), message => we2bin(Msg), link => [we2bin(Dir),we2bin(To)]},
    CallEvent1 = CallEvent#{body := [Send | Body]},
    Json = json:encode(Send),
    {State#state{call_stack = [CallEvent1 | CallStack]}, Json};

process_trace({trace, _Pid, send_to_non_existing_process, Msg, To}, #state{call_stack = [#{body:=Body} = CallEvent| CallStack]} = State) ->
    %% Send to non-existing process messages.
    Send = #{send => we2bin(To), message => we2bin(Msg), link => ?UNDEF},
      CallEvent1 = CallEvent#{body := [Send | Body]},
      Json = json:encode(Send),
      {State#state{call_stack = [CallEvent1 | CallStack]}, Json};

process_trace({trace, _Pid, 'receive', Msg}, #state{call_stack = []} = State) ->
        Recv = #{'receive' => ?UNDEF, message => we2bin(Msg), link => ?UNDEF},
        Json = json:encode(Recv),
        {State, Json};
process_trace({trace, _Pid, 'receive', Msg}, #state{call_stack = [#{body:=Body} = CallEvent| CallStack]} = State) ->
    %% Receive messages: include a comment about linking back to sender's trace.
    %% TODO: Should be possible to ask other process_tracers who sent this
    %%  id:ing it using the pid, message, and a timestamp?
    Recv = #{'receive' => ?UNDEF, message => we2bin(Msg), link => ?UNDEF},
    CallEvent1 = CallEvent#{body := [Recv| Body]},
    Json = json:encode(Recv),
    {State#state{call_stack = [CallEvent1 | CallStack]}, Json};

process_trace({trace, _Pid, call, {M, F, Args}, Caller}, State) ->
    %% TODO: detect in the call stack if this m:f/a == earlier m:f/a, its probably an idefinite loop
    %% -> take the session and call trace:function(sesssion, {m,f,a}, {'_', [], [{message, caller_line}]})
    %% For function calls, push a new call event on the call stack.
    MFA = {M,F,A} = {M, F, length(Args)},
    CallEvent = #{call => MFA,
                  args => Args,
                  body => [],
                  caller => Caller,
                  return => undefined},
    Indent = "\n"++string:copies("  ",length(State#state.call_stack)*2),
    NewStack = [CallEvent | State#state.call_stack],
    NewState = State#state{call_stack = NewStack},
    Caller1 = case Caller of
        {M1, _, _, {_, Line}} -> 
                case proplists:get_value(source, M1:module_info(compile), undefined) of
                        undefined -> ?UNDEF;
                        File -> we2bin("file://" ++ File ++ "#" ++ integer_to_list(Line))
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
                                        {File1, Line1} -> we2bin("file://" ++ File1 ++ "#" ++ integer_to_list(Line1))
                                end;
                        preloaded -> we2bin("preloaded")
                end;
        undefined -> ?UNDEF
    end,
    %% Output half completed json
    io_lib:format("~p",[Args]),
    Json = list_to_binary(io_lib:format("~s{\"call\": \"~p:~p/~p\",~s  \"args\": ~p,~s  \"file\": ~s,~s  \"body\": [",
      [Indent, M,F,A, Indent, Args, Indent, Caller1, Indent])),
    {NewState, Json};

%% If return trace is enabled for functions in the call stack, then return_to is sent from some returns
%% 
process_trace({trace, _Pid, return_to, {_M, _F, _Arity}=MFA0}, #state{call_stack = CallStack} = State) ->
        %% TODO: this is sent once per tailcall chain, if return_trace is enabled tailcalls are eliminated
        %% 
        %% TODO: may jump several calls in the stack, identify closest M, F, Arity
        %% in the call stack, store every call event up to  them in the body of the parent
        Ffold_call_stack = fun F(MFA, [#{call := MFA} | _]=CallStack1, Acc) ->
                        C = length(CallStack1),
                        Close = lists:flatten(lists:reverse(["\n"++string:copies("  ", (C+L)*2+1)++"]"++"\n"++string:copies("  ", (C+L)*2)++"}" || L <- lists:seq(1,length(Acc))])),
                        Json = list_to_binary(Close),
                        {CallStack1, Json};
                F(MFA, [CallEvent1| CallStack1], Acc) ->
                        F(MFA, CallStack1, [CallEvent1|Acc]);
                F(_, [], Acc) ->
                        Close = lists:flatten(lists:reverse(["\n"++string:copies("  ", L*2+1)++"]"++"\n"++string:copies("  ", L*2)++"}" || L <- lists:seq(1,length(Acc))])),
                        Json = list_to_binary(Close),
                        {[], Json} 
        end,
        {NewCallStack, Output} = Ffold_call_stack(MFA0, CallStack, []),
        {State#state{call_stack = NewCallStack}, Output};
process_trace({trace, _Pid, return_from, {M, F, Arity}, ReturnValue}, State) ->
    %% End the most recent call event and log its complete JSON.
    case State#state.call_stack of
        [_ | Rest] ->
            %CompletedCall = CallEvent#{return => ReturnValue},
            NewState = State#state{call_stack = Rest},
            Indent = "\n"++string:copies("  ",length(Rest)*2),
            Json = list_to_binary(io_lib:format("~s  ],~s  \"return\": \"~p\"~s}",[Indent, Indent, ReturnValue, Indent])),
            {NewState,Json};
        [] ->
            %% No active call event: log as a standalone return_from.
            Json = io_lib:format(
              "\n{\"return_from\":\"~p\",\n  \"value\":\"~p\"}",
              [{M, F, Arity}, ReturnValue]),
            {State, lists:flatten(Json)}
    end;

% process_trace({trace, _Pid, exception_from, {M, F, Arity}, {Class, Value}}, State) ->
%     %% Exception messages from function calls.
%     Json = io_lib:format(
%       "{\"exception\": {\"class\": \"~p\", \"value\": \"~p\"}}",
%       [Class, Value]),
%     {State, lists:flatten(Json)};

% process_trace({trace, _Pid, spawn, Pid2, {M, F, Args}}, State) ->
%     %% Process creation (spawn) events.
%     Json = io_lib:format(
%       "{\"spawn\": \"~p\", \"fun\": {\"call\": \"~p:~p/~p\", \"args\": \"~p\"}, \"comment\": \"See file:///~s.trace\"}",
%       [Pid2, M, F, length(Args), Args, pid_to_filename(Pid2)]),
%     {State, lists:flatten(Json)};

% process_trace({trace, _Pid, spawned, Pid2, {M, F, Args}}, State) ->
%     %% Spawned event indicating which process spawned the current PID.
%     Json = io_lib:format(
%       "{\"spawned\": \"~p\", \"fun\": {\"call\": \"~p:~p/~p\", \"args\": \"~p\"}, \"comment\": \"Spawned by process trace.\"}",
%       [Pid2, M, F, length(Args), Args]),
%     {State, lists:flatten(Json)};

% process_trace({trace, _Pid, exit, Reason}, State) ->
%     %% Process exit events.
%     Json = io_lib:format(
%       "{\"exit\": \"~p\"}",
%       [Reason]),
%     {State, lists:flatten(Json)};

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
% process_trace({trace, _Pid, LinkEvent, Pid2}, State)
%   when LinkEvent == link; LinkEvent == unlink;
%        LinkEvent == getting_linked; LinkEvent == getting_unlinked ->
%     Key = atom_to_list(LinkEvent),
%     Json = io_lib:format(
%       "{\"~s\": \"~p\"}",
%       [Key, Pid2]),
%     {State, lists:flatten(Json)};

%% Catch-all clause for unknown trace messages.
process_trace(Other, State) ->
    Json = io_lib:format(
      "{\"unknown_trace\": \"~p\"}",
      [Other]),
    {State, lists:flatten(Json)}.

%% call_event_to_json/1
%% Helper to convert a completed call event (a map) into a JSON-like string.
call_event_to_json(#{call:=CallStr, body:=Body, args:= Args, return := ReturnVal}) ->
    io_lib:format(
      "{\"call\": \"~s\", \"args\": \"~p\", \"body\": \"~p\", \"return\": \"~p\"}",
      [CallStr, Args, Body, ReturnVal]).