-module(message_tracer).

-behaviour(gen_server).

%% API
-export([start_link/0, create_sender_link/2, get_sender_link/1, register_message_queue/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {untraced_sends = #{}, trace_map = #{}}).

%% API Functions
start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_sender_link(TraceMsg, Line) ->
        gen_server:cast(?MODULE, {create_sender_link, TraceMsg, Line}).

get_sender_link(TraceMsg) ->
        gen_server:call(?MODULE, {get_sender_link, TraceMsg}).
%% The message queue length, actually is not of interest, the actual messages are
%% if two processes sends the same message, at roughly the same time we will not
%% be able to distinguish them, unless we use 1 trace file to actually see the order
%% of things happening..
%% 
register_message_queue(Pid, ProcessInfo) ->
        MsgQLen = proplists:get_value(message_queue_len, ProcessInfo),
        gen_server:cast(?MODULE, {register_message_queue, Pid, MsgQLen}).

%% gen_server Callbacks
init([]) ->
        {ok, #state{}}.


%% NOTE! This is solution is not entirely robust, since it doesnt take into account priority messages
%% 
%% When we receive the receive trace, we create a link from the receiver to the sender, and we have the line number stored
%% NOTE! there might be a race here, that the receive is handled before create_sender_link happens we should wait, but only if we know that the sender is being traced
%% ? Or that the sender is not being traced..
%% 
handle_call({get_sender_link, {trace, ReceiverPid, 'receive', Msg}}, _From, #state{untraced_sends = UntracedSendsMap, trace_map = TraceMap} = State) ->
        case UntracedSendsMap of
                #{ReceiverPid := 0} -> 
                        {SenderPid, Count, [Line|Lines]} = maps:get({ReceiverPid, Msg}, TraceMap, {unknown, 0, [undefined]}),
                        case Line of
                                undefined ->
                                        %% We have not yet received the send trace or it happened before we started tracing
                                        %% TODO: can we fix this by post processing somehow?
                                        {reply, {unknown, undefined}, State};
                                _ ->
                                        %% We have received the send trace
                                        NewMap = TraceMap#{{ReceiverPid, Msg} => {SenderPid, max(0,Count-1), Lines}},
                                        {reply, {SenderPid, Line}, State#state{trace_map = NewMap}}
                        end;
                #{ReceiverPid := Len} ->
                        {reply, {unknown, undefined}, State#state{untraced_sends = UntracedSendsMap#{ReceiverPid := max(0,Len-1)}}}
        end;
handle_cast({wait_sender_receiver_link, Pid, send, TraceMsg, Line}, State) ->
        %% A receive trace actually happens almost instantly as the send was made,
        %% This means that it doesnt matter whether the receiving process is in a selective receive or not, a link from the sender to a line number is not really interesting,
        %% since we dont know when the message is actually going to be handled... 
        %% However, it can be interesting to know who sent the message from the receiver point of view, and link to the sender with line number
        {noreply, State};
handle_call(_Request, _From, State) ->
        {reply, ok, State}.

%% When we receive the send trace, we create a link from the sender to the receiver, we don't yet have the line number
%% where we get the receive. That has to be done in Postprocessing
handle_cast({create_sender_link, {trace, SenderPid, send, Msg, ReceiverPid}, Line}, #state{trace_map = TraceMap} = State) ->
        {SenderPid, Count, Lines} = maps:get({ReceiverPid, Msg}, TraceMap, {SenderPid, 0, []}),
        NewMap = TraceMap#{{ReceiverPid, Msg} => {SenderPid, Count, Lines++[Line]}},
        {noreply, State#state{trace_map = NewMap}};
handle_cast({register_message_queue, Pid, MessageQueueLen}, #state{untraced_sends = UntracedSendsMap} = State) ->
        {noreply, State#state{untraced_sends = UntracedSendsMap#{Pid => MessageQueueLen}}};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.