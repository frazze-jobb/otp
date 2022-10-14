%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%% Purpose: Profile a system in order to figure out where the 
%% memory goes.
%%

-module(mana).
-behaviour(gen_server).

-export([start/0, stop/0,
    memory_trace/1, memory_status/0,
    test_process/0]).

%% Internal exports 
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-record(state, {
    process :: pid(),
    sent = 0 :: non_neg_integer(),
    received = 0 :: non_neg_integer(),
    unhandled = 0 :: non_neg_integer(),
    message_water_mark = {0, []},
    high_water_mark = {0, []},
    stack_trace_water_mark = {0, []}
}).
%% This is a gen_server that collects traces for a process
%% API
%% enable memory tracing for a process (keep track on how much is received and sent, and still in the list)
%% enable garbage collection monitoring and keep track on highest watermark
%% enable call trace memory profiling (hprof by max federov)
memory_trace(Pid) ->
    %% start the gen server?
    %% handle call % who will start the trace server and receive trace messages
    %%
    %stop(),
    GSPid = start(),
    Res = gen_server:call(?MODULE, {memory_trace, Pid}, infinity),
    {Res, GSPid}.

memory_status() ->
    gen_server:call(?MODULE, memory_status, infinity).

-spec start() -> {'ok', Pid} | {'error', Reason} when
    Pid :: pid(),
    Reason :: {'already_started', Pid}.
start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> 'stopped'.
stop() -> gen_server:call(?MODULE, stop, infinity).

init([]) ->
    {ok, #state{}}.


test_process() ->
    spawn(
        fun F() ->
            receive
                exec -> lists:seq(1, 100000), F();
                read -> receive
                        _ -> F()
                    end;
                stop -> stop
            end
        end).

handle_call({memory_trace, Pid}, _, #state{} = S) ->
    Self = self(),
    F = fun({_,P,E,T}, State) ->
        Self ! {State, {trace, P, E, T, erts_debug:size(T)}},
        State;
        ({_,P,E,T,R}, State) ->
            Self ! {State, {trace, P, E, T, R, erts_debug:size(T)}},
        State
    end,
    erlang:display("Memory trace activiated"),
    Tracer = dbg:tracer(process, {F, Self}),
    case Tracer of
        {error, already_started} -> {stop, "Tracer already started"};
        _ ->
            dbg:p(Pid, ['receive', send, garbage_collection]),
            {reply, ok, S#state{process = Pid}}
    end;
handle_call(memory_status, _, S) ->
    {reply, {status, S}, S};

handle_call(stop, _, S) ->
    dbg:stop_clear(),
    {reply, stopped, S}.
handle_cast(TraceCast, S) ->
    erlang:display("TraceCast"),
    erlang:display(TraceCast),
    {noreply, S}.

handle_info({_,{trace, _Pid, 'receive', Data, Size}}, #state{unhandled = Unhandled} = S) ->
    case Unhandled + Size > element(1, S#state.message_water_mark) of
        true ->
            S1 = S#state{unhandled = Unhandled + Size},
            {noreply, S1#state{message_water_mark = {Unhandled+Size, 
                erlang:process_info(S#state.process, messages), {last, Data}}}};
        false ->
            {noreply, S}
    end;
handle_info({_,{trace, _Pid, 'received', _Data, Size}}, #state{received = Received, unhandled = Unhandled} = S) ->
    {noreply, S#state{received = Received + Size, unhandled = max(Unhandled - Size, 0)}};
handle_info({_,{trace, _Pid, 'send', _Data, _Receiver, Size}}, #state{sent = Sent} = S) ->
    {noreply, S#state{sent = Sent + Size}};
handle_info({_, {trace, _Pid, gc_minor_start, Data, _Size}}, S) ->
    TotalSize = proplists:get_value(heap_block_size, Data) +
        proplists:get_value(old_heap_block_size, Data) +
        proplists:get_value(mbuf_size, Data),
    DataPruned = prune(Data),
    [H|_StackFramesPruned] = proplists:get_value(stack_frames, DataPruned),
    S_StackFrame = case H of
            {{_M, _F, _A}, Sz} when Sz > element(1, S#state.stack_trace_water_mark) ->
                S#state{stack_trace_water_mark = {Sz, DataPruned}};
            {{_M, _F, _A}, SzUb, _SzLb, _C} when SzUb > element(1,S#state.stack_trace_water_mark) ->
                S#state{stack_trace_water_mark = {SzUb, DataPruned}};
            _ ->
                S
        end,
    case TotalSize > element(1, S#state.high_water_mark) of
        true ->
            {noreply, S_StackFrame#state{high_water_mark = {TotalSize, DataPruned}}};
        false -> {noreply, S_StackFrame}
    end;
handle_info(_, S) ->
    {noreply, S}.

prune(Data) ->
    StackFrames = proplists:get_value(stack_frames, Data),
    proplists:delete(stack_frames, Data) ++ [{stack_frames, prune1(StackFrames)}].
prune1([{{M, F, A}, S},{{M,F,A},S2}|T]) ->
    prune1([{{M,F,A},S,S2, 2}|T]);
prune1([{{M, F, A}, S_H, S_L, Calls}, {{M, F, A}, S_Lr}|T]) when S_L >= S_Lr->
    prune1([{{M,F,A},S_H, S_Lr, Calls+1}|T]);
prune1([H|T])  ->
    [H | prune1(T)];
prune1([]) -> [].

terminate(_Reason, _) ->
    dbg:stop_clear(),
    ok.
% terminate(_Reason, #state{ fd = undefined }) ->
%     dbg:stop_clear(),
%     ok;
% terminate(_Reason, #state{ fd = Fd }) ->
%     ok = file:close(Fd),
%     dbg:stop_clear(),
%     ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.