```erlang
%% Build
c("main_tracer.erl").
c("trace_rotator.erl").
c("message_tracer.erl").
c("process_tracer.erl").
c("post_process_tracer.erl").

%% Start tracing
{ok, MT} = main_tracer:start_link().
Session = main_tracer:trace_session(MT).
ABC = fun() -> trace_test:fib(4) end.
InternalF = spawn(fun() -> Hej = fun H(State) -> receive start -> Pid = spawn(ABC), H(Pid); stop -> State ! stop end end, Hej(0) end).  
trace:process(Session, InternalF, true, [call,'receive',send, return_to, set_on_spawn, procs]).
trace:function(Session, {'_', '_', '_'}, [{'_', [], [{message, {caller_line}},{return_trace}]}], [local]).
InternalF ! start.
InternalF ! stop.

%% Current problems, we only want to trace specific functions, and whatever they spawn.
%% data should be encoded into json, they are currently not, for better readability, if a separate tool exists to read the json and step through it
%% then we don't really have to have readable data.
%% Stop tracing
main_tracer:stop(MT).

%% Post processing
post_process_tracer:start_link("main_tracer/latest").

```