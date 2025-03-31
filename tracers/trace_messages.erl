-module(trace_messages).
-export([wait_on_message/0]).

wait_on_message() ->
        receive
                the_message ->
                        do_calculations(),
                        wait_on_message()
        end.

do_calculations() ->
        fib(300).

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).