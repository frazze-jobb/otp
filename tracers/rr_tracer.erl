-module(rr_tracer).
-behavior(gen_server).
-export([]).

breakpoint() -> ok.
continue() -> ok.
next() -> ok.
step() -> ok.