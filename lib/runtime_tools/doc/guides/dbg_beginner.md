TODO:
As of 27, it is possible to create separate sessions, if you want to perform independent tracing from other traces in the system. Using the default tracer command, creates a default tracing session named dbg.

dbg:session_create(named_session)

## A Beginner's Guide to Understanding Your Code with `dbg`

Welcome! You're writing code, perhaps involving multiple processes sending messages, maybe even across different computers (nodes). Sometimes, things don't behave as expected. How can you peek inside your running system to see what's *really* happening without stopping everything?

Meet the `dbg` module! Think of it as a set of listening devices you can temporarily attach to your Erlang system. It lets you eavesdrop on:

* Function calls: See when functions are entered and exited, what data they received, and what they returned.
* Messages: Watch messages being sent and received between processes.
* Process events: Observe when processes start, stop, or link up.

This helps you diagnose problems, understand complex interactions, and confirm your code is working as intended.

**Why Bother with Tracing?**

Imagine these scenarios:

* **Mystery Crash:** A process suddenly stops, but you don't know exactly why or where.
* **Wrong Order:** Messages seem to arrive in an unexpected sequence, causing bugs.
* **Slowdown:** A part of your system becomes sluggish, but you're unsure which function or process is the bottleneck.
* **Silence:** You expect a message to arrive, but it never does (a potential deadlock).

`dbg` can help you pinpoint the cause in all these situations by showing you the flow of execution and messages.

### The Absolute Simplest Trace: Watching One Function Call (`dbg:c/3`)

The quickest way to see what happens inside a single function call *right now* is using `dbg:c(Module, Function, Arguments)`. It's like saying, "Hey `dbg`, quickly set up tracing, run *this specific function call* for me, show me what happened inside, and then clean up."

Let's try tracing the built-in function `lists:reverse/1` which reverses a list:

```erlang
%% In your Erlang shell (erl):
1> dbg:c(lists, reverse, [[1, 2, 3]]).
%% === dbg output starts ===
(<0.56.0>) call lists:reverse([1,2,3])        % Our initial call
(<0.56.0>) call lists:reverse([2,3], [1])      % Internal detail: how reverse works
(<0.56.0>) call lists:reverse([3], [2,1])     % Internal detail
(<0.56.0>) call lists:reverse([], [3,2,1])      % Internal detail (base case)
(<0.56.0>) returned from lists:reverse/2 -> [3,2,1] % Return from internal call
(<0.56.0>) returned from lists:reverse/2 -> [3,2,1] % Return from internal call
(<0.56.0>) returned from lists:reverse/2 -> [3,2,1] % Return from internal call
(<0.56.0>) returned from lists:reverse/1 -> [3,2,1] % Final return value of our call
%% === dbg output ends ===
[3,2,1]                                      % The actual result of the function
```

**What happened here?**

1.  `dbg:c/3` temporarily turned on tracing for function calls (`call`) and their return values (`return_trace`).
2.  It called `lists:reverse` with the argument `[1, 2, 3]`.
3.  `dbg` printed messages for each function call it detected (including internal calls `lists:reverse` makes to itself) and the values returned.
4.  Finally, the shell printed the actual result of the function call: `[3,2,1]`.

`dbg:c/3` is great for quick checks on pure functions, but for tracing interactions, processes, or longer-running code, you need the standard workflow.

### The Standard Tracing Workflow (More Control)

For most debugging tasks, you'll follow these steps:

1.  **Start a Listener (`dbg:tracer/0`)**: Create a dedicated process to receive and display trace messages.
2.  **Choose Targets & Events (`dbg:p/2`)**: Tell the listener *which* processes to watch and *what kind* of events (like messages or calls) to report.
3.  **(Optional) Specify Functions (`dbg:tp` or `dbg:tpl`)**: If tracing calls, tell the listener *exactly which* function calls you care about.
4.  **Run Your Code**: Trigger the behavior you want to investigate.
5.  **Stop Listening (`dbg:stop/0`)**: Clean up and turn off tracing.

Let's break that down:

**Step 1: Start the Listener (`dbg:tracer`)**

```erlang
1> dbg:tracer().
{ok,<0.90.0>} % Success! The <0.90.0> is the Process Identifier (Pid) of the listener.
```

This starts a background process that, by default, prints any trace messages it receives directly to your shell console.

**Step 2: Choose Targets and Events (`dbg:p`)**

Now, tell the listener what to watch using `dbg:p(Target, Flags)`.

* **`Target`**: Who are we watching?
    * A specific `Pid` (like `<0.105.0>`). You often get Pids from `spawn` or functions like `whereis/1`.
    * `new`: Watch *all future* processes that get started *after* this command.
    * `all`: Watch *all current and future* processes. (Use with caution, can be noisy!).
    * *(There are other targets like `existing`, registered names, ports, etc., but `Pid` and `new` are common starting points).*
* **`Flags`**: What events are we interested in? This is a list `[...]` of atoms:
    * `c` (or `call`): Watch function calls. **Requires a trace pattern too see Step 3 (`dbg:tp`).**
    * `m` (or `messages`): Watch messages being sent (`send`) and received (`'receive'`).
    * `p` (or `procs`): Watch process lifecycle events (spawning, exiting, linking).
    * `timestamp`: Adds a timestamp to every trace message, showing *when* things happened.
    * Many other flags exist, like `s` for only `send`, `r` for only `'receive'`, etc.

*Example: Let's watch all *new* processes, focusing on messages and process events, with timestamps.*

```erlang
2> dbg:p(new, [m, p, timestamp]).
{ok,[{matched,nonode@nohost,0}]} % OK. Matched 0 existing processes, but will apply to new ones.
```

**Step 3: Specify Functions (Optional, Needed for `c` flag) (`dbg:tp`)**

If you used the `c` flag in `dbg:p/2`, you *must* also tell `dbg` *which* specific function calls to trace using `dbg:tp(ModuleOrMFA, MatchSpec)`.

* **`ModuleOrMFA`**: Which function(s)?
    * `my_module`: Trace *all* exported functions in `my_module`.
    * `{my_module, my_fun, 2}`: Trace *only* the function `my_module:my_fun` with 2 arguments (Arity = 2). This `Module, Function, Arity` format is often called an "MFA".
    * *(You can use `'_'` as wildcards, e.g., `{my_module, '_', '_'}`, but start specific).*
* **`MatchSpec`**: How to trace?
    * `[]`: The simplest. Just shows the function was called (basic `call` trace).
    * **Helpful Aliases (use these!)**:
        * `x`: Trace calls, arguments, return values, **and crashes (exceptions)**.
        * `c`: Trace calls, arguments, and **who called** this function (`caller_trace`).
        * `cx`: Combines `c` and `x`. Shows calls, args, return values, exceptions, *and* the caller.

*Example: Let's trace calls to a function `my_calc:add/2` in any *new* process (because we set `dbg:p(new, ...)` earlier), and we want to see arguments, return values, exceptions, and the caller.*

```erlang
3> dbg:tp(my_calc, add, 2, cx).
{ok,[{matched,nonode@nohost,1},{saved,cx}]} % OK. Found 1 function matching, using 'cx' mode.
```

*Note: `dbg:tpl` is similar but traces non-exported/local function calls too.*

**Step 4: Run Your Code**

Now, perform the actions in your system that you want to debug. If you start a new process (matching `dbg:p(new,...)`) and it calls `my_calc:add/2` (matching `dbg:tp(...)`), you'll see trace messages appear in your shell.

*Example: Suppose we have a module `my_calc` with `add/2` and we spawn a process to use it:*

```erlang
%% --- Assume my_calc.erl ---
%% -module(my_calc).
%% -export([add/2]).
%% add(A, B) -> A + B.
%% --------------------------
4> c(my_calc). % Compile it
{ok, my_calc}

5> spawn(fun() -> Result = my_calc:add(5, 10), io:format("Calc result: ~p~n", [Result]) end).
%% === dbg output starts ===
(<0.100.0>) spawn <0.102.0> as {erlang,apply,2} (Timestamp: {1710, 900000, 100000})  % 'p' flag: Process <0.100.0> (shell) spawned <0.102.0>
<0.102.0>                                                                           % Pid of the new process returned by spawn/1
(<0.102.0>) call my_calc:add(5,10) ({erlang,apply,2}) (Timestamp: {1710, 900000, 150000}) % 'c' flag + 'tp': Call to add/2 with args (5, 10), called by erlang:apply/2
(<0.102.0>) returned from my_calc:add/2 -> 15 (Timestamp: {1710, 900000, 200000})       % 'tp' with cx: Return value is 15
Calc result: 15                                                                     % Normal output from the spawned process
(<0.102.0>) exit normal (Timestamp: {1710, 900000, 300000})                         % 'p' flag: Process <0.102.0> exited normally
%% === dbg output ends ===
```

**Look closely at the trace:** Each line starts with the `Pid` (`<0.102.0>`) that generated the event. The `timestamp` shows when it happened. The text explains the event (spawn, call, returned, exit) based on the flags (`p`, `c`) and trace pattern (`tp` with `cx`) we set up.

**Step 5: Stop Listening (`dbg:stop`)**

When you're done tracing, clean up:

```erlang
6> dbg:stop().
ok
```

This stops the tracer process and clears all the trace patterns and flags you set. Forgetting this can leave tracing active, potentially slowing down your system or filling logs.

### Example: Finding a Crash

Let's modify `my_calc` to sometimes crash and use `dbg` to see why.

```erlang
%% --- Assume my_calc.erl ---
%% -module(my_calc).
%% -export([add/2]).
%% add(A, 0) -> erlang:error(division_by_zero); % Intentionally crash if B is 0
%% add(A, B) -> A + B.
%% --------------------------

1> c(my_calc).
{ok, my_calc}
2> dbg:tracer().
{ok,<0.110.0>}
3> dbg:p(new, [p, c, timestamp]). % Watch new processes: process events and calls
{ok,[{matched,nonode@nohost,0}]}
4> dbg:tp(my_calc, add, 2, x). % Trace add/2, show exceptions ('x')
{ok,[{matched,nonode@nohost,1},{saved,x}]}

5> spawn(fun() -> my_calc:add(5, 0) end). % Call the crashing case
%% === dbg output starts ===
(<0.112.0>) spawn <0.114.0> as {erlang,apply,[#Fun<erl_eval.43.2783542>,[]]} (Timestamp: {1710, 910000, 100000}) % 'p': Spawn event
<0.114.0>                                                                          % Pid returned by spawn/1
(<0.114.0>) call my_calc:add(5,0) (Timestamp: {1710, 910000, 110000})               % 'c'+'tp': Call event with args
(<0.114.0>) exception exit:{{division_by_zero,[{my_calc,add,2,[...]}, ...]}, ...} from my_calc:add/2 (Timestamp: {1710, 910000, 120000}) % 'tp' with 'x': !!! EXCEPTION !!!
(<0.114.0>) exit {{division_by_zero,[{my_calc,add,2,[...]}, ...]}, ...} (Timestamp: {1710, 910000, 130000}) % 'p': Exit event with reason
%% === dbg output ends ===

6> dbg:stop().
ok
```
The `exception exit` line clearly shows the `division_by_zero` error originating from `my_calc:add/2`, exactly what we needed to know!

### Example: Watching Messages

Let's trace messages sent to a simple server.

```erlang
%% --- Assume simple_server.erl ---
%% -module(simple_server).
%% -export([start/0, loop/0]).
%% start() -> spawn(simple_server, loop, []).
%% loop() ->
%%   receive
%%     {From, ping} -> From ! pong, loop();
%%     stop -> ok
%%   end.
%% ---------------------------------

1> c(simple_server).
{ok, simple_server}
2> ServerPid = simple_server:start().
<0.122.0>
3> dbg:tracer().
{ok,<0.124.0>}
4> dbg:p(ServerPid, [m, timestamp]). % Watch ServerPid, trace messages ('m'), add timestamps
{ok,[{matched,nonode@nohost,1}]}

5> ServerPid ! {self(), ping}. % Send a message to the server
%% === dbg output starts ===
(<0.120.0>) <0.122.0> ! {<0.120.0>,ping} (Timestamp: {1710, 920000, 100000}) % 'm' send: Shell (<0.120.0>) sent ! message to ServerPid (<0.122.0>)
{<0.120.0>,ping}                                                              % Value returned by the send operator '!'
(<0.122.0>) << {<0.120.0>,ping} (Timestamp: {1710, 920000, 110000})            % 'm' receive: ServerPid (<0.122.0>) received << message
(<0.122.0>) <0.120.0> ! pong (Timestamp: {1710, 920000, 120000})             % 'm' send: ServerPid (<0.122.0>) sent ! pong back to shell (<0.120.0>)
%% === dbg output ends ===

6> flush(). % Display messages sent to the shell
Shell got pong
ok

7> ServerPid ! stop. % Stop the server
%% === dbg output starts ===
(<0.120.0>) <0.122.0> ! stop (Timestamp: {1710, 920000, 200000})
stop
(<0.122.0>) << stop (Timestamp: {1710, 920000, 210000})
%% === dbg output ends ===

8> dbg:stop().
ok
```
Here, the `!` lines show messages being sent, and the `<<` lines show messages being received by the `ServerPid` we targeted. The timestamps help see the order.

### Important Considerations for Beginners

* **Performance:** Tracing isn't free. Tracing lots of processes or very frequent function calls *will* slow down your system. Be specific about what you trace (`dbg:p(SpecificPid, ...)`, `dbg:tp(SpecificMFA, ...)`). Remember to `dbg:stop()` when done.
* **Shell Output (`dbg:tracer`) vs File (`dbg:trace_port`)**: For simple debugging, printing to the shell (`dbg:tracer()`) is fine. If you generate *tons* of trace messages, it can overwhelm the shell. For high-volume tracing, look into `dbg:trace_port/2` to write traces to a file, which is much more efficient.
* **Complexity:** `dbg` has many more features (complex match specs, tracing specific message content, tracing across nodes with `dbg:n/1`). Master these basics first, then explore the official Erlang `dbg` documentation when you need more power.

You've now learned the fundamentals of Erlang tracing with `dbg`. You can inspect single function calls with `dbg:c/3` and perform more controlled tracing of processes, messages, and specific functions using the standard `tracer`, `p`, `tp`, `stop` workflow. Happy debugging!