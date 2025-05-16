Okay, you've mastered the basics of `dbg` and can trace simple function calls and messages. Let's level up! This intermediate guide introduces more powerful features to tackle trickier debugging scenarios.

We'll assume you're comfortable with the `dbg:tracer()`, `dbg:p/2`, `dbg:tp/2`, and `dbg:stop()` workflow from the beginner's guide.

## 1. Deeper Function Tracing: Local Calls & Conditions

### Conditional Tracing with Match Specifications (`fun2ms`)

What if you only want to trace a function call if an argument has a specific value, or if it returns an error? Listing *every* call can be noisy. Match Specifications (`match_spec`) let you define complex conditions.

Writing raw match specs is tricky. Luckily, `dbg:fun2ms/1` lets you write conditions using familiar `fun` syntax!

**Important:** To use `fun2ms` in compiled code, you *must* include the `ms_transform` header:
`-include_lib("stdlib/include/ms_transform.hrl").`
(It works directly in the shell without this).

The basic `fun` structure for `fun2ms` is: `fun([Arg1, Arg2, ...]) when Guard -> Body end.`

* `[Arg1, Arg2, ...]`: Matches the arguments passed to the function. Use `_` for unused args.
* `Guard`: Erlang guard expressions (like `N > 3`, `is_atom(M)`) to filter calls.
* `Body`: What `dbg` should do. Often just `return_trace()` to trace the call and return value if the guard passes. You can also use `{message, term()}` to add custom info or `{enable_trace}`/`{disable_trace}`.

*Example 1: Trace `my_calc:add/2` only when the second argument is 0 (the crashing case from the beginner guide).*

```erlang
1> dbg:tracer().
{ok,<0.110.0>}
2> dbg:p(new, [c, timestamp]).
{ok,[...]}
3> % Define the condition using fun2ms
3> MatchSpec = dbg:fun2ms(fun([_Arg1, 0]) -> return_trace() end).
%% MatchSpec will contain the complex term representation, e.g.:
%% [{['$1',0],[],[{return_trace}]}]
4> dbg:tp(my_calc, add, 2, MatchSpec). % Use the generated MatchSpec
{ok,[{matched,nonode@nohost,1},{saved,1}]} % Saved match_spec #1

5> spawn(fun() -> my_calc:add(5, 10) end). % B=10, doesn't match [_, 0]
<0.114.0>
(<0.112.0>) spawn <0.114.0> ... (Timestamp: ...) % Process starts ('p' flag implied by 'c')
% !!! NO 'call' or 'returned' trace for add/2 !!!
(<0.114.0>) exit normal (Timestamp: ...)         % Process exits

6> spawn(fun() -> my_calc:add(5, 0) end). % B=0, matches [_, 0]
<0.116.0>
(<0.112.0>) spawn <0.116.0> ... (Timestamp: ...)
(<0.116.0>) call my_calc:add(5,0) (Timestamp: ...) % !!! TRACED !!! (Guard passed)
(<0.116.0>) exception exit:{division_by_zero, ...} from my_calc:add/2 (Timestamp: ...) % return_trace shows exception
(<0.116.0>) exit {division_by_zero, ...} (Timestamp: ...)

7> dbg:stop().
ok
```

*Example 2: Trace `lists:map/2` only if the result is an empty list `[]`.*

```erlang
1> dbg:tracer().
{ok,<0.120.0>}
2> dbg:p(self(), [c, timestamp]). % Trace the shell process itself
{ok,[...]}
3> % Match on the *return value* using '$_' inside the fun body
3> MatchSpec = dbg:fun2ms(fun(_) ->
3>    case '$_' of % '$_' represents the return value in the body
3>      [] -> return_trace(); % Trace only if return is []
3>      _  -> false           % Otherwise, don't trace this call
3>    end
3> end).
[{['_'],[],[{{case_,'$_',[{[],[],[{return_trace}]},{'_',[],[{const,false}]}]}}]}]

4> dbg:tp(lists, map, 2, MatchSpec).
{ok,[{matched,nonode@nohost,1},{saved,1}]}

5> lists:map(fun(X) -> X*2 end, [1,2]). % Returns [2,4], not []
% NO TRACE OUTPUT
[2,4]

6> lists:map(fun(X) -> X*2 end, []). % Returns [], matches!
(<0.120.0>) call lists:map(#Fun<...>,[]) (Timestamp: ...) % TRACED call
(<0.120.0>) returned from lists:map/2 -> [] (Timestamp: ...) % TRACED return
[]

7> dbg:stop().
ok
```
Match specifications are incredibly powerful but have a steeper learning curve. Start with `fun2ms` for simple conditions.

## 2. Fine-Grained Process Tracing Flags

In the beginner's guide, we used `m` (messages) and `p` (procs). You can be more specific:

* `send`: Trace only messages *sent by* the target process.
* `'receive'` (Note the quotes!): Trace only messages *received by* the target process.
* `procs`: This is actually a shortcut for flags like `spawn`, `exit`, `link`, `unlink`, `register`, `unregister`, `getting_linked`, `getting_unlinked`. You can use these individual flags if you only care about specific lifecycle events.
* `sos` (Set On Spawn): Any process spawned *by* a traced process automatically inherits its trace flags. Useful for tracing worker pools.
* `sol` (Set On Link): Any process linked *by* a traced process inherits its trace flags.

*Example: Trace only sends from Pid1 and only receives by Pid2.*

```erlang
1> Pid1 = spawn(fun() -> timer:sleep(100), whereis(receiver) ! hello end).
<0.130.0>
2> Pid2 = spawn(fun() -> register(receiver, self()), receive Msg -> io:format("Receiver got: ~p~n", [Msg]) end end).
<0.132.0>
3> dbg:tracer().
{ok,<0.134.0>}
4> dbg:p(Pid1, [send, timestamp]). % Only trace sends from Pid1
{ok,[...]}
5> dbg:p(Pid2, ['receive', timestamp]). % Only trace receives by Pid2
{ok,[...]}

% Wait for Pid1 to send...
(<0.130.0>) <0.132.0> ! hello (Timestamp: ...) % Send trace from Pid1 appears
Receiver got: hello                         % Output from Pid2
(<0.132.0>) << hello (Timestamp: ...)         % Receive trace from Pid2 appears

6> dbg:stop().
ok
```
*Note on Message Content:* `dbg` flags don't directly filter or display message *content* easily. To inspect message content during tracing, you typically need to use match specifications on the `'receive'` pseudo-function call, which is an advanced technique, or use other tools like `recon_trace`.

## 3. Tracing Across the Network (Distributed Erlang)

`dbg` shines in distributed systems. If you have connected Erlang nodes (e.g., `a@host` and `b@host`), you can trace interactions between them from a single console.

1.  **Start `dbg` on your main node** (e.g., `a@host`).
2.  **Tell `dbg` about the other node(s)** using `dbg:n(NodeName)`.
3.  **Set trace points (`p`, `tp`) as usual.** They will apply to *all* nodes managed by `dbg`.
4.  **Trace messages will be forwarded** from the remote node(s) to your main node's tracer.

*Example: Node `a@host` traces a message send to a process registered as `worker` on node `b@host`.*

```erlang
%% --- On Node B (b@host) ---
b@host> spawn(fun() -> receive Any -> io:format("Node B got: ~p~n", [Any]) end end).
<7198.60.0>
b@host> register(worker, <7198.60.0>).
true

%% --- On Node A (a@host) ---
a@host> dbg:tracer().
{ok,<0.140.0>}
a@host> dbg:n('b@host').  % Tell dbg to trace Node B as well
{ok,'b@host'}
a@host> dbg:p(all, [m, timestamp]). % Trace messages on ALL processes on ALL traced nodes (a and b)
{ok,[{matched,a@host,15},{matched,b@host,12}]} % Matched processes on both nodes

a@host> {worker, 'b@host'} ! {request, from_a}. % Send message from Node A to Node B
%% === dbg output on Node A ===
(<0.138.0>) {worker,'b@host'} ! {request,from_a} (Timestamp: ...) % Send from Node A shell (<0.138.0>)
{request,from_a}
(<7198.60.0>) << {request,from_a} (Timestamp: ...)              % Receive by Node B process (<7198.60.0>)
%% === dbg output ends ===

%% --- Output on Node B's console ---
Node B got: {request,from_a}

%% --- Back on Node A ---
a@host> dbg:stop(). % This stops tracing on ALL nodes managed (a and b)
ok
```
Notice how the trace output includes the Pid (`<7198.60.0>`) from the remote node, clearly showing the cross-node communication.

Use `dbg:ln()` to list nodes being traced and `dbg:cn(NodeName)` or `dbg:cn()` to remove nodes.

## 4. Handling Trace Floods: File & IP Tracing

If you trace very active processes or frequent functions, printing thousands of messages to the shell is slow and can even cause problems (see Group Leader Issue below). Solution: redirect trace output!

* **File Tracing:** `dbg:tracer(port, {file, FileName})`
    * Starts a tracer that writes efficiently to a file. `FileName` is a string (e.g., `"trace.log"`).
    * The file contains raw Erlang terms, not formatted text. Example line might look like: `{trace, <0.123.0>, send, {<0.456.0>, hello}, {1710, 900000, 100000}}`. You might need simple scripts to parse or view these logs.

* **IP Tracing (More Advanced):** `dbg:tracer(port, {ip, {Host, Port}})`
    * Sends trace messages as UDP packets to a specified IP address and port.
    * You need a separate process listening on that UDP port (use `gen_udp`) to receive and process the traces.
    * You can use `dbg:trace_client(Host, Port)` to start a simple default listener that prints to its console.

*Example: Trace messages for `ServerPid` to a file.*

```erlang
1> ServerPid = whereis(some_server).
<0.150.0>
2> dbg:tracer(port, {file, "server_trace.log"}). % Start file tracer
{ok,<0.155.0>} % Returns Pid of the file tracer *process*
3> dbg:p(ServerPid, [m, timestamp]).
{ok,[...]}

4> % ... run code that interacts with some_server ...

5> dbg:stop(). % Stops the file tracer and closes the file
ok

6> % Now inspect "server_trace.log" - it will contain Erlang terms.
```

## 5. Managing Trace Patterns & Nodes

As tracing sessions get complex, you might need to manage your setup:

* `dbg:ctp(MFA | Module | all)`: Clear Trace Pattern(s) for global calls.
* `dbg:ctpl(MFA | Module | all)`: Clear Trace Pattern(s) for local calls.
* `dbg:ctpg(MFA | Module | all)`: Clear Trace Pattern(s) for both.
* `dbg:ltp()`: List currently active Trace Patterns (shows saved match spec numbers).
* `dbg:ln()`: List Nodes being traced.
* `dbg:cn(NodeName | all)`: Clear Node(s) from tracing.

## 6. Avoiding Deadlocks: The Group Leader Problem

**The Issue:** If you use the default shell tracer (`dbg:tracer()`) and trace a process (like the shell itself or something printing heavily to the shell) that *also* uses the shell for output (via its "group leader"), you can hit a deadlock.
* Scenario: Process A is traced. It tries to print to the shell. The shell group leader waits for Process A to finish printing.
* Simultaneously: The `dbg` tracer process sees a trace event from Process A. It tries to print the trace message *to the shell*.
* Deadlock: The `dbg` tracer waits for the shell group leader, which is waiting for Process A, which might be waiting for its trace call to complete before continuing... nothing progresses.

**Mitigation:**

1.  **Be Specific:** Avoid `dbg:p(all, ...)` when tracing to the shell if you suspect I/O interactions. Trace specific Pids instead.
2.  **Don't Trace the Shell's I/O:** If you *must* trace shell-interactive processes, avoid tracing the shell group leader itself. (`GL = process_info(dbg:get_tracer(), group_leader), dbg:p(GL, clear)` might help, but it's fiddly).
3.  **Use File/IP Tracing:** The *best* solution for complex or I/O-heavy tracing is `dbg:trace_port/2`. Since it doesn't write to the shell group leader, it completely avoids this type of deadlock.

## Conclusion

You've now explored more advanced `dbg` techniques: tracing local functions with `dbg:tpl`, applying conditional tracing with `fun2ms`, managing distributed tracing with `dbg:n`, handling high volumes with `dbg:trace_port`, and understanding potential pitfalls like the group leader deadlock.

This gives you significantly more power and precision in diagnosing complex Erlang issues. Remember to consult the official Erlang `dbg` documentation for even more flags, match spec details, and functions as you encounter new challenges. Practice using these tools, and happy debugging!