-module(post_process_tracer).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        dir,
        processes
}).

start_link(Dir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Dir], []).

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

init([Dir]) ->
        %% message_queue_len from process info is of interest to map send, receive
        message_tracer:start_link(),
        case file:read_file(filename:join(Dir, "process_info")) of
                {ok, FileContent} ->
                        Terms = read_terms(FileContent, []),

                        Map = #{Pid => PT || {Pid, PT} <- [begin
                                        {ok, ProcTracer} = process_tracer:start_link(Pid, Dir, ProcessInfo),
                                        {Pid, ProcTracer}
                                end || {Pid, ProcessInfo} <- Terms, ProcessInfo =/= undefined]},

                        %% move to handle_cast(start)
                        maps:foreach(fun(Pid, PT) -> spawn(fun() ->
                                case file:read_file(filename:join(Dir, pid_to_filename(Pid) ++ ".trace")) of
                                        {ok, FileContent1} -> [begin process_tracer:trace(PT, {trace, Trace}) end || Trace <- read_terms(FileContent1, [])],
                                                process_tracer:stop(PT);
                                        {error, Reason} -> io:format("Failed to open file: ~p~n", [Reason])
                                end end) end, Map),
                        {ok, #state{dir=Dir, processes=Map}};
                {error, Reason} ->
                        io:format("Failed to open file: ~p~n", [Reason]),
                        {stop, Reason}
        end.

read_terms(<<>>, Acc) -> lists:reverse(Acc);
read_terms(Content, Acc) ->
        T=binary_to_term(Content),
        B=term_to_binary(T),
        [_, ContentNew] = binary:split(Content, B, []),
        read_terms(ContentNew, [T|Acc]).
        
% read_terms(File, Acc) ->
%         case file:read_line(File) of
%                 {ok, Line} ->
%                         case catch erlang:binary_to_term(Line) of
%                                 Term when is_tuple(Term) ->
%                                         read_terms(File, [Term | Acc]);
%                                 _ ->
%                                         io:format("Invalid term in file: ~p~n", [Line]),
%                                         read_terms(File, Acc)
%                         end;
%                 eof ->
%                         lists:reverse(Acc);
%                 {error, Reason} ->
%                         io:format("Error reading file: ~p~n", [Reason]),
%                         lists:reverse(Acc)
%         end.

handle_call(_Request, _From, State) ->
        {reply, ok, State}.

handle_cast(start, State) ->
        io:format("Starting post-process tracer~n"),
        {noreply, State};

handle_cast(stop, State) ->
        io:format("Stopping post-process tracer~n"),
        {stop, normal, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        io:format("Terminating post-process tracer~n"),
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.