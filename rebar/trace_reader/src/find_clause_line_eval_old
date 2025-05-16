-module(find_clause_line_eval).
-export([get_matching_clause_line/3]).

%-include_lib("include/erl_parse.hrl"). % For AST node definitions if needed, though erl_eval abstracts much

%% @doc Finds the line number of the first clause of FunName/Arity in FilePath
%%      whose patterns match Args and whose guards evaluate to true, using erl_eval.
%% @param FilePath Full path to the .erl source file (string() or binary()).
%% @param FunName The function name (atom()).
%% @param Args The list of arguments the function would be called with.
%% @returns {ok, LineNumber :: integer()} |
%%          {error, Reason :: term()}
%%          Reason: {compile_error, _, _} | {beam_lib_error, _} | no_abstract_code |
%%                  {function_not_found, _, _} | no_clause_matched |
%%                  {match_exception, Line, Exception} | {guard_exception, Line, Exception} |
%%                  {arg_conversion_error, Arg}
get_matching_clause_line(FilePath, FunName, Args) when is_list(Args) ->
    Arity = length(Args),
    % 1. Compile and get AST Forms
    case compile_and_extract_ast(FilePath) of
        {ok, Forms, _RecordDefs} -> % RecordDefs usually not needed when using erl_eval
             % 2. Find the function definition in the AST
             case find_function_clauses(Forms, FunName, Arity) of
                 {ok, Clauses} ->
                     % 3. Convert runtime Args to AST literal representation for matching
                     case args_to_ast_tuple(Args) of
                         {ok, ArgsAstTuple} ->
                             % 4. Iterate through clauses, using erl_eval to check match & guards
                             match_clauses_eval(Clauses, ArgsAstTuple);
                         {error, Reason} ->
                             {error, Reason} % Error converting args
                     end;
                 {error, Reason} ->
                      {error, Reason} % Function not found
             end;
        {error, Reason} ->
            {error, Reason} % Compilation or AST extraction error
    end.

% --- Compile and Extract AST (Slightly simplified - RecordDefs not strictly needed now) ---

compile_and_extract_ast(FilePath) ->
    CompileOpts = [binary, debug_info],
    case compile:file(FilePath, CompileOpts) of
        {ok, ModuleName, BeamBinary} ->
            extract_ast_and_records(BeamBinary, ModuleName); % Keep record parsing for now, might be useful later
        {error, Errors, Warnings} ->
            {error, {compile_error, Errors, Warnings}};
        error ->
             {error, {compile_error, "Unknown compilation error", []}}
    end.

extract_ast_and_records(BeamBinary, ModuleName) ->
     case beam_lib:chunks(BeamBinary, [abstract_code, attributes]) of
         {ok, {ModuleName, Chunks}} ->
              case lists:keyfind(abstract_code, 1, Chunks) of
                  {abstract_code, AbsCodeValue} ->
                      Forms = case AbsCodeValue of {raw_abstract_v1, F} -> F; {abstract_v2, _, F} -> F; _ -> [] end,
                      if Forms == [] -> {error, no_abstract_code};
                         true -> Attrs = case lists:keyfind(attributes, 1, Chunks) of {attributes, A} -> A; false -> [] end,
                                 RecordDefs = parse_record_defs(Attrs),
                                 {ok, Forms, RecordDefs}
                      end;
                  false -> {error, no_abstract_code}
              end;
         {error, Reason} -> {error, {beam_lib_error, Reason}}
     end.

parse_record_defs(Attributes) ->
    lists:foldl(
        fun({attribute, _, record, {RN, Fs}}, Acc) -> Acc#{ RN => [ get_rec_field_name(F) || F <- Fs ] }; (_, Acc) -> Acc end,
        maps:new(), Attributes).
get_rec_field_name({record_field, _, {atom, _, N}, _}) -> N; get_rec_field_name({record_field, _, {atom, _, N}}) -> N;
get_rec_field_name({typed_record_field, F, _}) -> get_rec_field_name(F); get_rec_field_name(_) -> error(bad_record_field_def).


% --- Find Function Clauses ---

find_function_clauses(Forms, FunName, Arity) ->
    TargetClauses = lists:filtermap(
        fun({function, _Line, Func, Ar, Clauses}) when Func == FunName, Ar == Arity -> {true, Clauses};
           (_) -> false
        end, Forms),
    case TargetClauses of
        [Clauses | _] -> {ok, Clauses}; % Return list of clause ASTs
        [] -> {error, {function_not_found, FunName, Arity}}
    end.

% --- Args to AST Conversion ---

%% Converts a list of runtime arguments to an AST tuple {tuple, 0, AstLiterals}
args_to_ast_tuple(Args) ->
    try lists:map(fun value_to_ast/1, Args) of
        ArgsAstList -> {ok, {tuple, 0, ArgsAstList}}
    catch
        throw:{arg_conversion_error, Arg} -> {error, {arg_conversion_error, Arg}}
    end.

%% Converts a single runtime value to its AST literal representation (simplified)
value_to_ast(V) when is_atom(V) -> {atom, 0, V};
value_to_ast(V) when is_integer(V) -> {integer, 0, V};
value_to_ast(V) when is_float(V) -> {float, 0, V};
value_to_ast(V) when is_pid(V) -> {pid, 0, erlang:pid_to_list(V)}; % Represent pid as string? erl_eval handles values directly
value_to_ast(V) when is_port(V) -> {port, 0, erlang:port_to_list(V)}; % Need a way erl_eval understands... maybe pass pid/port directly?
value_to_ast(V) when is_reference(V) -> {ref, 0, erlang:ref_to_list(V)}; % Let's pass values directly where AST literal is hard
value_to_ast([]) -> {nil, 0};
value_to_ast(V) when is_list(V) ->
    % Handle strings vs lists
    case io_lib:printable_list(V) of
        true -> {string, 0, V};
        false -> list_to_cons_ast(V) % Build {cons, 0, H, T} structure
    end;
value_to_ast(V) when is_tuple(V) ->
    ElemsAst = [value_to_ast(E) || E <- tuple_to_list(V)],
    {tuple, 0, ElemsAst};
value_to_ast(V) when is_map(V) ->
    AssocsAst = [{map_field_assoc, 0, value_to_ast(K), value_to_ast(Val)} || {K, Val} <- maps:to_list(V)],
    {map, 0, AssocsAst};
value_to_ast(V) when is_binary(V) ->
    % Build a {bin, [{bin_element, ...}]} structure for literal binary
    Segments = [{bin_element, 0, {integer, 0, Byte}, default, [unsigned, big, integer, 1]} || <<Byte>> <= V],
    {bin, 0, Segments};
value_to_ast(V) when is_function(V) ->
    % How to represent a fun in AST? Difficult. Maybe pass directly?
    % erl_eval might handle fun values in bindings. Let's pass it directly.
    % For the ArgsTupleAst, we need an AST node though. This is tricky.
    % Let's throw an error for now, as matching funs passed as args is complex anyway.
    throw({arg_conversion_error, V});
value_to_ast(Other) ->
    % Catch-all, potentially problematic
    throw({arg_conversion_error, Other}).

list_to_cons_ast([]) -> {nil, 0};
list_to_cons_ast([H | T]) -> {cons, 0, value_to_ast(H), list_to_cons_ast(T)}.

% --- Clause Matching using erl_eval ---

match_clauses_eval([], _ArgsAstTuple) ->
    {error, no_clause_matched};
match_clauses_eval([{clause, Line, _Patterns, _Guards, _Body} = Clause | RestClauses], ArgsAstTuple) ->
    case check_clause_match_eval(Clause, ArgsAstTuple) of
        {match, _Bindings} ->
            % Pattern matched and guards passed!
            {ok, Line};
        no_match ->
            % Pattern didn't match or guards failed, try next
            match_clauses_eval(RestClauses, ArgsAstTuple);
        {error, Reason} ->
             % An actual error occurred during evaluation
             {error, Reason}
    end.

%% @private Checks a single clause using erl_eval for pattern match and guards.
%% Returns: {match, Bindings} | no_match | {error, Reason}
check_clause_match_eval({clause, Line, Patterns, Guards, _Body}, ArgsAstTuple) ->
    erlang:display({clause, Line, Patterns, Guards, _Body}),
    PatternAstTuple = {tuple, Line, Patterns}, % Use clause line? Use 0? Use Line.
    MatchExpr = {match, Line, PatternAstTuple, ArgsAstTuple},
    InitialBindings = erl_eval:new_bindings(),
    erlang:display({match_expr, MatchExpr}),
    try erl_eval:expr(MatchExpr, InitialBindings) of
        {value, _MatchedValue, MatchBindings} ->
            case Guards of
                [] ->
                    % No guards, pattern matched successfully
                    {match, MatchBindings};
                _ ->
                        % Pattern match succeeded, now evaluate guards
                        try erl_eval:exprs(Guards, MatchBindings) of % Guards is list of guard exprs [[G1, G2], [G3]]
                        {value, true, _GuardBindings} ->
                                % Guards evaluated to true!
                                {match, _GuardBindings}; % Or return MatchBindings? GuardBindings is more complete.
                        {value, false, _GuardBindings} ->
                                % Guards evaluated to false
                                no_match
                        catch
                                ExceptionType:Reason:Stacktrace ->
                                % Error during guard evaluation
                                erlang:display({error, {guard_exception, Line, {ExceptionType, Reason, Stacktrace}}}), no_match
                        end
                end
    catch % Catch errors in erl_eval:expr/2 itself if any
         ExceptionType:Reason:Stacktrace ->
             erlang:display({error, {match_exception, Line, {ExceptionType, Reason, Stacktrace}}}), no_match % Or a more general eval_error?
    end.
