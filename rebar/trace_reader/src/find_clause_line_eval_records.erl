-module(find_clause_line_eval_records).
-export([get_matching_clause_line/3]).

%% @doc Finds the line number of the first clause of FunName/Arity in FilePath
%%      whose patterns match Args and whose guards evaluate to true, using erl_eval
%%      and handling record patterns.
%% @param FilePath Full path to the .erl source file (string() or binary()).
%% @param FunName The function name (atom()).
%% @param Args The list of arguments the function would be called with.
%% @returns {ok, LineNumber :: integer()} |
%%          {error, Reason :: term()}
%%          Reason: {compile_error, _, _} | {beam_lib_error, _} | no_abstract_code |
%%                  {function_not_found, _, _} | no_clause_matched |
%%                  {match_exception, Line, Exception} | {guard_exception, Line, Exception} |
%%                  {arg_conversion_error, Arg} | {record_def_not_found, RecordName} |
%%                  {bad_record_field_pattern, FieldPattern}
get_matching_clause_line(FilePath, FunName, Args) when is_list(Args) ->
    Arity = length(Args),
    % 1. Compile and get AST Forms and Record Definitions
    case compile_and_extract_ast(FilePath) of
        {ok, Forms, RecordDefs} ->
             % 2. Find the function definition in the AST
             case find_function_clauses(Forms, FunName, Arity) of
                 {ok, Clauses} ->
                     % 3. Convert runtime Args to AST literal representation for matching
                     case args_to_ast_tuple(Args) of
                         {ok, ArgsAstTuple} ->
                             % 4. Iterate through clauses, expanding record patterns
                             %    and using erl_eval to check match & guards
                             match_clauses_eval(Clauses, ArgsAstTuple, RecordDefs);
                         {error, Reason} ->
                             {error, Reason} % Error converting args
                     end;
                 {error, Reason} ->
                      {error, Reason} % Function not found
             end;
        {error, Reason} ->
            {error, Reason} % Compilation or AST extraction error
    end.

% --- Compile and Extract AST ---
compile_and_extract_ast(FilePath) ->
    CompileOpts = [binary, debug_info],%, {parse_transform, expand_records}], % Added expand_records PT for easier AST access initially
    case compile:file(FilePath, CompileOpts) of
        {ok, ModuleName, BeamBinary} ->
            extract_ast_and_records(BeamBinary, ModuleName);
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
                         true -> Attrs = [ R || {attribute, _, record, {RecName, RecDef}}=R <- Forms],
                                 RecordDefs = parse_record_defs(Attrs),
                                 {ok, Forms, RecordDefs} % Return RecordDefs
                      end;
                  false -> {error, no_abstract_code}
              end;
         {error, Reason} -> {error, {beam_lib_error, Reason}}
     end.

% Parses record definitions from attributes
% Returns: map(#{RecordName :: atom() => [FieldName :: atom()]})
parse_record_defs(Attributes) ->
    lists:foldl(
        fun({attribute, _, record, {RN, Fs}}, Acc) ->
                FieldNames = [get_rec_field_name(F) || F <- Fs],
                Acc#{ RN => FieldNames };
           (_, Acc) -> Acc
        end,
        #{}, Attributes).

% Extracts field name atom from record field definition in AST
get_rec_field_name({record_field, _, {atom, _, N}, _}) -> N;
get_rec_field_name({record_field, _, {atom, _, N}}) -> N;
get_rec_field_name({typed_record_field, F, _}) -> get_rec_field_name(F);
get_rec_field_name({record_field, _}) -> '_'; % Handle '_' field name if explicitly used? Unlikely.
get_rec_field_name(Other) ->
    error({bad_record_field_def, Other}).

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

% --- Args to AST Conversion (remains the same) ---
args_to_ast_tuple(Args) ->
    try lists:map(fun value_to_ast/1, Args) of
        ArgsAstList -> {ok, {tuple, 0, ArgsAstList}}
    catch
        throw:{arg_conversion_error, Arg} -> {error, {arg_conversion_error, Arg}}
    end.

value_to_ast(V) when is_atom(V) -> {atom, 0, V};
value_to_ast(V) when is_integer(V) -> {integer, 0, V};
value_to_ast(V) when is_float(V) -> {float, 0, V};
value_to_ast(V) when is_pid(V) -> value_to_ast(erlang:pid_to_list(V)); % Use string for erl_eval
value_to_ast(V) when is_port(V) -> value_to_ast(erlang:port_to_list(V)); % Use string
value_to_ast(V) when is_reference(V) -> value_to_ast(erlang:ref_to_list(V)); % Use string
value_to_ast([]) -> {nil, 0};
value_to_ast(V) when is_list(V) ->
    case io_lib:printable_list(V) of
        true -> {string, 0, V};
        false -> list_to_cons_ast(V)
    end;
value_to_ast(V) when is_tuple(V) ->
    ElemsAst = [value_to_ast(E) || E <- tuple_to_list(V)],
    {tuple, 0, ElemsAst};
value_to_ast(V) when is_map(V) ->
    AssocsAst = [{map_field_assoc, 0, value_to_ast(K), value_to_ast(Val)} || {K, Val} <- maps:to_list(V)],
    {map, 0, AssocsAst};
value_to_ast(V) when is_binary(V) ->
    Segments = [{bin_element, 0, {integer, 0, Byte}, default, default} || <<Byte>> <= V], % Simplified binary AST
    {bin, 0, Segments};
value_to_ast(V) when is_function(V) ->
    throw({arg_conversion_error, V}); % Still problematic
value_to_ast(Other) ->
    throw({arg_conversion_error, Other}).

list_to_cons_ast([]) -> {nil, 0};
list_to_cons_ast([H | T]) -> {cons, 0, value_to_ast(H), list_to_cons_ast(T)}.

% --- Clause Matching using erl_eval (Modified) ---
match_clauses_eval([], _ArgsAstTuple, _RecordDefs) ->
    {error, no_clause_matched};
match_clauses_eval([{clause, Line, Patterns, Guards, Body} = _Clause | RestClauses], ArgsAstTuple, RecordDefs) ->
    % Expand record patterns *before* checking the match
    case expand_record_patterns(Patterns, RecordDefs) of
        {ok, ExpandedPatterns} ->
            case check_clause_match_eval({clause, Line, ExpandedPatterns, Guards, Body}, ArgsAstTuple) of
                {match, _Bindings} ->
                    {ok, Line, Body}; % Pattern matched and guards passed!
                no_match ->
                    match_clauses_eval(RestClauses, ArgsAstTuple, RecordDefs); % Try next
                {error, Reason} ->
                    {error, Reason} % Error during evaluation
            end;
        {error, Reason} ->
             {error, Reason} % Error during record expansion
    end.

%% @private Expands record patterns within a list of AST patterns.
%% Returns: {ok, ExpandedPatterns :: [erl_parse:abstract_expr()]} | {error, Reason}
expand_record_patterns(Patterns, RecordDefs) ->
    try lists:map(fun(P) -> expand_single_pattern(P, RecordDefs) end, Patterns) of
        ExpandedPatterns -> {ok, ExpandedPatterns}
    catch
        throw:{record_def_not_found, RecName} -> {error, {record_def_not_found, RecName}};
        throw:{bad_record_field_pattern, Pat} -> {error, {bad_record_field_pattern, Pat}}
    end.

%% @private Expands a single pattern, focusing on records.
expand_single_pattern({match, Anno, First, Second}=A, RecordDefs) ->
        {match, Anno, expand_single_pattern(First, RecordDefs), expand_single_pattern(Second, RecordDefs)};
expand_single_pattern({record, Anno, RecName, FieldsPat}, RecordDefs) ->
    case maps:find(RecName, RecordDefs) of
        {ok, AllFieldNames} ->
            PatternFieldsMap = parse_pattern_fields(FieldsPat),
            TupleElements = [{atom, Anno, RecName} | % First element is record name atom
                             build_tuple_elements(AllFieldNames, PatternFieldsMap, RecordDefs, Anno)],
            {tuple, Anno, TupleElements}; % Replace record with tuple AST
        error -> % Record definition not found
             throw({record_def_not_found, RecName})
    end;
expand_single_pattern({tuple, Anno, Elements}, RecordDefs) -> % Recurse into tuples
    {tuple, Anno, [expand_single_pattern(E, RecordDefs) || E <- Elements]};
expand_single_pattern({cons, Anno, H, T}, RecordDefs) -> % Recurse into lists
    {cons, Anno, expand_single_pattern(H, RecordDefs), expand_single_pattern(T, RecordDefs)};
expand_single_pattern({map, Anno, Assocs}, RecordDefs) -> % Recurse into maps
    ExpandedAssocs = [expand_map_assoc(A, RecordDefs) || A <- Assocs],
    {map, Anno, ExpandedAssocs};
expand_single_pattern(OtherPattern, _RecordDefs) ->
    OtherPattern. % Keep other patterns as they are

% Helper to expand map associations
expand_map_assoc({map_field_assoc, Anno, KPat, VPat}, RecordDefs) ->
    {map_field_assoc, Anno, expand_single_pattern(KPat, RecordDefs), expand_single_pattern(VPat, RecordDefs)};
expand_map_assoc({map_field_exact, Anno, KPat, VPat}, RecordDefs) ->
     {map_field_exact, Anno, expand_single_pattern(KPat, RecordDefs), expand_single_pattern(VPat, RecordDefs)}.

% Parses the fields from a {record, ...} AST node's Fields component
% Returns: map(#{FieldName :: atom() => FieldPatternAst :: term()})
parse_pattern_fields(Fields) ->
    lists:foldl(
        fun({record_field, _, {atom, _, Name}, ValuePat}, Acc) -> Acc#{ Name => ValuePat };
           ({record_field, _, {var, _, '_'}, _ValuePat}, Acc) -> Acc; % Ignore #rec._ = ...
           (Other, _Acc) -> throw({bad_record_field_pattern, Other})
        end,
        #{},
        Fields).

% Builds the list of elements for the expanded tuple pattern
build_tuple_elements([], _PatternFieldsMap, _RecordDefs, _Anno) -> [];
build_tuple_elements([FieldName | RestFieldNames], PatternFieldsMap, RecordDefs, Anno) ->
    Element = case PatternFieldsMap of
       #{FieldName := FieldPatternAst} ->
           expand_single_pattern(FieldPatternAst, RecordDefs);
       #{} ->
           {var, Anno, '_'}
   end,
    [Element | build_tuple_elements(RestFieldNames, PatternFieldsMap, RecordDefs, Anno)].


%% @private Checks a single clause using erl_eval for pattern match and guards.
%% Assumes record patterns in Clause have already been expanded.
%% Returns: {match, Bindings} | no_match | {error, Reason}
check_clause_match_eval({clause, Line, ExpandedPatterns, Guards, _Body}, ArgsAstTuple) ->
    PatternAstTuple = {tuple, Line, ExpandedPatterns}, % Use expanded patterns
    MatchExpr = {match, Line, PatternAstTuple, ArgsAstTuple},
    InitialBindings = erl_eval:new_bindings(),
    %erlang:display({match_expr, MatchExpr}),
    try erl_eval:expr(MatchExpr, InitialBindings) of
        {value, _MatchedValue, MatchBindings} ->
            % Pattern match succeeded, now evaluate guards
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
                                no_match
                        end
                end
    catch
         ExceptionType:Reason:Stacktrace ->
             no_match % Error in erl_eval:expr/2
    end.
