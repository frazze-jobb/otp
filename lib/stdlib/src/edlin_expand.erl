%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2021. All Rights Reserved.
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
-module(edlin_expand).

%% a default expand function for edlin, expanding modules, functions
%% filepaths, variable binding, record names, function parameter values, 
%% record fields and map keys and record field values.
-include("../../kernel/include/eep48.hrl").
-export([expand/1, expand/3, format_matches/1, get_exports/1]).

-import(lists, [reverse/1, prefix/2]).

%% Extracts everything within the quote
over_to_opening_quote(Q, Bef) when Q == $'; Q == $" ->
    over_to_opening_quote([Q], Bef, [Q]);
over_to_opening_quote(_, Bef) -> {Bef, []}.
over_to_opening_quote([], Bef, Word) -> {Bef, Word};
over_to_opening_quote([Q|Stack], [Q|Bef], Word) ->
    over_to_opening_quote(Stack, Bef, [Q| Word]);
over_to_opening_quote([Q|Stack], [Q,$\\|Bef], Word) ->
    over_to_opening_quote([Stack], Bef, [$\\,Q| Word]);
over_to_opening_quote([Stack], [C|Bef], Word) ->
    over_to_opening_quote([Stack], Bef, [C| Word]);
over_to_opening_quote(_,_,_) -> {no, [], []}.

matching_paren($(,$)) -> true;
matching_paren($[,$]) -> true;
matching_paren(${,$}) -> true;
matching_paren($<,$>) -> true;
matching_paren(_,_) -> false.

%% Extracts everything within the brackets
%% Recursively extracts nested bracket expressions.
over_to_opening_paren(CC, Bef) when CC == $); CC == $];
                                     CC == $}; CC == $> ->
    over_to_opening_paren([CC], Bef, [CC]);
over_to_opening_paren(_, Bef) -> {Bef, []}. %% Not a closing parenthesis
over_to_opening_paren([], Bef, Word) -> {Bef, Word};
over_to_opening_paren([CC|Stack], [CC,$$|Bef], Word) ->
    over_to_opening_paren([CC|Stack], Bef, [$$,CC|Word]);
over_to_opening_paren([CC|Stack], [OC|Bef], Word) when OC==$(; OC==$[; OC==${; OC==$< ->
    case matching_paren(OC, CC) of
        true -> over_to_opening_paren(Stack, Bef, [OC|Word]);
        false -> over_to_opening_paren([CC|Stack], Bef, [OC|Word])
    end;
over_to_opening_paren([CC|Stack], [CC|Bef], Word) -> %% Nested parenthesis of same type
    over_to_opening_paren([CC,CC|Stack], Bef, [CC|Word]);
over_to_opening_paren(Stack, [Q|Bef], Word) when Q == $"; Q == $' ->
    %% Consume the whole quoted text, it may contain parenthesis which
    %% would have confused us.
    {Bef1, QuotedWord} = over_to_opening_quote(Q, Bef),
    over_to_opening_paren(Stack, Bef1, QuotedWord ++ Word);
over_to_opening_paren(CC, [C|Bef], Word) -> over_to_opening_paren(CC, Bef, [C|Word]).

%% Extract a whole filepath
%% Stops as soon as we hit a double quote (")
%% and returns everything it found before stopping.
%% assumes the string is not a filepath if it contains unescaped spaces
over_filepath([],_) -> {no,[],[]};
over_filepath([$"|Bef1], Filepath) -> {Bef1, Filepath};
over_filepath([32,$\\|Bef1], Filepath) -> over_filepath(Bef1, [$\\,32|Filepath]);
over_filepath([32|_], _) -> {no, [], []};
over_filepath([C|Bef1], Filepath) ->
    over_filepath(Bef1, [C|Filepath]).
split_at_last_slash(Filepath) ->
    {File, Path} = lists:splitwith(fun(X)->X/=$/ end, lists:reverse(Filepath)),
    {lists:reverse(Path), lists:reverse(File)}.
%% Extract a whole keyword expression
%% Keyword<code>end
%% Function expects a string of erlang code in reverse, and extracts everything
%% including a keyword being one of if, fun, case, maybe, receiver (need to add all here)
%% Recursively extracts nested keyword expressions
%% Note: In the future we could autocomplete case expressions by looking at the
%% return type of the expression.
over_keyword_expression(Bef) ->
    over_keyword_expression(Bef, []).
over_keyword_expression("dne"++Bef, Expr)->
    %% Nested expression
    {Bef1, KWE}=over_keyword_expression(Bef),
    over_keyword_expression(Bef1, KWE++"end"++Expr);
over_keyword_expression("fi"++Bef, Expr) -> {Bef, "if" ++ Expr};
over_keyword_expression("nuf"++Bef, Expr) -> {Bef, "fun" ++ Expr};
over_keyword_expression("esac"++Bef, Expr) -> {Bef, "case" ++ Expr};
over_keyword_expression("ebyam"++Bef, Expr) -> {Bef, "maybe" ++ Expr};
over_keyword_expression("eveicer"++Bef, Expr) -> {Bef, "receive" ++ Expr};
over_keyword_expression([], _) -> {no, [], []};
over_keyword_expression([C|Bef], Expr) -> over_keyword_expression(Bef, [C|Expr]).

over_word(Bef) ->
    {Bef1,_,_} = over_white(Bef, [], 0),
    {Bef2, Word, _} = edlin:over_word(Bef1, [], 0),
    {Bef2, Word}.

%% Check that the string given starts with a capital letter or an underscore
%% followed by a capital letter.
is_binding([X | _]) when 65 < X, X < 91 -> true;
is_binding([$_, X | _]) when 65 < X, X < 91 -> true;
is_binding(_) -> false.

keys({X, _}) ->
    [C || C <- atom_to_list(X), X /= $'].

expand_binding(Prefix, Bindings) ->
    Alts = [keys(B) || B <- Bindings],
    match(Prefix, Alts, "").

expand_record(Record, RT) ->
    Matches = ets:match(RT, '$1'),
    Records = [K || [{K,_}] <- Matches],
    match(Record, Records, "{").

%% Translates the spec AST to a structure that resembles the AST but trimmed of unneeded data.
%% User types and remote types are fetched and embedded in the structure depending on requested
%% Level of unnestling, and flattens unions.
%% Visited is used to prevent infinite loop when looking up a recursive type 
type_traverser(Mod, FunType, Nestings) ->
    type_traverser(Mod, FunType, [], length(Nestings)+1 ).
type_traverser(Mod, {type, _, bounded_fun, [Fun, Constraints]}, Visited, Level) ->
    Cl = [type_traverser(Mod,X,Visited, Level) || X <- Constraints],
    F = type_traverser(Mod, Fun, Visited, Level),
    {function, F, Cl};
type_traverser(Mod, {type, _, 'fun', [Product, Return]}, Visited, Level) ->
    P = type_traverser(Mod, Product, Visited, Level),
    R = type_traverser(Mod, Return, Visited, Level),
    {P, {return, R}};
type_traverser(Mod, {type, _, product, Childs}, Visited, Level) ->
    Cl = [type_traverser(Mod, X, Visited, Level) || X <- Childs],
    {parameters, Cl};
type_traverser(Mod, {type, _, constraint, [{atom, _, is_subtype}, [Type1, Type2]]}, Visited, Level) ->
    {constraint, type_traverser(Mod, Type1, Visited, Level), type_traverser(Mod, Type2, Visited, Level)};
type_traverser(_, {var, _, Name}, _Visited, _Level) ->
    {var, Name};
type_traverser(_Mod,{type, _, any}, _Visited, _Level) ->
    {type, any, []};
type_traverser(_Mod,{type, _, map, any}, _Visited, _Level) ->
    {type, map, []};
type_traverser(Mod, {type, _, map, Params}, Visited, Level) ->
    {map, [type_traverser(Mod, X, Visited, Level) || X <- Params]};
type_traverser(Mod, {type, _, map_field_assoc, [Type1, Type2]}, Visited, Level) ->
    {map_field_assoc, type_traverser(Mod,Type1, Visited, Level), type_traverser(Mod,Type2, Visited, Level)};
type_traverser(_Mod, {atom, _, Atom}, _Visited, _Level) when is_atom(Atom) ->
    Atom;
type_traverser(Mod, {op, _, Op, Type}, Visited, Level) ->
    {op, Op, type_traverser(Mod, Type, Visited, Level)};
type_traverser(Mod, {op, _, Op, Type1, Type2}, Visited, Level) ->
    {op, Op, type_traverser(Mod, Type1, Visited, Level), type_traverser(Mod, Type2, Visited, Level)};
type_traverser(_Mod, {integer, _, Int}, _Visited, _Level) ->
    {integer, Int};
type_traverser(Mod, {type, _, list, [ChildType]}, Visited, Level) ->
    {list, type_traverser(Mod, ChildType, Visited, Level-1)};
type_traverser(_Mod, {type, _, tuple, any}, _Visited, _Level) ->
    {type, tuple, []};
type_traverser(Mod, {type, _, tuple, ChildTypes}, Visited, Level) ->
    {tuple, [type_traverser(Mod, X, Visited, Level-1) || X <- ChildTypes]};
type_traverser(Mod, {type, _, union, ChildTypes}, Visited, Level) ->
    Childs = [type_traverser(Mod, X, Visited, Level) || X <- ChildTypes],
    ChildsFiltered = [X || X <- Childs, X/=undefined],
    {UnionChilds, NonUnionChilds} = lists:partition(
        fun(X) ->
            case X of
                {union, _} -> true;
                _ -> false
            end
        end, ChildsFiltered),
    ChildsFlattened = lists:flatten([T || {union, T} <- UnionChilds]) ++ NonUnionChilds,
    {union, ChildsFlattened};
type_traverser(Mod, {ann_type,_,[T1,T2]}, Visited, Level) ->
    {ann_type, type_traverser(Mod, T1, Visited, Level), type_traverser(Mod, T2, Visited, Level)};
type_traverser(Mod, {user_type,_,Name,Params}, Visited, Level) when 0 >= Level ->
    %% when we have level 0, do not traverse the type further and just print it
    {type, Name, [type_traverser(Mod, P, Visited, 0) || P <- Params]};
type_traverser(_, {remote_type,_,[{_,_,Mod},{_,_,Name}, Params]}, Visited, Level) when 0 >= Level ->
    %% when we have level 0, do not traverse the type further and just print it
    {type, Mod, Name, [type_traverser(Mod, P, Visited, 0) || P <- Params]};
type_traverser(Mod, {user_type,_,Name,Params}=T, Visited, Level) ->
    %% only continue if this type has not already been traversed (in case of recursive types)
    case lists:member(T, Visited) of 
        false ->
            Type = lookup_type(Mod, Name, length(Params)),
            type_traverser(Mod, Type, [T|Visited], Level);
        true -> {type, Mod, Name, [type_traverser(Mod, P, Visited, Level) || P <- Params]}
    end;
type_traverser(_, {remote_type, _, [{_,_,Mod},{_,_,Name}, Params]}=T, Visited, Level) ->
    %% only continue if this type has not already been traversed (in case of recursive types)
    case lists:member(T, Visited) of 
        false ->
            Type = lookup_type(Mod, Name, length(Params)),
            type_traverser(Mod, Type, [T|Visited], Level);
        true -> {type, Name, [type_traverser(Mod, P, Visited, Level) || P <- Params]}
    end;
type_traverser(_, {type, _, record, [{atom, _, Record}]}, _Visited, _Level) ->
    {record, Record};
type_traverser(Mod, {type, _, Name, Params}, Visited, Level) ->
    {type, Name, [type_traverser(Mod, P, Visited, Level) || P <- Params]};
type_traverser(_, hidden, _, _) ->
    {type, hidden, []}.

lookup_type(Mod, TypeName, A) ->
    case shell_docs:get_type_doc(Mod, TypeName, A) of
        [{_,_,_,_,#{signature := [{_,_,_,{_,Type,_}}]}}] -> Type;
        [] -> hidden
    end.

get_atoms(Mod, Constraints, Type, Nestings) ->
    case get_atoms1(Mod, Type, Constraints, Nestings) of
        List when is_list(List) -> [erlang:atom_to_list(Atom) || Atom <- List];
        Atom when is_atom(Atom) -> [erlang:atom_to_list(Atom)]
    end.
get_atoms1(Mod, {var, _Var}=C, Constraints, Nestings) ->
    case get_constraint(C, Constraints) of
        {constraint, _, T} -> get_atoms1(Mod, T, Constraints, Nestings);
        _ -> []
    end;
get_atoms1(Mod, {list, T}, Constraints, ['list'|Nestings]) ->
    get_atoms1(Mod, T, Constraints, Nestings);
get_atoms1(Mod, {tuple, LT}, Constraints, [{'tuple', N}|Nestings]) when length(LT) >= N ->
    %% Todo, should we check at least the first element of a tuple to guess if we are completing the correct tuple
    get_atoms1(Mod, lists:nth(N, LT), Constraints, Nestings);
get_atoms1(Mod, {tuple, [First|_]=LT}, Constraints, [{'tuple', First, N}|Nestings]) when length(LT) >= N ->
    get_atoms1(Mod, lists:nth(N, LT), Constraints, Nestings);
get_atoms1(Mod,  {union, Types}, Constraints, Nestings) ->
    Atoms = [get_atoms1(Mod, T, Constraints, Nestings) || T <- Types],
    [X || X <- lists:flatten(Atoms), X/=[]];
get_atoms1(_Mod, Atom, _Constraints, []) when is_atom(Atom) ->
    Atom;
get_atoms1(_, _, _, _) ->
    [].

get_types(Mod, Constraints, T, Nestings) ->
    case get_types1(Mod, T, Constraints, Nestings) of
        [] -> [];
        [_|_]=Types -> [Type || Type <- Types, Type /= []];
        Type -> [Type]
    end.
get_types1(Mod, {var, _Var}=C, Constraints, Nestings) ->
    case get_constraint(C, Constraints) of
        {constraint, _, T} -> get_types1(Mod, T, Constraints, Nestings);
        _ -> []
    end;
get_types1(Mod, {union, Types}, Cs, Nestings) ->
    lists:flatten([get_types1(Mod, T, Cs, Nestings) || T <- Types]);
get_types1(_, Atom, _, _) when is_atom(Atom) -> [];
get_types1(Mod, {list, T}, Cs, [list|Nestings]) ->
    get_types1(Mod, T, Cs, Nestings);
get_types1(Mod, {tuple, LT}, Cs, [{tuple, N}|Nestings]) when length(LT) >= N ->
    get_types1(Mod, lists:nth(N, LT), Cs, Nestings);
get_types1(Mod, {tuple, [First|_]=LT}, Cs, [{tuple, First, N}|Nestings]) when length(LT) >= N ->
    get_types1(Mod, lists:nth(N, LT), Cs, Nestings);
get_types1(_Mod, Type, Cs, []) ->
    {lists:flatten(print_type(Type, Cs)), ""};
get_types1(_, _, _, _) -> [].

print_type(Type, Constraints) ->
    print_type(Type, Constraints, []).
print_type({var, Name}=Var, Constraints, Visited) ->
    case lists:member(Var, Visited) of
        true -> atom_to_list(Name);
        false ->
            case get_constraint(Var, Constraints) of
                {constraint, _, T2} -> print_type(T2, Constraints, [Var| Visited]);
                _ -> atom_to_list(Name)
            end
    end;
print_type(Atom, _Cs, _V) when is_atom(Atom) -> atom_to_list(Atom);
print_type({{parameters, Ps}, {return, R}}, Cs, V) ->
    "fun("++lists:join(", ", [print_type(X, Cs, V) || X <- Ps]) ++ " -> " ++ print_type(R, Cs, V) ++ ")";
print_type({list, Type}, Cs, V)->
    "[" ++ print_type(Type, Cs, V) ++  "]";
print_type({tuple, Types}, Cs, V) when is_list(Types) ->
    Types1 = [print_type(X, Cs, V) || X <- Types],
    "{" ++ lists:join(", ", Types1) ++ "}";
print_type({ann_type, Var, Type}, Cs, V) ->
    print_type(Var, Cs, V) ++ " :: " ++ print_type(Type, Cs, V);
print_type({map, Types}, Cs, V) ->
    Types1 = [print_type(X, Cs, V) || X <- Types],
    "#{"++lists:join(", ", Types1) ++ "}";
print_type({map_field_assoc, Type1, Type2}, Cs, V) ->
    print_type(Type1, Cs, V) ++ "=>" ++ print_type(Type2, Cs, V);
print_type({integer, Int}, _Cs, _V) ->
    "int("++integer_to_list(Int)++")";
print_type({op, Op, Type}, Cs, V) ->
    "op "++atom_to_list(Op)++"("++print_type(Type, Cs, V)++")";
print_type({op, Op, Type1, Type2}, Cs, V) ->
    "op "++atom_to_list(Op)++"("++print_type(Type1, Cs, V)++", "++print_type(Type2, Cs, V) ++")";
print_type({record, Record}, _Cs, _V) ->
    "#" ++ atom_to_list(Record);
print_type({type, range, [{integer, Int1},{integer, Int2}]}, _Cs, _V) ->
    integer_to_list(Int1) ++ ".." ++ integer_to_list(Int2);
print_type({type, Name, []}, _Cs, _V) ->
    atom_to_list(Name)++"()";
print_type({type, Name, Params}, _Cs, _V) ->
    atom_to_list(Name) ++ "(" ++ lists:join(", ",[ extract_param(P) || P <- Params]) ++ ")";
print_type({_, Type}, _Cs, _V) when is_atom(Type) -> atom_to_list(Type) ++ "()";
print_type({_, {_,Type}}, _Cs, _V) when is_atom(Type) -> atom_to_list(Type) ++ "()";
print_type({union, Types}, Cs, V) ->
    lists:join(" | ", [print_type(X, Cs, V) || X <- Types]);
print_type({_, Mod, Fun, Params}, _Cs, _V) -> atom_to_list(Mod) ++ ":" ++ atom_to_list(Fun) ++
    "(" ++ lists:join(", ", [extract_param(P) || P <- Params]) ++ ")";
print_type(_,_,_) -> atom_to_list(unknown).

extract_param({var, Var}) ->
    atom_to_list(Var);
extract_param({integer, Value}) ->
    io_lib:format("~p",[Value]);
extract_param({type, Type,_}) ->
    io_lib:format("~p", [Type]);
extract_param(T)->
    print_type(T, []).

record_type_traverser(Field,
                      {attribute, _, record, 
                          {_RecordName, FieldTypes}}, Word, Nestings) ->
    FieldTypesFiltered = [Type1 || {typed_record_field, {record_field, _, {_,_, F}}, Type1} <- FieldTypes, F == Field],
    case FieldTypesFiltered of
        [] -> {no, [], []};
        [Type] ->
            T = type_traverser("erlang", Type, Nestings),
            Atoms = get_atoms("erlang", [], T, Nestings),
            {Res, CompleteChars, Matches} = match(Word, Atoms, ","),
            %% if word is empty also write additional types
            case Word of
                [] ->
                    Types = get_types("erlang", [], T, Nestings),
                    {Res, CompleteChars, Types ++ Matches};
                _ -> {Res, CompleteChars, Matches}
            end
    end.

expand_record_fields(FieldToComplete, Word, Record, Fields, RT, Nestings) ->
    Record2 = list_to_atom(Record),
    FieldSet2 = sets:from_list([list_to_atom(F) || F <- Fields]),
    FieldToComplete2 = list_to_atom(FieldToComplete),

    case ets:match(RT, {Record2, '$1'}) of
        [[RecordType|_]] ->
            case sets:is_element(FieldToComplete2, FieldSet2) of
                true -> %% expand field content,
                    record_type_traverser(FieldToComplete2, RecordType, Word, Nestings);
                false -> %% expand field
                    RecordFieldsList = extract_record_fields(Record2, RecordType),
                    RecordFieldsSet = sets:from_list(RecordFieldsList),
                    RecordFields2 = sets:subtract(RecordFieldsSet, FieldSet2),
                    match(Word, sets:to_list(RecordFields2), "=")
            end;
        _ ->
            {no, [], []}
    end.
get_constraint(Type, Constraints) ->
    case [ X || {constraint, T, _}=X <- Constraints, T == Type] of
        [C|_] -> C;
        [] -> []
    end.

%% Suggest completion for parameter at Position
get_parameter_suggest(Word, Position, {ModStr, FunStr, A}, Nestings) ->
    Mod = list_to_atom(ModStr),
    Fun = list_to_atom(FunStr),
    try shell_docs:get_doc(Mod, Fun, A) of
        [{_,_,_,_,#{ signature := [FunDoc]}}] ->
            {attribute,_,spec,{_,FunTypes}} = FunDoc, %% TODO: fold incase of multiple type specs i.e. c:memory/1
            lists:foldl(fun fold_completion_result/2, {no, [], []},
                [get_parameter_suggest1(Mod, FunType, Position, Word, Nestings) || FunType <- FunTypes]);
        _ -> {no, [], []}
    catch
        _:_ -> {no, [], []}
    end.

get_parameter_suggest1(Mod, FunType, Position, Word, Nestings) ->
    case type_traverser(Mod, FunType, Nestings) of
        {function, {{parameters, Parameters},_}, Constraints} -> 
            case lists:nth(Position, Parameters) of
                {var, Name}=ParameterVar ->
                    Atoms = get_atoms(Mod, Constraints, ParameterVar, Nestings),
                    {Res, CompleteChars, Matches} = match(Word, Atoms, ","),
                    %% If word is empty also write additional types
                    case Word of
                        [] ->
                            Types = get_types(Mod, Constraints, ParameterVar, Nestings),
                            {Res, CompleteChars, [{atom_to_list(Name) ++ " ::",""}] ++ Types ++ Matches};
                        _ -> {Res, CompleteChars, Matches}
                    end;
                _ -> {no, [], []}
            end;
        {{parameters, Parameters},_}=_F ->
            case lists:nth(Position, Parameters) of
                {ann_type, {var, Name}, T2}=_T ->
                    Types = get_types(Mod, [], T2, Nestings),
                    {no, [], [{atom_to_list(Name)++" ::", ""}]++Types};
                    %%{no, [], [{print_type(T, []),""}]}; %% code:module_status
                _ -> {no, [], []} %% beam_lib:all_chunks (type, 'fun')
            end
    end.

extract_record_fields(Record, {attribute,_,record,{Record, Fields}})->
    [X || X <- [extract_record_field(F) || F <- Fields], X /= []];
extract_record_fields(_, _)-> error.
extract_record_field({typed_record_field, {_, _,{atom, _, Field}},_})->
    Field;
extract_record_field({record_field, _,{atom, _, Field},_})->
    Field;
extract_record_field({record_field, _,{atom, _, Field}})->
    Field;
extract_record_field(_) -> [].

%% Probably a bug can be found here
fold_completion_result({yes,Cmp1,Suggest1}, {yes,Cmp1, Suggest2}) ->
    {yes, Cmp1, lists:uniq(Suggest1 ++ Suggest2)};
fold_completion_result({yes,_Cmp1,Suggest1}, {yes,_, Suggest2}) ->
    {no, [], lists:uniq(Suggest1 ++ Suggest2)};
fold_completion_result({yes,Cmp1, Suggest1}, {_, [], Suggest2}) ->
    {yes, Cmp1, lists:uniq(Suggest1 ++ Suggest2)};
fold_completion_result({_, [], Suggest1}, {yes, Cmp1, Suggest2}) ->
    {yes, Cmp1, lists:uniq(Suggest1 ++ Suggest2)};
fold_completion_result({_, [], Suggest1}, {_, [], Suggest2}) ->
    {no, [], lists:uniq(Suggest1 ++ Suggest2)}.

function_completioner(Mod, Fun, MinArity, Word, Nestings) ->
    case [A || A <- get_arities(Mod, Fun), A >= MinArity] of
            [] -> {no, [], []};
            Arities ->
                lists:foldl(fun fold_completion_result/2,
                    {no, [], []},
                    [get_parameter_suggest(Word, MinArity,{Mod, Fun, Arity}, Nestings) ||
                        Arity <- Arities])
    end.

expand_filepath(PathPrefix, Word2) ->
    Path = case PathPrefix of
        [$/|_] -> PathPrefix;
        _ ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/" ++ PathPrefix
    end,
    Entries = case file:list_dir(Path) of
        {ok, E} -> E;
        _ -> []
    end,
    case match(Word2, [".."|Entries], []) of
        {yes, Cmp, [Match]} ->
            case filelib:is_dir(Path ++ "/" ++ Word2 ++ Cmp) of
                true -> {yes, Cmp ++ "/", [Match]};
                false -> {yes, Cmp ++ "\"", [Match] }
            end;
        X -> X
    end.

-spec expand(Bef0, Bs, Rt) -> {Res, Completion, Matches} when
    Bef0 :: string(),
    Bs :: #{atom() => term()},
    Rt :: ets:tab(),
    Res :: 'yes' | 'no',
    Completion :: string(),
    Matches :: [{string(), term()}].
expand(Bef0, Bs, Rt) ->
    case {over_word(Bef0), odd_quotes($", Bef0)} of
        {_, true} ->
            %% Complete filepaths
            string_completioner(Bef0);
        {{[$#|_], Record}, _} when Record /= [] ->
            %% Complete record name
            expand_record(Record, Rt);
        {{Bef1, Word}, _} ->
            case is_binding(Word) of
                true ->
                    %% Complete a bound variable
                    expand_binding(Word, Bs);
                false ->
                    case completioner(Bef1, Word) of
                        {no, [], []} ->
                            %% Complete module or function name
                            expand(Bef0);
                        {fun_, Mod} ->
                            %% Complete function name in a fun mod: expression
                            expand_function_name(Mod, Word, "/");
                        {fun_, Mod, Fun} ->
                            %% Complete with arity of a fun mod:fun/ expression
                            Arities = [integer_to_list(A) || A <- get_arities(Mod, Fun)],
                            match(Word, Arities, ",");
                        {function, Mod, Fun, MinArity, Nesting} ->
                            %% Complete or suggest type of a function parameter
                            case Mod of
                                "shell_default" -> expand(Bef0);
                                _ -> function_completioner(Mod, Fun, MinArity, Word, Nesting)
                            end;
                        {map, MapBind, Keys} ->
                            %% Complete or suggest keys of a map
                            case proplists:get_value(list_to_atom(MapBind), Bs) of
                                Map when is_map(Map) ->
                                    K1 = sets:from_list(maps:keys(Map)),
                                    K2 = sets:subtract(K1, sets:from_list([list_to_atom(K) || K <- Keys])),
                                    match(Word, sets:to_list(K2), "=>");
                                _ -> {no, [], []}
                            end;
                        {record, Record, Fields, FieldToComplete, Nestings} ->
                            %% Complete an unfinished field
                            %% Complete or suggest type of a record field
                            expand_record_fields(FieldToComplete, Word, Record, Fields, Rt, Nestings)
                    end
            end
    end.
odd_quotes(Q, [Q,C|Line], Acc) when C == $\\; C == $$ ->
    odd_quotes(Q, Line, Acc); 
odd_quotes(Q, [Q|Line], Acc) ->
    odd_quotes(Q, Line, Acc+1);
odd_quotes(Q, [_|Line], Acc) ->
    odd_quotes(Q, Line, Acc);
odd_quotes(_, [], Acc) ->
    Acc band 1 == 1.
odd_quotes(Q, Line) ->
    odd_quotes(Q, Line, 0).


shell_default_or_erlang(Fun) ->
    case lists:member(list_to_atom(Fun), [E || {E,_}<-get_exports(shell_default)]) of
        true -> "shell_default";
        _ -> erlang(Fun)
    end.
erlang(Fun) ->
    case lists:member(list_to_atom(Fun), [E || {E,_}<-get_exports(erlang)]) of
        true -> "erlang";
        _ -> []
    end.
over_module(Bef, Fun)->
    case Bef of
        [$:|Bef1] ->
            over_word(Bef1);
        [] -> {Bef, shell_default_or_erlang(Fun)};
        _ -> {Bef, erlang(Fun)}
    end.

string_completioner(Bef0) ->
    case over_filepath(Bef0, []) of
        {_, Filepath} -> 
            {Path, File} = split_at_last_slash(Filepath),
            expand_filepath(Path, File);
        _ -> {no, [], []}
    end.

%% A function parameter or record field can have a nested type, what we mean by that is
%% that a parameter be of tuple type, and the contents of the tuple can also be of tuple type.
%% Similarly this can be said about lists. 
-type nesting() :: {'tuple', non_neg_integer()} | {'tuple', atom(), non_neg_integer()} | 'list'.
-record(completioner_record,{
    %%current_word = [] :: string(), %% The word at the cursor
    current_nesting_elements = [] :: list(string()), %% The terms before the current word within this nesting
    current_nesting_elements_count = 0:: non_neg_integer(), %% can be derived from previous
    current_field = [] :: string(), %% field of a record or key of a map field=<tab>
    first_tuple_argument = [] :: string(),
    nestings = [] :: list(nesting())}). %% the head of nesting is the first openingshould be the top of the nesting

%% completioner - basically get the context to be able to deduce how we should complete the word
%% If the word is empty, then we do not want to complete with anything if we just closed
%% a bracket, ended a quote (user has to enter , or . themselves)
%% but maybe we can help to add ',',},] depending on context in the future
completioner([C, N_Esc|_], []) when C == $"; C == $'; C == $]; C == $); C == $}, N_Esc /= $$ -> {no, [], []};
%% If the line ends with => or ->, just complete module name.
completioner(">=" ++ _, L) when is_list(L) -> {no, [], []};
completioner(">-" ++ _, L) when is_list(L) -> {no, [], []};
completioner(Bef0, _Word) when is_list(_Word) ->
    completioner(Bef0, #completioner_record{});

completioner([$(|Bef], CR) ->
    %% We have an unclosed opening parenthesis
    %% Check if we have a function call
    %% We can deduce the minimum arity based on how many terms we trimmed
    %% We can check the type of the following Term and suggest those in special cases
    %% shell_default and erlang are imported, make sure we can do expansion for those
    {Bef1, Fun} = over_word(Bef),
    {_, Mod} = over_module(Bef1, Fun),
    case Mod of
        [] -> {no, [], []};
        _ -> {function, Mod, Fun, CR#completioner_record.current_nesting_elements_count+1,
                                  CR#completioner_record.nestings}
    end;
completioner([${|Bef], #completioner_record{current_nesting_elements=Fields,
                                                   current_nesting_elements_count=Count,
                                                   current_field=FieldToComplete,
                                                   first_tuple_argument=First,
                                                   nestings=Nestings}=CR) ->
    case over_word(Bef) of
        {[$#|Bef1], []} -> %% Map
            {_, Map} = over_word(Bef1),
            {map, Map, Fields};
        {_, []} ->
            case First of
                [] -> completioner(Bef, CR#completioner_record{
                    %% We finished a nesting lets reset and read the next nesting
                    current_field = [],
                    current_nesting_elements = [],
                    current_nesting_elements_count = 0,
                    nestings = [{'tuple', Count+1}|Nestings]});
                Str -> completioner(Bef, CR#completioner_record{
                    %% We finished a nesting lets reset and read the next nesting
                    current_field = [],
                    current_nesting_elements = [],
                    current_nesting_elements_count = 0,
                    nestings = [{'tuple', list_to_atom(Str), Count+1}|Nestings]})
            end;
        {[$#|_Bef3], Record} -> %% Record
            {record, Record, Fields, FieldToComplete, Nestings};
        _ -> {no, [], []} %% Term{ <- unsupported
    end;
completioner([$[|Bef1], #completioner_record{nestings=Nestings}=CR) ->
    completioner(Bef1, CR#completioner_record{
        %% We finished a nesting lets reset and read the next nesting
        current_field = [],
        current_nesting_elements = [],
        current_nesting_elements_count = 0,
        nestings = ['list'|Nestings]});
        %% Consider saving elements of a list
        %% 
completioner([$,|Bef1], #completioner_record{
                            current_nesting_elements_count=Count,
                            current_field=FieldToComplete,
                            first_tuple_argument=_First,
                            nestings=Nestings}=CR) ->
        Field = case {FieldToComplete, Nestings} of
            %% #file_info{atime={{1,
            %% at this time nesting is [], so whenever we pass a nesting we need to reset ... to []
                {[],[]} -> "...";
                _ -> FieldToComplete
            end,
            completioner(Bef1, CR#completioner_record{
            current_field = Field,
            current_nesting_elements_count = Count+1});
completioner([$>,$=|Bef1],#completioner_record{current_nesting_elements=Fields}=CR) -> 
    {Bef2, Field}=over_word(Bef1),
        completioner(Bef2, CR#completioner_record{current_nesting_elements = [Field|Fields]});
completioner([$=|Bef1], #completioner_record{
                                        current_nesting_elements=Fields,
                                        current_field=FieldToComplete}=CR) ->
    {Bef2, Field}=over_word(Bef1),
    case FieldToComplete of
        [] -> %%[$=|_],
            completioner(Bef2, CR#completioner_record{current_nesting_elements = [Field|Fields],
                current_field = Field});
        _ -> completioner(Bef2, CR#completioner_record{current_nesting_elements = [Field|Fields]})
    end;
completioner([$.|Bef2], CR) ->
    case over_word(Bef2) of
        {[$#|_Bef3], Record} -> %% Record
            {record, Record, CR#completioner_record.current_nesting_elements, CR#completioner_record.current_field, CR#completioner_record.nestings};
        _ -> {no, [], []}
    end;
completioner([$:|Bef2], _) ->
    %% look backwards to see if its a fun
    {Bef3, Mod} = over_word(Bef2),
    case over_word(Bef3) of
        {_, "fun"} -> {fun_, Mod};
        _ -> {no, [], []}
    end;
completioner(" nuf" ++ _, _) -> {no, [], []};
completioner([$/|Bef1], _) ->
            {Bef2, Fun} = over_word(Bef1),
            {_, Mod} = over_module(Bef2, Fun),
            {fun_, Mod, Fun};
completioner([$"|Bef2], CR) ->
    %% Consume quote
    {Bef3, _Quote} = over_to_opening_quote($", Bef2),
    completioner(Bef3, CR);
completioner([$}|Bef2], CR) ->
    %% Consume tuple, map or record
    {Bef3, _Clause} = over_to_opening_paren($},Bef2),
    {Bef4, MaybeRecord} = over_word(Bef3),
    case MaybeRecord of
        [] -> case Bef4 of
            [$#|Bef5] -> completioner(Bef5, CR);
            _ -> %% Tuple, do we want to save it?
                completioner(Bef4, CR)
            end;
        _Record ->
            [$#|Bef5] = Bef4,
            {Bef6, _Var} = over_word(Bef5), %% Var#record{ or []#record{
            completioner(Bef6, CR)
    end;
completioner([$)|Bef2],CR) ->
    %% Consume parenthesis and function identifier
    {Bef3, _Clause} = over_to_opening_paren($),Bef2),
    {Bef4, Fun} = over_word(Bef3),
    {Bef5, _ModFun} = case Bef4 of
        [$:|Bef41] ->
            {Bef42, Mod} = over_word(Bef41),
            {Bef42, Mod++[$:|Fun]};
        _ -> {Bef4, Fun}
    end,
    completioner(Bef5, CR);
completioner([$]|Bef2], CR) ->
    %% Consume list
    {Bef3, _Clause} = over_to_opening_paren($],Bef2),
    completioner(Bef3, CR);
completioner([$>|Bef2], CR) ->
    %% Consume pid
    {Bef3, _Clause} = over_to_opening_paren($>,Bef2),
    completioner(Bef3, CR);
completioner("dne" ++ Bef1, CR) ->
    %% Consume keyword expression
    case over_keyword_expression(Bef1) of
        {Bef2, _} -> completioner(Bef2, CR);
        _ -> {no, [], []}
    end;

%%completioner([32|Bef2],CR) -> completioner(Bef2, CR); %% When do we need this? %%edlin_over_white
completioner(Bef0, CR) ->
    case over_word(Bef0) of
        {_Bef1, []} -> {no, [], []};
        {Bef2, Var} ->
            try list_to_integer(Var) of
                _ -> case over_fun_function(Bef0) of
                    {Bef3, "fun " ++ _ModFunArr} -> completioner(Bef3, CR);
                    _ -> completioner(Bef2, CR)
                end
            catch
                _:_ -> completioner(Bef2,
                    CR#completioner_record{first_tuple_argument = Var})
            end
    end.
over_fun_function(Bef) ->
    over_fun_function(Bef, []).
over_fun_function(Bef, Acc) ->
    case over_word(Bef) of
        {[$/|Bef1], Arity} -> over_fun_function(Bef1, [$/|Arity]++Acc);
        {[$:|Bef1], Fun} -> over_fun_function(Bef1, [$:|Fun]++Acc);
        {" nuf"++Bef1, ModOrFun} -> over_fun_function(Bef1, "fun "++ModOrFun ++ Acc);
        _ -> {Bef,Acc}
    end.

%% expand(CurrentBefore) -> {yes, Expansion, Matches} | {no, [], Matches}
%%  Try to expand the word before as either a module name or a function
%%  name. We can handle white space around the seperating ':' but the
%%  function name must be on the same line. CurrentBefore is reversed
%%  and over_word/3 reverses the characters it finds. In certain cases
%%  possible expansions are printed.
%%
%%  The function also handles expansion with "h(" for module and functions.
%%  TODO: handle expansion with "ht" for module and type
expand(Bef0) ->
    {Bef1,Word,_} = edlin:over_word(Bef0, [], 0),
    case over_white(Bef1, [], 0) of
        {[$,|Bef2],_White,_Nwh} ->
            {Bef3,_White1,_Nwh1} = over_white(Bef2, [], 0),
            {Bef4,Mod,_Nm} = edlin:over_word(Bef3, [], 0),
            case expand_function(Bef4) of
                help ->
                    expand_function_name(Mod, Word, ",");
                help_type ->
                    expand_type_name(Mod, Word, ",");
                _ ->
                    expand_module_name(Word, ",")
            end;
        {[$:|Bef2],_White,_Nwh} ->
            {Bef3,_White1,_Nwh1} = over_white(Bef2, [], 0),
            {_,Mod,_Nm} = edlin:over_word(Bef3, [], 0),
            expand_function_name(Mod, Word, "(");
        {_,_,_} ->
            CompleteChar
                = case expand_function(Bef1) of
                      help -> ",";
                      help_type -> ",";
                      _ -> ":"
                  end,
            expand_module_name(Word, CompleteChar)
    end.

expand_function("("++Str) ->
    case edlin:over_word(Str, [], 0) of
        {_,"h",_} ->
            help;
        {_,"ht",_} ->
            help_type;
        _ ->
            module
    end;
expand_function(_) ->
    module.

expand_module_name("",_) ->
    {no, [], []};
expand_module_name(Prefix,CompleteChar) ->
    Modules = [{list_to_atom(M),""} || {M,_,_} <- code:all_available()],
    ShellDefault = get_exports(shell_default),
    Erlang = get_exports(erlang),
    lists:foldl(fun fold_completion_result/2, {no, [],[]}, [ match(Prefix, Alts, CC) || {Alts,CC} <- [{Modules, CompleteChar}, {ShellDefault, "("}, {Erlang, "("}]]).
    %%match(Prefix, Modules ++ ShellDefault ++ Erlang, CompleteChar).


get_arities(ModStr, FuncStr) ->
    case to_atom(ModStr) of
        {ok, Mod} ->
            Exports = get_exports(Mod),
            lists:sort(
                [A || {H, A} <- Exports, string:equal(FuncStr, flat_write(H))]);
        error ->
            {no, [], []}
    end.

get_exports(Mod) ->
    case erlang:module_loaded(Mod) of
        true ->
            Mod:module_info(exports);
        false ->
            case beam_lib:chunks(code:which(Mod), [exports]) of
                {ok, {Mod, [{exports,E}]}} ->
                    E;
                _ ->
                    []
            end
    end.

expand_function_name(ModStr, FuncPrefix, CompleteChar) ->
    case to_atom(ModStr) of
        {ok, Mod} ->
            Exports = get_exports(Mod),
            match(FuncPrefix, Exports, CompleteChar);
        error ->
            {no, [], []}
    end.

get_module_types(Mod) ->
    case code:get_doc(Mod) of
        {ok, #docs_v1{metadata = #{types:=Types}}} ->
            Types1 = [{X, ""} || {X,_}<-maps:keys(Types)],
            Types1;
        _ -> {no, [], []}
    end.

expand_type_name(ModStr, TypePrefix, CompleteChar) ->
    case to_atom(ModStr) of
        {ok, Mod} ->
            case get_module_types(Mod) of
                {no, [], []} ->
                    {no, [], []};
                Types ->
                    match(TypePrefix, Types, CompleteChar)
            end;
        error ->
            {no, [], []}
    end.
%% if it's a quoted atom, atom_to_list/1 will do the wrong thing.
to_atom(Str) ->
    case erl_scan:string(Str) of
    {ok, [{atom,_,A}], _} ->
        {ok, A};
    _ ->
        error
    end.

match_preprocess_alt({_,_}=Alt) -> Alt;
match_preprocess_alt(X) -> {X, ""}.

match(Prefix, Alts, Extra0) ->
    Alts2 = [match_preprocess_alt(A) || A <- Alts],
    Len = string:length(Prefix),
    Matches = lists:sort(
		[{S, A} || {H, A} <- Alts2,
			   prefix(Prefix, S=flat_write(H))]),
    case longest_common_head([N || {N, _} <- Matches]) of
 	{partial, []} ->
 	    {no, [], Matches}; % format_matches(Matches)};
 	{partial, Str} ->
            case string:slice(Str, Len) of
 		[] ->
		    {yes, [], Matches}; % format_matches(Matches)};
 		Remain ->
 		    {yes, Remain, Matches}
 	    end;
 	{complete, Str} ->
	    Extra = case {Extra0,Matches} of
            {"/",[{Str,N}]} when is_integer(N) -> "/"++integer_to_list(N);
			{"(",[{Str,0}]} -> "()";
			{_,_} -> Extra0
		    end,
	    {yes, string:slice(Str, Len) ++ Extra, Matches}; %% Todo evaluate if we can return matches always, if its a single ton list, it should not be printed of we have no completion
 	no ->
 	    {no, [], []}
    end.

flat_write(T) when is_atom(T) ->
    lists:flatten(io_lib:fwrite("~tw",[T]));
flat_write(S) ->
    S.

%% Return the list of names L in multiple columns.
format_matches(L) ->
    {S1, Dots} = format_col(lists:sort(L), []),
    S = case Dots of
            true ->
                {_, Prefix} = longest_common_head(vals(L)),
                PrefixLen = string:length(Prefix),
                case PrefixLen =< 3 of
                    true -> S1; % Do not replace the prefix with "...".
                    false ->
                        LeadingDotsL = leading_dots(L, PrefixLen),
                        {S2, _} = format_col(lists:sort(LeadingDotsL), []),
                        S2
                end;
            false -> S1
        end,
    ["\n" | S].

format_col([], _) -> [];
format_col(L, Acc) ->
    LL = 79,
    format_col(L, field_width(L, LL), 0, Acc, LL, false).

format_col(X, Width, Len, Acc, LL, Dots) when Width + Len > LL ->
    format_col(X, Width, 0, ["\n" | Acc], LL, Dots);
format_col([A|T], Width, Len, Acc0, LL, Dots) ->
    {H0, R} = format_val(A),
    Hmax = LL - length(R),
    {H, NewDots} =
        case string:length(H0) > Hmax of
            true -> {io_lib:format("~-*ts", [Hmax - 3, H0]) ++ "...", true}; %%{H0, Dots};
            false -> {H0, Dots}
        end,
    Acc = [io_lib:format("~-*ts", [Width, H ++ R]) | Acc0],
    format_col(T, Width, Len+Width, Acc, LL, NewDots);
format_col([], _, _, Acc, _LL, Dots) ->
    {lists:reverse(Acc, "\n"), Dots}.

format_val({H, I}) when is_integer(I) ->
    %% If it's a tuple {string(), integer()}, we assume it's an
    %% arity, and meant to be printed.
    {H, "/" ++ integer_to_list(I)};
format_val({H, _}) ->
    {H, ""};
format_val(H) ->
    {H, ""}.

field_width(L, LL) -> field_width(L, 0, LL).

field_width([{H,_}|T], W, LL) ->
    case string:length(H) of
        L when L > W -> field_width(T, L, LL);
        _ -> field_width(T, W, LL)
    end;
field_width([H|T], W, LL) ->
    case string:length(H) of
        L when L > W -> field_width(T, L, LL);
        _ -> field_width(T, W, LL)
    end;
field_width([], W, LL) when W < LL - 3 ->
    W + 4;
field_width([], _, LL) ->
    LL.

vals([]) -> [];
vals([{S, _}|L]) -> [S|vals(L)];
vals([S|L]) -> [S|vals(L)].

leading_dots([], _Len) -> [];
leading_dots([{H, I}|L], Len) ->
    [{"..." ++ string:slice(H, Len), I}|leading_dots(L, Len)];
leading_dots([H|L], Len) ->
    ["..." ++ string:slice(H, Len)|leading_dots(L, Len)].

%% Strings are handled naively, but it should be OK here.
longest_common_head([]) ->
    no;
longest_common_head(LL) ->
    longest_common_head(LL, []).

longest_common_head([[]|_], L) ->
    {partial, reverse(L)};
longest_common_head(LL, L) ->
    case same_head(LL) of
 	true ->
 	    [[H|_]|_] = LL,
 	    LL1 = all_tails(LL),
 	    case all_nil(LL1) of
 		false ->
 		    longest_common_head(LL1, [H|L]);
 		true ->
 		    {complete, reverse([H|L])}
 	    end;
 	false ->
 	    {partial, reverse(L)}
    end.

same_head([[H|_]|T1]) -> same_head(H, T1).

same_head(H, [[H|_]|T]) -> same_head(H, T);
same_head(_, [])        -> true;
same_head(_, _)         -> false.

all_tails(LL) -> all_tails(LL, []).

all_tails([[_|T]|T1], L) -> all_tails(T1, [T|L]);
all_tails([], L)         -> L.

all_nil([]) -> true;
all_nil([[] | Rest]) -> all_nil(Rest);
all_nil(_) -> false.

%% over_white(Chars, InitialStack, InitialCount) ->
%%    {RemainingChars,CharStack,Count}.

over_white([$\s|Cs], Stack, N) ->
    over_white(Cs, [$\s|Stack], N+1);
over_white([$\t|Cs], Stack, N) ->
    over_white(Cs, [$\t|Stack], N+1);
over_white(Cs, Stack, N) when is_list(Cs) ->
    {Cs,Stack,N}.
