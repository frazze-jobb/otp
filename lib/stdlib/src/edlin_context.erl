%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2023. All Rights Reserved.
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
-module(edlin_context).
%% description
%%
-export([get_context/1, get_context/3, odd_quotes/2]).
-export([over_to_opening_quote/2]).
%% The context record is a structure that helps with viewing a nested expression
%% Typically we do not know the types of a tuple or a map in isolation, but if
%% we make use of the type information available for a function or a record
%% then we are able to extract more information.
%% A nesting can be either be a tuple a list or a map, more types of nestings could
%% be supported in the future. But it is for these types that we have type information.
%% We do not need to care about nested records or nested functions since the type information
%% is available for the deepest nested one.
%% The context record stores the top most nesting so far, while the nesting field contains
%% a list of nestings we found, the last element being the deepest nesting.
-type nesting() :: {'tuple', [{atom(), string()}], {atom(), string()} | []}
                 | {'list', [{atom(), string()}], {atom(), string()} | []}
                 | {'map', [string()], string(), [{atom(), string()}], {atom(), string()} | []}.
-record(context,{
                 arguments = [] :: [any()],
                 fields = [] :: [string()], %% The Field or Keys in this nesting
                 parameter_count = 0 :: non_neg_integer(), %% Number of parameters in this nesting
                 current_field = [] :: string(), %% field of a record or key of a map field=<tab> %% should be the last element in this context
                 nestings = [] :: [nesting()]}).
%% get_context - basically get the context to be able to deduce how we should complete the word
%% If the word is empty, then we do not want to complete with anything if we just closed
%% a bracket, ended a quote (user has to enter , or . themselves)
%% but maybe we can help to add ',',},] depending on context in the future
% -spec get_context2(Line) -> [Context] wxHelpEvent
%     Context :: {string, String} %% unclosed string
%              | {expression, Expression} %% an expression containing operators thats not finished?
%              | {term, FinishedTerms, Term} %% potentially finished ',' separated terms
%              | {function, Mod, Fun, FinishedArgs, UnfinishedArg, NestingsOfUnfinishedArg}
%              | {record, Name, Fields, FieldToComplete, FinishedArgs, UnfinishedArg, NestingsOfUnfinishedArg}
 
-spec get_context(Line) -> [Context] when
      Line :: string(), %% The whole line in reverse excluding Word
      Mod :: string(),
      Fun :: string(),
      Args :: list(any()),
      Unfinished :: any(),
      Binding :: string(), %% map variable
      Keys :: [string()], %% list of keys in the map MapBind
      Record :: string(),
      Fields :: [string()], %% list of keys in the record
      FieldToComplete :: string(),
      Nesting :: [nesting()] | [],
      Context :: {string}  %% cursor is inside a string "_
               | {binding} %% cursor is inside a Binding statement
               | {term}    %% cursor is at a free position, where any value is expected
               | {term, Args, Unfinished}
               | {fun_}    %% cursor is in a fun statement (either a NewVar, '(' or mfa() is expected)
               | {fun_, Mod} %% cursor is in a fun mod: statement (mfa)
               | {fun_, Mod, Fun} %% cursor is in a fun mod:fun statement
               | {new_fun, Unfinished}
               | {function}
               | {function, Mod}
               | {function, Mod, Fun, Args, Unfinished, Nesting}
               | {map, Binding, Keys}
               | {map_or_record}
               | {record}
               | {record, Record, Fields, FieldToComplete, Args, Unfinished, Nesting}
               | {error, integer()}.
get_context(Line) ->
    case odd_quotes($", Line) of
        true -> 
            {Bef0, Quote} = over_to_opening_quote($", Line),
            get_context(Bef0, #context{}, [{string, lists:droplast(Quote)}]);
        false -> get_context(Line, #context{}, [])
    end.

% get_context(Line) ->

%     {Bef0, Word} = edlin_expand:over_word(Line),
%     case {{Bef0, Word}, odd_quotes($", Bef0)} of
%         %% TODO, this only works for single word strings
%         %% What we should do is this, if odd_quotes is true, then we should
%         %% read up until the next quote, when we got the context, we should
%         %% continue reading the context until until we reach the beginning of the string
%         %% the context should be reversed before we return it.
%         %% Now we should always be able to tell what the parent context is and this should
%         %% help us make better predictions.

%         {{[$"|Bef1],_}, true} -> ParentContext = get_context(Bef1),
%         %erlang:display({parent, ParentContext}),
%             case ParentContext of
%                 {{string, String}, {map, Map, Fields}} -> 
%                     %erlang:display({string, String}),
%                     %erlang:display({map, Map, Fields}),
%                     {{string, String++"\""}, {map, Map, Fields}};
%                 _ ->  {string, ParentContext}
%             end;
%             %get_context(Bef0, Word);
%         {{[$#|_], []}, _} -> {map_or_record};
%         {{_Bef1, Word}, _} ->
%             case is_binding(Word) of
%                 true -> {binding};
%                 false -> get_context(Bef0, Word)
%             end
%     end.
% get_context(">-" ++ _, L) when is_list(L) -> {term};
% get_context([$?|_], _) ->
%     {macro};
% get_context(Bef0, Word) when is_list(Word) ->
%     get_context(lists:reverse(Word) ++ Bef0, #context{});
get_context([], #context{arguments = Args, parameter_count = Count, nestings = Nestings} = _CR, Acc) ->
    LastContext = case Count+1 == length(Args) of
        true -> [{term, lists:droplast(Args), lists:last(Args)}];
        _ ->
            %% Nestings will not end up as an argument
            Nestings1 = lists:reverse(Nestings),
            case Nestings1 of
                [] -> case Count of
                    0 when length(Args) > 0 -> [{term, lists:droplast(Args), lists:last(Args)}];
                    _ -> []
                end;
                %% TODO: what todo with multiple nestings {case_clause,[{list,[],[]},{tuple,[],[]}]}
                [{list, Args1, Arg}|_] -> [{term, Args1, Arg}];
                [{tuple, Args1, Arg}|_] -> [{term, Args1, Arg}];
                [{map, Fields, [], _Args1, _Arg}|_] -> %erlang:display({m1, M}),
                    [{map, [], Fields}] %;
                %[{map, _Fields, _FieldToComplete, Args1, Arg}|_] -> %erlang:display({m2, M}),
                %    {term, Args1, Arg}
            end
    end,
    lists:reverse(LastContext ++ Acc);
%% TODO should we ouput unfinished and such directly in the context stack, instead of accumulatiing
%% a nesting inside the context object?
get_context([$(|Bef], CR, Acc) ->
    %% We have an unclosed opening parenthesis
    %% Check if we have a function call
    %% We can deduce the minimum arity based on how many terms we trimmed
    %% We can check the type of the following Term and suggest those in special cases
    %% shell_default and erlang are imported, make sure we can do expansion for those
    {Bef1, Fun} = edlin_expand:over_word(Bef),
    case Fun of
        [] -> get_context(Bef, #context{}, [{term}|Acc]); % parenthesis
        _ ->
            {Bef2, Mod} = over_module(Bef1, Fun),
            case Mod of
                "shell" -> get_context(Bef2, #context{}, [{term}|Acc]);
                "shell_default" -> get_context(Bef2, #context{}, [{term}|Acc]);
                _ ->
                    case CR#context.parameter_count+1 == length(CR#context.arguments) of
                        true ->
                            %% N arguments N-1 commas, this means that we have an argument
                            %% still being worked on.
                            get_context(Bef2, #context{}, 
                                [{function, Mod, Fun, lists:droplast(CR#context.arguments),
                             lists:last(CR#context.arguments),CR#context.nestings}|Acc]);
                        _ ->
                            get_context(Bef2, #context{}, [{function, Mod, Fun, CR#context.arguments,
                             [], CR#context.nestings}|Acc])
                    end
            end
    end;
get_context([${|Bef], #context{ fields=Fields,
                                current_field=FieldToComplete,
                                arguments = Arguments,
                                parameter_count = Count,
                                nestings=Nestings}, Acc) ->
    {Args, Unfinished} = case Count+1 == length(Arguments) of
                             true -> {lists:droplast(Arguments), lists:last(Arguments)};
                             _ -> {Arguments, []}
                         end,
    case edlin_expand:over_word(Bef) of
        {[$#|Bef1], []} -> %% Map
            {Bef2, Map} = edlin_expand:over_word(Bef1),
            case {Map, Unfinished} of
                %% We are inside a map, but don't have the name of the map
                %% hopefully we are inside a function call which will give us context
                {[],_} -> get_context(Bef2,
                                      #context{
                                           %% We finished a nesting lets reset and read the next nesting
                                           nestings = [{'map', Fields, FieldToComplete, Args, Unfinished}|Nestings]}, Acc);
                %% We are inside a map, we know the binding of the map
                %% and we know the completed fields
                %% If we don't have an unfinished field, suggest the next field
                %% We don't really know if we have an argument or not...
                {_,[]} -> get_context(Bef2, #context{}, [{map, Map, Fields}|Acc]);
                %% If we have an unfinished field, it could be a valid key
                {_, {_Type, String}} ->
                    edlin:display(_Type),
                    get_context(Bef2, #context{}, [{string, String}, {map, Map, Fields}|Acc])
                %_ -> {term, Args, Unfinished} %% Should maybe return the value type of the map if such exist
            end;
        {_, []} ->
            get_context(Bef, #context{
                                %% We finished a nesting lets reset and read the next nesting
                                nestings = [{'tuple', Args, Unfinished}|Nestings]}, Acc);
        {[$#|Bef3], Record} -> %% Record
            get_context(Bef3, #context{}, [{record, Record, Fields, FieldToComplete, Args,
                Unfinished, Nestings}|Acc]);
        {[], _} ->
            %% TODO a{ produces {case_clause,{[],"a"} not really valid syntax, should we stop expanding, or just ignore?
            get_context(Bef, #context{
                %% We finished a nesting lets reset and read the next nesting
                nestings = [{'tuple', Args, Unfinished}|Nestings]}, Acc);
        {_, _} ->
            %% TODO a{ produces {case_clause,{[],"a"} not really valid syntax, should we stop expanding, or just ignore?
            get_context(Bef, #context{
                %% We finished a nesting lets reset and read the next nesting
                nestings = [{'tuple', Args, Unfinished}|Nestings]}, Acc)
    end;
get_context([$[|Bef1], #context{arguments = Arguments, parameter_count = Count, nestings=Nestings}, Acc) ->
    {Args, Unfinished} = case Count+1 == length(Arguments) of
                             true -> {lists:droplast(Arguments), lists:last(Arguments)};
                             _ -> {Arguments, []}
                         end,
    get_context(Bef1, #context{
                         %% We finished a nesting lets reset and read the next nesting
                         nestings = [{'list', Args, Unfinished}|Nestings]}, Acc);
get_context([$,|Bef1], #context{parameter_count=Count}=CR, Acc) ->
    get_context(Bef1, CR#context{
                        parameter_count = Count+1}, Acc);
get_context([$>,$=|Bef1], #context{ parameter_count=Count,
                                    fields=Fields}=CR, Acc) ->
    {Bef2, Field0}=extract_argument2(Bef1),
    Field = case Field0 of
        {_, Field1} -> Field1;
        _ -> Field0
    end,
    %erlang:display({field, Field, Bef2}),
    case Count of
        0 -> %% If count is 0, then we know its a value we may want to complete.
            get_context(Bef2, CR#context{fields = [Field|Fields],
                                         current_field = Field}, Acc);
        _ -> get_context(Bef2, CR#context{fields = [Field|Fields]}, Acc)
    end;
get_context([$=,$:|Bef1], #context{ parameter_count=Count,
                                    fields=Fields}=CR, Acc) ->
    {Bef2, Field}=extract_argument2(Bef1),
    case Count of
        0 -> %% If count is 0, then we know its a value we may want to complete.
            get_context(Bef2, CR#context{fields = [Field|Fields],
                                         current_field = Field}, Acc);
        _ -> get_context(Bef2, CR#context{fields = [Field|Fields]}, Acc)
    end;
get_context([$=|Bef1], #context{
                          parameter_count=Count,
                          fields=Fields}=CR, Acc) ->
    {Bef2, Field}=edlin_expand:over_word(Bef1),
    % if we are here, its always going to be 
    case Count of
        0 -> %%[$=|_],
            get_context(Bef2, CR#context{fields = [Field|Fields],
                                         current_field = Field}, Acc);
        _ -> get_context(Bef2, CR#context{fields = [Field|Fields]}, Acc)
    end;
get_context([$.|Bef2], CR, Acc) ->
    Arguments = CR#context.arguments,
    Count = CR#context.parameter_count,
    {Args, Unfinished} = case Count+1 == length(Arguments) of
                             true -> {lists:droplast(Arguments), lists:last(Arguments)};
                             _ -> {Arguments, []}
                         end,
    case edlin_expand:over_word(Bef2) of
        {[$#|Bef3], Record} -> %% Record
            get_context(Bef3, #context{},
                [{record, Record, CR#context.fields, CR#context.current_field,
                    Args, Unfinished, CR#context.nestings}|Acc]);
        _ -> get_context(Bef2, #context{}, [{'end'}|Acc])
    end;
get_context([$:|Bef2], _, Acc) ->
    %% look backwards to see if its a fun
    {Bef3, Mod} = edlin_expand:over_word(Bef2),
    case edlin_expand:over_word(Bef3) of
        {Bef4, "fun"} -> get_context(Bef4, #context{}, [{fun_, Mod}|Acc]);
        _ when Mod =:= [] -> get_context(Bef3, #context{}, [{function}|Acc]);
        _ -> get_context(Bef3, #context{}, [{function, Mod}|Acc])
    end;
get_context([$/|Bef1], _, Acc) ->
    {Bef2, Fun} = edlin_expand:over_word(Bef1),
    {Bef3, Mod} = over_module(Bef2, Fun),
    get_context(Bef3, #context{}, [{fun_, Mod, Fun}|Acc]);
get_context([$>,$-|Bef], #context{arguments = Args} = CR, Acc) ->
    %% Inside a function or case/catch/receive expression
    %% TODO, we should read that context, and continue reading upwards
    %% in the context stack
    case CR#context.parameter_count+1 == length(Args) of
        true ->
            get_context(Bef, #context{}, [{term, lists:droplast(Args), lists:last(Args)}|Acc]);
        _ -> get_context(Bef, #context{}, [{term, Args, []}|Acc])
    end;
get_context([$;|Bef1], #context{arguments = Args}=CR, Acc) ->
    %% Separator of a guard clause, or some other clause
    %% in this case we want to suggest a pattern match for the next clause
    %% so we have to read up until the clause opener, (when, call_fun\(\), catch, after, receive, else, case_of)
    {Bef2, ClauseOpenerKeyWord, Clauses} = over_to_clause_opener(Bef1),
    case CR#context.parameter_count+1 == length(Args) of
        true ->
            get_context(Bef2, #context{}, [{ClauseOpenerKeyWord, Clauses},{term, lists:droplast(Args), lists:last(Args)}|Acc]);
        _ -> get_context(Bef2, #context{}, [{ClauseOpenerKeyWord, Clauses},{term, Args, []} |Acc])
    end;
get_context("nehw " ++ Bef, #context{arguments = Args} = CR, Acc) ->
    %% Inside a guard
    case CR#context.parameter_count+1 == length(Args) of
        true ->
            get_context(Bef, #context{}, [{term, lists:droplast(Args), lists:last(Args)}|Acc]);
        _ -> get_context(Bef, #context{}, [{term, Args, []}|Acc])
    end;
get_context([$\ |Bef],CR, Acc) -> get_context(Bef, CR, Acc); %% matching space here simplifies the other clauses
get_context(Bef0, #context{arguments=Args, parameter_count=Count} = CR, Acc) ->
    case over_to_opening(Bef0) of
        {Bef1,[]} -> get_context(Bef1, #context{}, [{term}|Acc]);
        {error, _}=E -> E;
        {Bef1, record} -> 
            case get_context(Bef1) of
                {function, Mod, _, _, _, _} ->
                    get_context(Bef1, #context{}, [{record, Mod}|Acc]);
                _ ->
                    get_context(Bef1, #context{}, [{record}|Acc])
            end;
        {Bef1, fun_} -> get_context(Bef1, #context{}, [{fun_}|Acc]);
        {Bef1, {new_fun, _}}=F -> get_context(Bef1, #context{}, [F|Acc]);
        {Bef1, {fun_, Str}=Arg} ->
            case Count of
                0 ->
                    [_, Mod, Fun| _] = string:tokens(Str, " :/"),
                    get_context(Bef1, #context{}, [{fun_, Mod, Fun}|Acc]);
                _ -> get_context(Bef1, CR#context{arguments=[Arg|Args]}, Acc)
            end;
        {Bef1, Arg} -> get_context(Bef1, CR#context{arguments=[Arg|Args]}, Acc)
    end.

read_operator(Bef) ->
    read_operator1(Bef).

operator_string() -> "-=><:+*/|&^~".
read_operator1([$\s|Bef]) -> read_operator1(Bef);
read_operator1("mer " ++ Bef) -> {Bef, "rem"};
read_operator1("osladna " ++ Bef) -> {Bef, "andalso"};
read_operator1("dna " ++ Bef) -> {Bef, "and"};
read_operator1("eslero " ++ Bef) -> {Bef, "orelse"};
read_operator1("ro " ++ Bef) -> {Bef, "or"};
read_operator1([$>,$>,$>|Bef1]) -> {[$>,$>|Bef1], [$>]}; %% comparison with a binary or typo
read_operator1([$>,$>|Bef1]) -> {[$>|Bef1], [$>]}; %% comparison with a pid, or binary
read_operator1([$>,$-, C|Bef1]=Bef) ->
    case lists:member(C, operator_string()) of
        true -> {Bef1, [C,$-,$>]};
        false -> {Bef, []}
    end;
read_operator1([$>,$=, C|Bef1]=Bef) ->
    case lists:member(C, operator_string()) of
        true -> {Bef1, [C,$=,$>]};
        false -> {Bef, []}
    end;
read_operator1([$=,$:, C|Bef1]=Bef) ->
    case lists:member(C, operator_string()) of
        true -> {Bef1, [C,$:,$=]};
        false -> {Bef, []}
    end;
read_operator1([$:|_]=Bef) -> {Bef, []}; %% this operator does not count
read_operator1([Op1,Op2,Op3|Bef])->
    case {lists:member(Op1, operator_string()),
         lists:member(Op2, operator_string()),
         lists:member(Op3, operator_string())} of
        {true, true, true} -> {Bef, [Op3, Op2, Op1]};
        {true, true, false} -> {[Op3|Bef], [Op2, Op1]};
        {true, false, _} -> {[Op2,Op3|Bef], [Op1]};
        _ -> {[Op1,Op2,Op3|Bef], []}
    end;
read_operator1([Op1, Op2]) ->
    case {lists:member(Op1, operator_string()),
         lists:member(Op2, operator_string())} of
        {true, true} -> {[], [Op2, Op1]};
        {true, false} -> {[Op2], [Op1]};
        _ -> {[Op1,Op2], []}
    end;
read_operator1([Op1]) ->
    case lists:member(Op1, operator_string()) of
        true -> {[], [Op1]};
        _ -> {[Op1], []}
    end;
read_operator1(Bef) -> {Bef, []}.

read_opening_char("nehw "++Bef) ->
    {Bef, "when"};
read_opening_char([OC|Bef]) when OC =:= $(; OC =:= $[; OC =:= ${; OC =:= $,; OC =:= $. ->
    {Bef, [OC]};
read_opening_char([$>,$-|_]=Bef) ->
    case read_operator(Bef) of
        {_, []} -> {Bef, "->"};
        _ -> {Bef, []}
    end;
read_opening_char([$\ |Bef]) -> read_opening_char(Bef);
read_opening_char(Bef) -> {Bef, []}.

over_to_opening(Bef) -> try
        over_to_opening1(Bef,#{args => []})
    catch
        throw:E -> E
    end.
over_to_opening1([], #{'args' := Args}) ->
    over_to_opening_return([], Args);
over_to_opening1(Bef, Acc = #{args := Args}) ->
    case edlin_expand:over_word(Bef) of
        {_, []} -> %% removed spaces
            case read_opening_char(Bef) of
                {Bef1, []} -> %% not an opening
                    case extract_argument2(Bef1) of
                        {stop} -> over_to_opening_return(Bef1, Args);
                        {Bef2, []} -> over_to_opening_return(Bef2, Args);
                        {Bef2, Arg} -> over_to_opening1(Bef2, Acc#{args => [Arg | Args]})
                    end;
                {_Bef1, _Opening} -> over_to_opening_return(Bef, Args)
            end;
        _ -> case extract_argument2(Bef) of
                {stop} -> over_to_opening_return(Bef, Args);
                {Bef2, []} -> over_to_opening_return(Bef2, Args);
                {Bef2, Arg} -> over_to_opening1(Bef2, Acc#{args => [Arg | Args]})
            end
    end.
over_to_opening_return(Bef, Args) ->
    case Args of
        [] -> {Bef, []};
        [Arg] -> {Bef, Arg};
        [{operator, "-"}, {integer, I}] -> {Bef, {integer, "-" ++ I}};
        [{operator, "-"}, {float, F}] -> {Bef, {float, "-" ++ F}};
        [{atom, "fun"}, {atom, _}] -> throw({Bef, fun_});
        _ ->
            case look_for_non_operator_separator(Args) of
                true ->
                    Operation = case lists:flatten(lists:join(" ", lists:map(fun({_, Arg}) -> Arg end, Args))) of
                        [$<,$<,$\s|Rest] -> "<<"++Rest;
                        R -> R
                    end,
                    {Bef, {operation, Operation}};
                false -> {error, length(Bef)}
            end
    end.
look_for_non_operator_separator([{string, _},{string, _}=A|Args]) ->
    look_for_non_operator_separator([A|Args]);
look_for_non_operator_separator([{operator, _}, {operator, _}|_]) -> false;

look_for_non_operator_separator([_, {operator, _}=B|Args]) ->
    look_for_non_operator_separator([B|Args]);
look_for_non_operator_separator([{operator, _}, B|Args]) ->
    look_for_non_operator_separator([B|Args]);
look_for_non_operator_separator([_]) -> true;
look_for_non_operator_separator(_) -> false.

over_map_record_or_tuple(Bef0) ->
    case over_to_opening_paren($},Bef0) of
        {_, []} -> %% no matching {
            throw({error, length(Bef0)});
        {Bef3, Clause} ->
            {Bef4, MaybeRecord} = edlin_expand:over_word(Bef3),
            case MaybeRecord of
                [] -> case Bef4 of
                        [$#|Bef5] -> %% Map
                            {Bef6, _Var} = edlin_expand:over_word(Bef5),
                            {Bef6, {map, _Var++"#"++Clause}};
                        _ -> %% Tuple
                            {Bef4, {tuple, Clause}}
                    end;
                _Record -> %% Record
                    [$#|Bef5] = Bef4,
                    {Bef6, _Var} = edlin_expand:over_word(Bef5),
                    {Bef6, {record, _Var++"#"++_Record++Clause}}
        end
    end.
over_pid_port_or_ref(Bef2) ->
        %% Extracts argument or part of an operation
        %% Consume Pid, Ref, FunRef or Binary
        case over_to_opening_paren($>,Bef2) of
            {_, []} -> %% no matching <, maybe a '>' operator
                throw({soft_error, length(Bef2)});
            {Bef3, Clause} ->
                case Bef3 of
                    "feR#" ++ Bef4 ->
                        {Bef4, {ref, "#Ref" ++ Clause}};
                    "nuF#" ++ Bef4 ->
                        {Bef4, {'funref', "#Fun" ++ Clause}};
                    "troP#" ++ Bef4 ->
                        {Bef4, {port, "#Port" ++ Clause}};
                    _ -> case edlin_expand:over_word(Bef3) of
                        {Bef3, []} ->
                            case Bef2 of
                                [$>|_] -> %% binary
                                    {Bef3, {binary, Clause}};
                                _ -> %% pid
                                    %% match <Num.Num.Num>
                                    {Bef3, {pid, Clause}}
                            end;
                        _ ->
                            throw({error, length(Bef3)})
                        end
                end
        end.
over_list(Bef2) ->
    case over_to_opening_paren($],Bef2) of
            {_, []} -> %% no matching [
                throw({error, length(Bef2)});
            {Bef3, Clause} ->
        {Bef3, {list, Clause}}
    end.
over_parenthesis_or_call(Bef2) ->
    case over_to_opening_paren($),Bef2) of
            {_, []} -> %% no matching (
                throw({error, length(Bef2)});
            {Bef3, Clause} ->
                {Bef4, Fun} = edlin_expand:over_word(Bef3),
                {Bef5, ModFun} = case Bef4 of
                                    [$:|Bef41] ->
                                        {Bef42, Mod} = edlin_expand:over_word(Bef41),
                                        {Bef42, Mod++[$:|Fun]};
                                    _ -> {Bef4, Fun}
                                end,
                case {ModFun, is_binding(ModFun)} of
                    {[],_} -> {Bef5, {parenthesis, Clause}};
                    {"fun",_} -> throw({Bef5, {new_fun, Clause}});
                    {Binding, true} -> 
                        {Bef6, Fun2} = edlin_expand:over_word(Bef5),
                        case Fun2 of
                            "fun" -> throw({Bef6, {new_fun, Binding, Clause}});
                            _ -> throw({error, length(Bef6)})
                        end;
                    {_,_} -> {Bef5, {call, ModFun++Clause}}
                end
    end.
over_keyword_or_fun(Bef1) ->
    case over_keyword_expression(Bef1) of
        {Bef2, KeywordExpression} -> {Bef2, {keyword, KeywordExpression ++ " end"}};
        _ -> throw({error, length(Bef1)})
    end.
extract_argument2([$\s|Bef0])->extract_argument2(Bef0);
extract_argument2([$>|_]=Bef)->
    case read_operator(Bef) of
        {[$>|_]=Bef1, ">"=Operator} ->
            try over_pid_port_or_ref(Bef1)
            catch
                %% not a pid, port, ref or binary
                throw:{error, _}=E -> throw(E);
                throw:{soft_error, _Col} -> {Bef1, {operator, Operator}}
            end;
        {Bef1, ">"=Operator} ->
            try over_pid_port_or_ref(Bef1)
            catch
                %% not a pid, port or ref
                throw:{error, _}=E -> throw(E);
                throw:{soft_error, _Col} -> {Bef1, {operator, Operator}}
            end;
        {_Bef1, []} -> {stop};
        {Bef1, Operator} -> {Bef1, {operator, Operator}}
    end;
extract_argument2(Bef0) ->
    case read_operator(Bef0) of
        {[$}|Bef1], []} -> over_map_record_or_tuple(Bef1);
        {[$)|Bef1], []} -> over_parenthesis_or_call(Bef1);
        {[$]|Bef1], []} -> over_list(Bef1);
        {[$"|Bef2], []} -> {Bef3, _Quote} = over_to_opening_quote($", Bef2),
            case Bef3 of
                [$"|Bef2] -> {Bef2, {string, _Quote}};
                _ -> {Bef3, {string, _Quote}}
            end;
        {"dne "++Bef1, []} -> over_keyword_or_fun(Bef1);
        {[$=,$:|_], []} -> {stop};
        {[$:|_], []} -> {stop};
        {"nehw" ++ _Bef1,[]} -> {stop};
        {_, []} -> extract_argument(Bef0);
        {Bef1, Operator} -> 
            {Bef1, {operator, Operator}}
    end.

extract_argument(Bef0) ->
    %% TODO: We probably need to be able to extract Terms with operators...
    case edlin_expand:over_word(Bef0) of
        {_Bef1, []} ->
            case read_char(_Bef1) of
                {_, []} -> {_Bef1, []};
                {Bef2, Char} -> {Bef2, {char, Char}}
            end;
        {Bef2, Var} ->
            try list_to_integer(Var) of
                _ -> %% there is an integer
                    case over_fun_function(Bef0) of
                         {Bef3, "fun " ++ _ModFunArr} -> {Bef3, {fun_, "fun "++_ModFunArr}};
                         _ -> case over_number(Bef0) of
                            {Bef3, []} -> {Bef3, []}; %% how to deal with operators
                            {Bef3, Number} -> {Bef3, Number}

                         end
                    end
            catch
                _:_ ->
                    case is_binding(Var) of
                        true -> {Bef2,{var, Var}};
                        false -> case Bef2 of
                            [$#|Bef3] -> throw({Bef3, record});
                            _ -> {Bef2, {atom, Var}}
                        end
                    end
            end
    end.
over_number(Bef) ->
    case edlin_expand:over_word(Bef) of
        {_, []} -> {Bef, []}; 
        {Bef2, Var} ->
            try list_to_integer(Var) of
                _ ->
                    {Bef6, {NumberType, Number}}=Res = case edlin_expand:over_word(Bef2) of
                        {[$.|Bef3],[]} -> %% float
                            {Bef4, Integer} = edlin_expand:over_word(Bef3),
                            {Bef4, {float, Integer ++ "." ++ Var}};
                        {[$#|Bef3],[]} -> %% integer base
                            {Bef4, Base} = edlin_expand:over_word(Bef3),
                            {Bef4, {integer, Base ++ "#" ++ Var}};
                        _ ->
                            {Bef2, {integer, Var}}
                        %% otherwise its an operation that can be very complicated we should read everything up to the closest CC
                        %% and return an {operation, Clause}
                    end,
                    case edlin_expand:over_word(Bef6) of
                        {[$-|Bef5], []} -> 
                            case read_opening_char(Bef5) of
                                {_, []} -> Res;
                                _ -> {Bef5, {NumberType, "-" ++Number}}
                            end;
                        _ -> Res
                    end
            catch
                _:_ -> {Bef, []}
            end
    end.
read_char([C,$$|Line]) ->
    {Line, [$$,C]};
read_char([$$|Line]) ->
    {Line, "$ "};
read_char(Line) ->
    {Line, []}.

over_fun_function(Bef) ->
    over_fun_function(Bef, []).
over_fun_function(Bef, Acc) ->
    case edlin_expand:over_word(Bef) of
        {[$/|Bef1], Arity} -> over_fun_function(Bef1, [$/|Arity]++Acc);
        {[$:|Bef1], Fun} -> over_fun_function(Bef1, [$:|Fun]++Acc);
        {" nuf"++Bef1, ModOrFun} -> over_fun_function(Bef1, "fun "++ModOrFun ++ Acc);
        _ -> {Bef,Acc}
    end.

%% Extracts everything within the quote
over_to_opening_quote(Q, Bef) when Q == $'; Q == $" ->
    over_to_opening_quote([Q], Bef, [Q]);
over_to_opening_quote(_, Bef) -> {Bef, []}.
over_to_opening_quote([], Bef, Word) -> {Bef, Word};
over_to_opening_quote([Q|Stack], [Q|Bef], Word) ->
    over_to_opening_quote(Stack, Bef, [Q| Word]);
over_to_opening_quote([Q|Stack], [Q,EC|Bef], Word) when EC=:=$\\; EC=:=$$ ->
    over_to_opening_quote([Q|Stack], Bef, [EC,Q| Word]);
over_to_opening_quote([Stack], [C|Bef], Word) ->
    over_to_opening_quote([Stack], Bef, [C| Word]);
over_to_opening_quote(_,_,Word) -> {lists:reverse(Word), []}.

matching_paren("\"\"\"","\"\"\"") -> true;
matching_paren("\"","\"") -> true;
matching_paren("'","'") -> true;
matching_paren("<<",">>") -> true;
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
over_to_opening_paren(_, [], Word) -> {lists:reverse(Word), []}; %% Not a closing parenthesis
over_to_opening_paren([CC|Stack], [CC,$$|Bef], Word) ->
    over_to_opening_paren([CC|Stack], Bef, [$$,CC|Word]);
over_to_opening_paren([CC|Stack], [OC|Bef], Word) when OC==$(; OC==$[; OC==${; OC==$< ->
    case matching_paren(OC, CC) of
        true -> over_to_opening_paren(Stack, Bef, [OC|Word]);
        false -> over_to_opening_paren([CC|Stack], Bef, [OC|Word])
    end;
over_to_opening_paren([CC|Stack], [CC|Bef], Word) -> %% Nested parenthesis of same type
    over_to_opening_paren([CC,CC|Stack], Bef, [CC|Word]);
over_to_opening_paren(Stack, [Q,NEC|Bef], Word) when Q == $"; Q == $', NEC /= $$, NEC /= $\\ ->
    %% Consume the whole quoted text, it may contain parenthesis which
    %% would have confused us.
    {Bef1, QuotedWord} = over_to_opening_quote(Q, [NEC|Bef]),
    over_to_opening_paren(Stack, Bef1, QuotedWord ++ Word);
over_to_opening_paren(CC, [C|Bef], Word) -> over_to_opening_paren(CC, Bef, [C|Word]).


%% patterns we should support
%% some:fun(X) when hej; svej -> yolo;
%% |
%% fun F(X) when hej; svej -> yolo;
%% |
%% some:fun(X) when hej,yolo; svej;|
%% case Expr of
%%   P when hej; svej -> yolo;
%%   |
%% should return {Bef, {"case", Expr, "of"}, [{"P", ["hej", "svej"], "yolo"}]} 
%% case Expr of
%%   P when hej,yolo; svej;|

over_to_clause_opener(Bef) ->
    case over_to_clause_opener(Bef, [], []) of
        {[],[],_} -> {Bef, [], []}; %% TODO maybe return an error?
        R -> R
    end.
over_to_clause_opener([], Clause, Clauses) -> {[], [], [Clause|Clauses]};
over_to_clause_opener(">-"++Bef, Clause, Clauses) ->
    %% Check if we have -> ...; or clause_opener Expr1 when Expr2;[Expr3]
    %% otherwise we will have only Expr1
    {Bef1, Guards} = case over_to_clause_opener(Bef, [], []) of
        {Bef_, "when", Guards_} -> {Bef_, Guards_};
        _ -> {Bef, []}
    end,
    {Bef2, P} = extract_argument2(Bef1),
    over_to_clause_opener(Bef2, [], [{P, Guards, Clause}|Clauses]);
over_to_clause_opener([$"|Bef], Clause, Clauses) ->
    {Bef1, Quote} = over_to_opening_quote($", Bef),
    over_to_clause_opener(Bef1, Quote ++ Clause, Clauses);
over_to_clause_opener([$;|Bef], Clause, Clauses) ->
    over_to_clause_opener(Bef, [], [Clause|Clauses]);
over_to_clause_opener([C|Rest]=Bef, Clause, Clauses) ->
    {Bef1, ClauseOpener} = is_clause_opener(Bef),
    case ClauseOpener of
        [] -> over_to_clause_opener(Rest, [C|Clause], Clauses);
        _ -> {Bef1, ClauseOpener, [Clause|Clauses]}
    end.

is_clause_opener(Bef) ->
    case edlin_expand:over_word(Bef) of
        {Bef1, Keyword} ->
            case lists:member(Keyword, [
                "when","after", "else", "catch", "receive",
                "try", "of", "fun", "if"]) of
                true ->
                    case Keyword of  
                        "of" ->
                            %% Read everything between case and of
                            {Bef2, "case "++Expr} = over_keyword_expression(Bef1, []),
                            {Bef2, {"case", Expr, "of"}};
                        KW -> {Bef1, KW}
                    end;
                false -> {Bef, []}
            end;
        _ -> {Bef, []}
    end.
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
over_keyword_expression("yrt"++Bef, Expr) -> {Bef, "try" ++ Expr};
over_keyword_expression("esac"++Bef, Expr) -> {Bef, "case" ++ Expr};
over_keyword_expression("hctac"++Bef, Expr) ->
    case over_keyword_expression(Bef, []) of
        {Bef1, "try" ++ Expr1} -> {Bef1, "try" ++ Expr1 ++ "catch" ++ Expr};
        _ -> {Bef, "catch" ++ Expr}
    end;
over_keyword_expression("nigeb"++Bef, Expr) -> {Bef, "begin" ++ Expr};
over_keyword_expression("ebyam"++Bef, Expr) -> {Bef, "maybe" ++ Expr};
over_keyword_expression("eviecer"++Bef, Expr) -> {Bef, "receive" ++ Expr};
over_keyword_expression([], _) -> {no, [], []};
over_keyword_expression([C|Bef], Expr) -> over_keyword_expression(Bef, [C|Expr]).

odd_quotes(Q, [Q,C|Line], Acc) when C == $\\; C == $$ ->
    odd_quotes(Q, Line, Acc);
odd_quotes(Q, [Q|Line], Acc) ->
    odd_quotes(Q, Line, Acc+1);
odd_quotes(Q, [_|Line], Acc) ->
    odd_quotes(Q, Line, Acc);
odd_quotes(_, [], Acc) -> Acc band 1 == 1.
odd_quotes(Q, Line) ->
    odd_quotes(Q, Line, 0).

over_module(Bef, Fun)->
    case edlin_expand:over_word(Bef) of
        {[$:|Bef1], _} ->
            edlin_expand:over_word(Bef1);
        {[], _} -> {Bef, edlin_expand:shell_default_or_bif(Fun)};
        _ -> {Bef, edlin_expand:bif(Fun)}
    end.

%% Check that the given string starts with a capital letter, or an underscore
%% followed by an alphanumeric grapheme.
is_binding(Word) ->
    Normalized = unicode:characters_to_nfc_list(Word),
    nomatch =/= re:run(Normalized,
                       "^[_[:upper:]][[:alpha:][:digit:]]*$",
                       [unicode, ucp]).
