%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2024. All Rights Reserved.
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
-module(edlin_context_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1]).

-export([get_context/1]).

suite() ->
    [{timetrap,{minutes,1}}].
all() ->
    [get_context].
groups() ->
    [].
init_per_suite(Config) ->
    Config.
end_per_suite(_Config) ->
    ok.

get_context(_Config) ->
    TestCases = [
        {"h", {term, [], {atom, "h"}}},
        {"h(file", {term}},
        {"h(file,open", {term}},
        {"h(file,open), h", {term, [{call, "h(file,open)"}], {atom, "h"}}},
        {"h(file,open), h(file", {term}},
        {"h(file,open), h(file,open", {term}},
        {"h(file,open), h(file,open)", {term, [{call, "h(file,open)"}], {call, "h(file,open)"}}},
        {"file:", {function, "file"}},
        {"file:open", {function, "file"}},
        {"file:open(", {function, "file", "open", [], [], []}},
        {"file:open(\"", {string}},
        {"file:open(\"/", {string}},
        {"file:open(\"Word", {string}},
        {"file:open(\"\"", {function, "file", "open", [], {string, "\"\""}, []}},
        {"file:open(\"\",", {function, "file", "open", [{string, "\"\""}], [], []}},
        {"file:open(\"\",[", {function, "file", "open", [{string, "\"\""}], [], [{list, [], []}]}},
        {"file:open(\"\",{", {function, "file", "open", [{string, "\"\""}], [], [{tuple, [], []}]}},
        {"file:open(\"\",[{", {function, "file", "open", [{string, "\"\""}], [], [{list, [], []},{tuple, [], []}]}},
        {"file:open(\"\",[atom", {function, "file", "open", [{string, "\"\""}], [], [{list, [], {atom, "atom"}}]}},
        {"file:open(\"\",{atom", {function, "file", "open", [{string, "\"\""}], [], [{tuple, [], {atom, "atom"}}]}},
        {"file:open(\"\",[{atom", {function, "file", "open", [{string, "\"\""}], [], [{list, [], []},{tuple, [], {atom, "atom"}}]}},
        {"file:open(\"\",[{atom,", {function, "file", "open", [{string, "\"\""}], [], [{list, [], []},{tuple, [{atom, "atom"}], []}]}},
        {"file:open(\"\",#{ atom =>", {function, "file", "open", [{string, "\"\""}], [], [{map, ["atom"], "atom", [], []}]}},
        {"#{list", {term, [], {atom, "list"}}},
        {"{list", {term, [], {atom, "list"}}},
        {"[list", {term, [], {atom, "list"}}},
        {"M#{", {map, "M", []}},
        {"M#{key", {map, "M", []}},
        {"M#{key=>", {map, "M", ["key"]}},
        {"M#{key:=", {map, "M", ["key"]}},
        {"M#{key=>0", {map, "M", ["key"]}},
        {"M#{key=>0,", {map, "M", ["key"]}},
        {"M#{key=>0,key2=>", {map, "M", ["key", "key2"]}},
        {"#", {map_or_record}},
        {"#record{", {record, "record", [], [], [], [], []}},
        {"#record.", {record, "record", [], [], [], [], []}},
        {"#record{field", {record, "record", [], [], [], {atom, "field"}, []}},
        {"#record{field=>", {record, "record", ["field"], "field", [], [], []}},
        {"#record{field:=", {record, "record", ["field"], "field", [], [], []}},
        {"R#record{field=>0,", {record, "record", ["field"], [], [{integer, "0"}], [], []}},
        {"R#record{field=>0,field2=>[{atom,", {record, "record", ["field", "field2"], "field2", [{integer,"0"}], [], [{list, [], []},{tuple, [{atom, "atom"}], []}]}},
        {"fun", {term,[],{atom,"fun"}}},
        {"fun ", {term,[],{atom,"fun"}}},
        {"fun m", {fun_}},
        {"fun m:", {fun_, "m"}},
        {"fun m:f", {fun_, "m"}},
        {"fun m:f/", {fun_, "m", "f"}},
        {"fun m:f/1", {fun_, "m", "f"}},
        {"fun m:f/1 ", {fun_, "m", "f"}},
        {"fun m:f/1 ,", {term,[{fun_,"fun m:f/1"}],[]}},
        {"my_fun(receive X -> X end, ", {function,"user_defined","my_fun", [{keyword,"receive X -> X end"}], [], []}},
        {"my_fun(maybe X -> X end, ", {function,"user_defined","my_fun", [{keyword,"maybe X -> X end"}], [], []}},
        {"my_fun(try a end, ", {function,"user_defined","my_fun", [{keyword,"try a end"}], [], []}},
        {"my_fun(catch X -> X end, ", {function,"user_defined","my_fun", [{keyword,"catch X -> X end"}], [], []}},
        {"my_fun(try a catch _:_ -> b end, ", {function,"user_defined","my_fun", [{keyword,"try a catch _:_ -> b end"}], [], []}},
        {"my_fun(begin X end, ", {function,"user_defined","my_fun", [{keyword,"begin X end"}], [], []}},
        {"my_fun(if X -> X end, ", {function,"user_defined","my_fun", [{keyword,"if X -> X end"}], [], []}},
        {"my_fun(case X of _ -> X end, ", {function,"user_defined","my_fun", [{keyword,"case X of _ -> X end"}], [], []}},
        {"fun() -> X", {binding}},
        {"fun() -> x", {term,[],{atom,"x"}}},
        {"fun() -> 0", {term,[],{integer,"0"}}},
        {"fun() ->", {term}},
        {"?", {macro}},
        {"#{ one = a, two = b, ", {term,[{operation,"one = a"},{operation,"two = b"}],[]}},
        {"#{ one := a, two := b, ", {term,[{atom,"a"},{atom,"b"}],[]}},
        {"#{ one => a, two => b, ", {term,[{atom,"a"},{atom,"b"}],[]}},
        {"A = a, B = b, ", {term,[{operation,"A = a"},{operation,"B = b"}],[]}},
        {"#{ one = a, two = ", {term,[{operation,"one = a"}],[]}},
        {"#{ one := a, two = ", {term,[{atom,"a"}],[]}},
        {"#{ one => a, two = ", {term,[{atom,"a"}],[]}},
        {"A = a, B = ", {term,[{operation,"A = a"}],[]}},
        {"A = a", {term,[],{operation,"A = a"}}},
        {"a.", {'end'}},
        {"#record.", {record,"record",[],[],[],[],[]}},
        {"{#record.", {record,"record",[],[],[],[],[]}},
        {"#record.a", {record,"record",[],[],[],{atom,"a"},[]}},
        {"{#record.a", {record,"record",[],[],[],{atom,"a"},[]}},
        {"#record{}", {term,[],{record,"#record{}"}}},
        {"#{ a => b}", {term,[],{map,"#{ a => b}"}}},
        {"{a, tuple", {term,[{atom,"a"}],{atom,"tuple"}}},
        {"{a, tuple}", {term,[],{tuple,"{a, tuple}"}}},
        {"lists:my_fun()", {term,[],{call,"lists:my_fun()"}}},
        {"(", {term}},
        {"()", {term,[],{parenthesis,"()"}}},
        {"fun()", {new_fun,"()"}},
        {"[]", {term,[],{list,"[]"}}},
        {"fun() when a, b", {term,[{atom,"a"}],{atom,"b"}}},
        {"fun() -> a, b", {term,[{atom,"a"}],{atom,"b"}}},
        {"fun() -> a, b, ", {term,[{atom,"a"},{atom,"b"}],[]}},
        {"<1.0.1>", {term, [], {pid, "<1.0.1>"}}},
        {"#Fun<erl_eval.0.1>", {term, [], {funref, "#Fun<erl_eval.0.1>"}}},
        {"#Ref<1.0.1>", {term, [], {ref, "#Ref<1.0.1>"}}},
        {"#Port<1.0>", {term, [], {port, "#Port<1.0>"}}},
        {"<<0>>", {term, [], {binary, "<<0>>"}}},
        {"fun (X) -> X end", {term,[],{keyword,"fun (X) -> X end"}}},
        {"fun(X) -> X end", {term,[],{keyword,"fun(X) -> X end"}}},
        {"receive X -> X end", {term,[],{keyword,"receive X -> X end"}}},
        {"no_keyword -> X end", {error, '_'}},
        {"@", {term}},
        {"$@", {term,[],{char,"$@"}}},
        {"$ ", {term,[],{char,"$ "}}},
        {"1.0", {term,[],{float,"1.0"}}},
        {"10#10", {term,[],{integer,"10#10"}}},
        {"1", {term,[],{integer,"1"}}},
        {"{X", {binding}},
        {"{X, ", {term,[{var, "X"}], []}},
        {"<abc)", {error,'_'}},
        {"<abc]", {error,'_'}},
        {"<abc}", {error,'_'}},
        {"(abc>", {term}},
        {"\"\\\"\"", {term}},
        {"{\"\", $\"}", {error, '_'}},
        {"receive X -> ", {term}},
        {"foo bar", {error,'_'}},
        {"\" \" \" \"", {term,[],{operation,"\" \" \" \""}}},
        {"1+2", {term,[],{operation,"1 + 2"}}},
        {"1 andalso 2", {term,[],{operation,"1 andalso 2"}}},
        {"1 and 2", {term,[],{operation,"1 and 2"}}},
        {"1 orelse 2", {term,[],{operation,"1 orelse 2"}}},
        {"1 or 2", {term,[],{operation,"1 or 2"}}},
        {"1 =/= 2", {term,[],{operation,"1 =/= 2"}}},
        {"1 =:= 2", {term,[],{operation,"1 =:= 2"}}},
        {"1 <=> 2", {term,[],{operation,"1 <=> 2"}}},
        {"<<1>>><<2>>", {term,[],{operation,"<<1>> > <<2>>"}}},
        %{"<<1>> > <<2>>", {term,[],{operation,"<<1>> > <<2>>"}}},
        {"1 + + 2", {error,'_'}},
        {"1 -> 2", {term,[],{integer,"2"}}},
        {"receive X -> 2", {term,[],{integer,"2"}}},
        {"receive X ->", {term}},
        {"receive X -> 2, 1+3", {term,[{integer,"2"}],{operation,"1 + 3"}}},
        {"-1", {term,[],{integer,"-1"}}},
        {"-1.2", {term,[],{float,"-1.2"}}},
        {"begin {hej, svej}", {term,[],{tuple, "{hej, svej}"}}},
        {"begin {hej, svej} = {", {term,[],[]}},
        {"fun(",{fun_}},
        {"maps:map(fun(", {fun_}},
        {"/", {term}},
        {"Foo/", {term}},
        {"a/", {fun_, "user_defined", "a"}},
        {"Foo(", {term}}
    ],
    lists:foreach(fun({Input, Expected}) ->
        Result = edlin_context:get_context(lists:reverse(Input)),
            case Expected of
                {error, '_'} -> {error, _} = Result;
                _ -> ?assertEqual(Expected, Result, Input)
            end
    end, TestCases),
    ok.
