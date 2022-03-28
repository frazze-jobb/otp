%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2021. All Rights Reserved.
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
-module(edlin_expand_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2,
	 init_per_group/2,end_per_group/2]).
-export([normal/1, type_completion/1, quoted_fun/1, quoted_module/1, quoted_both/1, erl_1152/1,
         erl_352/1, unicode/1, filename_completion/1, binding_completion/1, record_completion/1,
         map_completion/1, function_parameter_completion/1, fun_completion/1]).
-record(a_record,
        {a_field   :: atom1 | atom2 | btom | 'my atom' | {atom3, {atom4, non_neg_integer()}} | 'undefined',
         b_field   :: boolean() | 'undefined',
         c_field   :: list(term()) | 'undefined',
         d_field   :: non_neg_integer() | 'undefined'}).
-include_lib("common_test/include/ct.hrl").

init_per_testcase(_Case, Config) ->
    cleanup(),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [normal, type_completion, filename_completion, binding_completion,
     record_completion, fun_completion, map_completion, function_parameter_completion,
     quoted_fun, quoted_module, quoted_both, erl_1152, erl_352,
     unicode].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

cleanup() ->
    [try
         code:purge(M),
         code:delete(M)
     catch _:_ -> ok end || M <- [expand_test, expand_test1, expand_function_parameter,
                                  'ExpandTestCaps', 'ExpandTestCaps2']].

normal(Config) when is_list(Config) ->
    {module,expand_test} = compile_and_load(Config,expand_test),
    %% These tests might fail if another module with the prefix
    %% "expand_" happens to also be loaded.
    {yes, "test:", [{"expand_test",""}]} = do_expand("expand_"),
    {no, [], []} = do_expand("expandXX_"),
    {no,[],
     [{"a_fun_name",1},
      {"a_less_fun_name",1},
      {"b_comes_after_a",1},
      {"expand0arity_entirely",0},
      {"module_info",0},
      {"module_info",1}]} = do_expand("expand_test:"),
    {yes,[],[{"a_fun_name",1},
         {"a_less_fun_name",1}]} = do_expand("expand_test:a_"),
    {yes,"arity_entirely()",[{"expand0arity_entirely",0}]} = do_expand("expand_test:expand0"),
    ok.

to_atom(Str) ->
    case erl_scan:string(Str) of
    {ok, [{atom,_,A}], _} ->
        {ok, A};
    _ ->
        error
    end.

type_completion(_Config) ->
    %% test that edlin_expand:type_traverser works for all preloaded modules in the shell
    LoadedModules = [to_atom(M) || {M,_,_} <- code:all_available()],
    ModExports = [{Mod, edlin_expand:get_exports(Mod)} || {ok, Mod} <- LoadedModules],
    lists:map(
        fun({Mod, Exports}) ->
            lists:map(
                fun({Func, _}) ->
                    Str = atom_to_list(Mod) ++ ":" ++ atom_to_list(Func) ++ "(",
                    io:format("~p~n", [Str]),
                    do_expand(Str)
                end, Exports)
        end, ModExports),
    ok.

filename_completion(Config) ->
    %% test that filenames can be completed
    {yes,"ll\"", [{"null",""}]} = do_expand("\"/dev/nu"),
    {yes,[],[{"stderr",[]},{"stdin",[]},{"stdout",[]}]} = do_expand("\"/dev/std"),
    Dir = proplists:get_value(data_dir,Config),
    {yes,"erl\"",[{"complete_function_parameter.erl",""}]} = do_expand("\""++Dir++"complete_function_parameter."),
    {no,[],[]} = do_expand("\"\""),
    ok.

record_completion(_Config) ->
    %% test record completion for loaded records
    %% test record field name completion
    %% test record field completion
    {yes,"ord{", [{"a_record",""}]} = do_expand("#a_rec"),
    {no, [], [{"a_field",_}, {"b_field",_}, {"c_field",_}, {"d_field",_}]} = do_expand("#a_record{"),
    {no, [], [{"a_field",_}, {"b_field",_}, {"c_field",_}, {"d_field",_}]} = do_expand("#a_record."),
    {yes,"eld=", [{"a_field",""}]} = do_expand("#a_record{a_fi"),
    {no, [], [ {"{atom3, {atom4, non_neg_integer()}}",[]},
    {"atom1",[]},
    {"atom2",[]},
    {"btom",[]},
    {"my atom",[]} ]} = do_expand("#a_record{a_field="),
    %% test that an already specified field does not get suggested again
    {no,[],[{"b_field",[]},{"c_field",[]},{"d_field",[]}]} = do_expand("#a_record{a_field=1,"),
    {yes, "atom3,", [{"atom3",""}]} = do_expand("#a_record{a_field={"),
    {no,[],[{"{atom4, non_neg_integer()}",[]}]} = do_expand("#a_record{a_field={atom3,"),
    {no,[],[{"b_field",[]},{"c_field",[]},{"d_field",[]}]} = do_expand("#a_record{a_field={atom3,b},"),
    ok.

fun_completion(_Config) ->
    {yes, "/1", [{"unzip3",1}]} = do_expand("fun lists:unzip3"),
    {no, [], []} = do_expand("fun lists:unzip3/1,"),
    ok.

binding_completion(_Config) ->
    %% test that bindings in the shell can be completed
    {yes,"ding",[{"Binding",""}]} = do_expand("Bin"),
    {yes,"ding",[{"Binding",""}]} = do_expand("file:open(Bin"),
    {yes,"ding",[{"Binding",""}]} = do_expand("fun (X, Y) -> Bin"),
    {yes,"öndag", [{"Söndag",""}]} = do_expand("S"),
    ok.

map_completion(_Config) ->
    %% test that key suggestion works for a known map in bindings
    {no,[],[{"a_key",[]},{"b_key",[]},{"c_key",[]}]} = do_expand("MapBinding#{"),
    {yes, "_key=>", [{"b_key",""}]} = do_expand("MapBinding#{b"),
    %% test that an already specified key does not get suggested again
    {no, [], [{"a_key",_},{"c_key", _}]} = do_expand("MapBinding#{b_key=>1,"),
    %% test that unicode works
    ok.

function_parameter_completion(Config) ->
    %% test first and second parameter
    %% test multiple arities with same type on first parameter
    %% test multiple arities with different type on first parameter
    %% test that recursive types does not trigger endless loop
    %% test that getting type of out of bound parameter does not trigger crash
    compile_and_load2(Config,complete_function_parameter),
    {no, [], []} = do_expand("complete_function_parameter:an_untyped_fun("),
    {no, [], [{"Start ::",[]},{"integer()",[]}]} = do_expand("complete_function_parameter:a_fun_name("),
    {no, [], [{"End ::",[]},{"integer()",[]}]} = do_expand("complete_function_parameter:a_fun_name(1,"),
    {no, [], []} = do_expand("complete_function_parameter:a_fun_name(1,2,"),
    {no, [], [{"Deeplist ::",[]},{"term()",[]},{"[term() | [Deeplist]]",[]}]} = do_expand("complete_function_parameter:a_deeplist_fun("),
    {no, [], [{"T1 ::",[]},{"integer()",[]}]} = do_expand("complete_function_parameter:multi_arity_fun("),
    {no, [], [{"T2 ::",[]},{"boolean()",[]}]} = do_expand("complete_function_parameter:multi_arity_fun(1,"),
    {no, [], [{"B1 ::",[]},{"boolean()",[]},{"T1 ::",[]},{"integer()",[]}]} = do_expand("complete_function_parameter:different_multi_arity_fun("),
    {no, [], [{"T1 ::",[]},{"integer()",[]}]} = do_expand("complete_function_parameter:different_multi_arity_fun(false,"),
    {yes,"atom", [{"T1 ::",[]},{"{atom1, {non_neg_integer(), non_neg_integer()}}",[]},
        {"[atom4 | atom5]",[]},
        {"atom1",[]},
        {"atom2",[]}]} = do_expand("complete_function_parameter:advanced_nested_parameter("),
    {yes,"atom1,",[{"T1 ::",[]},{"atom1",[]}]} = do_expand("complete_function_parameter:advanced_nested_parameter({"),
    {no, [], [{"T1 ::",[]},{"{non_neg_integer(), non_neg_integer()}",[]}]} = do_expand("complete_function_parameter:advanced_nested_parameter({atom1,"),
    {yes,"atom",[{"T1 ::",[]},{"atom4",[]},{"atom5",[]}]} = do_expand("complete_function_parameter:advanced_nested_parameter([,"),
    ok.

%% Normal module name, some function names using quoted atoms.
quoted_fun(Config) when is_list(Config) ->
    {module,expand_test} = compile_and_load(Config,expand_test),
    {module,expand_test1} = compile_and_load(Config,expand_test1),
    %% should be no colon after test this time
    {yes, "test", [{"expand_test",""},{"expand_test1",""}]} = do_expand("expand_"),
    {no, [], []} = do_expand("expandXX_"),
    {no,[],[{"'#weird-fun-name'",1},
	    {"'Quoted_fun_name'",0},
	    {"'Quoted_fun_too'",0},
	    {"a_fun_name",1},
	    {"a_less_fun_name",1},
	    {"b_comes_after_a",1},
	    {"module_info",0},
	    {"module_info",1}]} = do_expand("expand_test1:"),
    {yes,"_",[{"a_fun_name",1},
        {"a_less_fun_name",1}]} = do_expand("expand_test1:a"),
    {yes,[],[{"a_fun_name",1},
	     {"a_less_fun_name",1}]} = do_expand("expand_test1:a_"),
    {yes,[],
     [{"'#weird-fun-name'",1},
      {"'Quoted_fun_name'",0},
      {"'Quoted_fun_too'",0}]} = do_expand("expand_test1:'"),
    {yes,"uoted_fun_",
     [{"'Quoted_fun_name'",0},
      {"'Quoted_fun_too'",0}]} = do_expand("expand_test1:'Q"),
    {yes,[],
     [{"'Quoted_fun_name'",0},
      {"'Quoted_fun_too'",0}]} = do_expand("expand_test1:'Quoted_fun_"),
    {yes,"weird-fun-name'(",[{"'#weird-fun-name'",1}]} = do_expand("expand_test1:'#"),

    %% Since there is a module_info/1 as well as a module_info/0
    %% there should not be a closing parenthesis added.
    {yes,"(",[{"module_info",0},{"module_info",1}]} = do_expand("expand_test:module_info"),
    ok.

quoted_module(Config) when is_list(Config) ->
    {module,'ExpandTestCaps'} = compile_and_load(Config,'ExpandTestCaps'),
    {yes, "Caps':", [{"'ExpandTestCaps'",""}]} = do_expand("'ExpandTest"),
    {no,[],
     [{"a_fun_name",1},
      {"a_less_fun_name",1},
      {"b_comes_after_a",1},
      {"module_info",0},
      {"module_info",1}]} = do_expand("'ExpandTestCaps':"),
    {yes,[],[{"a_fun_name",1},
	     {"a_less_fun_name",1}]} = do_expand("'ExpandTestCaps':a_"),
    ok.

quoted_both(Config) when is_list(Config) ->
    {module,'ExpandTestCaps'} = compile_and_load(Config,'ExpandTestCaps'),
    {module,'ExpandTestCaps1'} = compile_and_load(Config,'ExpandTestCaps1'),
    %% should be no colon (or quote) after test this time
    {yes, "Caps", [{"'ExpandTestCaps'",[]},{"'ExpandTestCaps1'",[]}]} = do_expand("'ExpandTest"),
    {no,[],[{"'#weird-fun-name'",0},
	    {"'Quoted_fun_name'",0},
	    {"'Quoted_fun_too'",0},
	    {"a_fun_name",1},
	    {"a_less_fun_name",1},
	    {"b_comes_after_a",1},
	    {"module_info",0},
	    {"module_info",1}]} = do_expand("'ExpandTestCaps1':"),
    {yes,"_",[{"a_fun_name",1},{"a_less_fun_name",1}]} = do_expand("'ExpandTestCaps1':a"),
    {yes,[],[{"a_fun_name",1},
	     {"a_less_fun_name",1}]} = do_expand("'ExpandTestCaps1':a_"),
    {yes,[],
     [{"'#weird-fun-name'",0},
      {"'Quoted_fun_name'",0},
      {"'Quoted_fun_too'",0}]} = do_expand("'ExpandTestCaps1':'"),
    {yes,"uoted_fun_",[{"'Quoted_fun_name'",0},{"'Quoted_fun_too'",0}]} = do_expand("'ExpandTestCaps1':'Q"),
    {yes,[],
     [{"'Quoted_fun_name'",0},
      {"'Quoted_fun_too'",0}]} = do_expand("'ExpandTestCaps1':'Quoted_fun_"),
    {yes,"weird-fun-name'()",[{"'#weird-fun-name'",0}]} = do_expand("'ExpandTestCaps1':'#"),
    ok.

%% Note: pull request #1152.
erl_1152(Config) when is_list(Config) ->
    "\n"++"foo"++"    "++[1089]++_ = do_format(["foo",[1089]]),
    ok.

erl_352(Config) when is_list(Config) ->
    erl_352_test(3, 3),

    erl_352_test(3, 75),
    erl_352_test(3, 76, [trailing]),
    erl_352_test(4, 74),
    erl_352_test(4, 75, [leading]),
    erl_352_test(4, 76, [leading, trailing]),

    erl_352_test(75, 3),
    erl_352_test(76, 3, [leading]),
    erl_352_test(74, 4),
    erl_352_test(75, 4, [leading]),
    erl_352_test(76, 4, [leading]),

    erl_352_test(74, 74, [leading]),
    erl_352_test(74, 75, [leading]),
    erl_352_test(74, 76, [leading, trailing]).

erl_352_test(PrefixLen, SuffixLen) ->
    erl_352_test(PrefixLen, SuffixLen, []).

erl_352_test(PrefixLen, SuffixLen, Dots) ->
    io:format("\nPrefixLen = ~w, SuffixLen = ~w\n", [PrefixLen, SuffixLen]),

    PrefixM = lists:duplicate(PrefixLen, $p),
    SuffixM = lists:duplicate(SuffixLen, $s),
    LM = [PrefixM ++ S ++ SuffixM || S <- ["1", "2"]],
    StrM = do_format(LM),
    check_leading(StrM, "", PrefixM, SuffixM, Dots),

    PrefixF = lists:duplicate(PrefixLen, $p),
    SuffixF = lists:duplicate(SuffixLen-2, $s),
    LF = [{PrefixF ++ S ++ SuffixF, 1} || S <- ["1", "2"]],
    StrF = do_format(LF),
    true = check_leading(StrF, "/1", PrefixF, SuffixF, Dots),

    ok.

check_leading(FormStr, ArityStr, Prefix, Suffix, Dots) ->
    List = string:tokens(FormStr, "\n "),
    io:format("~p\n", [List]),
    true = lists:all(fun(L) -> length(L) < 80 end, List),
    case lists:member(leading, Dots) of
        true ->
            true = lists:all(fun(L) ->
                                     {"...", Rest} = lists:split(3, L),
                                     check_trailing(Rest, ArityStr,
                                                    Suffix, Dots)
                             end, List);
        false ->
            true = lists:all(fun(L) ->
                                     {Prefix, Rest} =
                                         lists:split(length(Prefix), L),
                                     check_trailing(Rest, ArityStr,
                                                    Suffix, Dots)
                             end, List)
    end.

check_trailing([I|Str], ArityStr, Suffix, Dots) ->
    true = lists:member(I, [$1, $2]),
    case lists:member(trailing, Dots) of
        true ->
            {Rest, "..." ++ ArityStr} =
                lists:split(length(Str) - (3 + length(ArityStr)), Str),
            true = lists:prefix(Rest, Suffix);
        false ->
            {Rest, ArityStr} =
                lists:split(length(Str) - length(ArityStr), Str),
            Rest =:= Suffix
    end.

unicode(Config) when is_list(Config) ->
    {module,unicode_expand} = compile_and_load(Config,'unicode_expand'),
    {no,[],[{"'кlирилли́ческий атом'",0},
            {"'кlирилли́ческий атом'",1},
            {"'кlирилли́ческий атомB'",1},
            {"module_info",0},
            {"module_info",1}]} = do_expand("unicode_expand:"),
    {yes,"рилли́ческий атом", [{"'кlирилли́ческий атом'",0},
                              {"'кlирилли́ческий атом'",1},
                              {"'кlирилли́ческий атомB'",1}]} = do_expand("unicode_expand:'кlи"),
    {yes,"еский атом", [{"'кlирилли́ческий атом'",0},
                        {"'кlирилли́ческий атом'",1},
                        {"'кlирилли́ческий атомB'",1}]} = do_expand("unicode_expand:'кlирилли́ч"),
    {yes,"(",[{"'кlирилли́ческий атомB'",1}]} = do_expand("unicode_expand:'кlирилли́ческий атомB'"),
    "\n'кlирилли́ческий атом'/0   'кlирилли́ческий атом'/1   "
    "'кlирилли́ческий атомB'/1  \nmodule_info/0             "
    "module_info/1             \n" =
        do_format([{"'кlирилли́ческий атом'",0},
                   {"'кlирилли́ческий атом'",1},
                   {"'кlирилли́ческий атомB'",1},
                   {"module_info",0},
                   {"module_info",1}]),
    ok.

do_expand(String) ->
    Bs = [
        {'Binding', 0},
        {'MapBinding', #{a_key=>0, b_key=>1, c_key=>2}},
        {'Söndag', 0}],
    Rt = ets:new(records,[]),
    shell:read_and_add_records(edlin_expand_SUITE, '_', [], Bs, Rt),
    edlin_expand:expand(lists:reverse(String), Bs, Rt).

do_format(StringList) ->
    lists:flatten(edlin_expand:format_matches(StringList)).

compile_and_load2(Config, Module) ->
    Filename = filename:join(
        proplists:get_value(data_dir,Config),
        atom_to_list(Module)),
    PrivDir = proplists:get_value(priv_dir,Config),
    c:c(Filename, [debug_info, {output_dir, PrivDir}]).

compile_and_load(Config,Module) ->
    Filename = filename:join(
                 proplists:get_value(data_dir,Config),
                 atom_to_list(Module)),
    {ok,Module,Bin} = compile:file(Filename, [binary]),
    code:load_binary(Module, Filename, Bin).
