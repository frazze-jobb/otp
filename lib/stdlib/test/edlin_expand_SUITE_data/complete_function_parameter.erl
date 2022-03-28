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
-module(complete_function_parameter).

-export(
    [a_fun_name/2,
     an_untyped_fun/2,
     a_deeplist_fun/1,
     multi_arity_fun/0,
     multi_arity_fun/1,
     multi_arity_fun/2,
     different_multi_arity_fun/1,
     different_multi_arity_fun/2,
     advanced_nested_parameter/1
    ]).
%% test first and second parameter
    %% test multiple arities with same type on first parameter
    %% test multiple arities with different type on first parameter
    %% test that recursive types does not trigger endless loop
    %% test that getting type of out of bound parameter does not trigger crash
-spec a_fun_name(Start, End) -> Return when
    Start :: integer(),
    End :: integer(),
    Return:: integer().
a_fun_name(_Start, _End) -> 0.

an_untyped_fun(_Start, _End) -> 1.

-spec a_deeplist_fun(Deeplist) -> integer() when
    Deeplist :: T | [Deeplist],
    T :: term().
a_deeplist_fun(Deeplist) -> lists:flatten(Deeplist).

-spec multi_arity_fun() -> integer().
multi_arity_fun() -> 0.

-spec multi_arity_fun(T1) -> integer() when
    T1 :: integer().
multi_arity_fun(_T1) -> 1.

-spec multi_arity_fun(T1,T2) -> integer() when
    T1 :: integer(),
    T2 :: boolean().
multi_arity_fun(_T1, _T2) -> 2.

-spec different_multi_arity_fun(T1) -> integer() when
    T1 :: integer().
different_multi_arity_fun(_T1) -> 1.
-spec different_multi_arity_fun(B1, T1) -> integer() when
    B1 :: boolean(),
    T1 :: integer().
different_multi_arity_fun(_T1, _T2) -> 2.

-spec advanced_nested_parameter(T1) -> integer() when
    T1 :: {atom1, {non_neg_integer(), non_neg_integer()}} | 'atom1' | 'atom2' | ['atom4' | 'atom5'].
advanced_nested_parameter(_T1) -> 0.