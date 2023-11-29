%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2023. All Rights Reserved.
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
-module(edlin).

%% A simple Emacs-like line editor.
%% About Latin-1 characters: see the beginning of erl_scan.erl.

-export([init/0,init/1,start/1,start/2,edit_line/2]).
-export([erase_line/0,erase_inp/1,redraw_line/1]).
-export([length_before/1,length_after/1,prompt/1]).
-export([current_line/1, current_chars/1]).
-export([color/1, color/3]).
-export([text_mate_grammar/2, erlang_grammar/1]).
-export([edit_line1/2]).
-export([keymap/0]).
-import(lists, [reverse/1, reverse/2]).
-include_record("xmerl.hrl").
-export([over_word/3]).

-type keymap() :: #{atom() => #{string()|default => atom()}}.

%% A Continuation has the structure:
%%	{line,Prompt,{LinesBefore,{ColumnsBefore, ColumnsAfter},LinesAfter},{ShellMode, EscapePrefix}}

%% init()
%%  Initialise the line editor. This must be done once per process using
%%  the editor.

init() ->
    put(key_map, edlin_key:get_key_map()),
    put(kill_buffer, []).

init(Pid) ->
    init(),
    %% copy the kill_buffer from the process Pid
    CopiedKillBuf =
        case erlang:process_info(Pid, dictionary) of
	        {dictionary,Dict} ->
		            case proplists:get_value(kill_buffer, Dict) of
		                    undefined -> [];
		                    Buf       -> Buf
		            end;
	        undefined ->
		            []
	    end,
    put(kill_buffer, CopiedKillBuf).

%% start(Prompt)
%% edit(Characters, Continuation)
%%  Return
%%	{done,Line,Rest,Requests}
%%	{more_chars,Cont,Requests}
%%	{blink,Cont,Requests}

start(Pbs) ->
    start(Pbs, {normal,none}).

%% Only two modes used: 'none' and 'search'. Other modes can be
%% handled inline through specific character handling.
start(Pbs, {_,{_,[]},[]}=Cont) ->
    %% Skip redraw if the cursor is at the end.
    {more_chars,{line,Pbs,Cont,{normal,none}},[{insert_chars,unicode,multi_line_prompt(Pbs)}]};

start(Pbs, {_,{_,_},_}=Cont) ->
    {more_chars,{line,Pbs,Cont,{normal,none}},redraw(Pbs, Cont, [])};

start(Pbs, EditState) ->
    {more_chars,{line,Pbs,{[],{[],[]},[]},EditState},[new_prompt, {insert_chars,unicode,Pbs}]}.

-spec keymap() -> keymap().
keymap() ->
    get(key_map).

edit_line(Cs, {line,P,L,{blink,N_Rs}}) ->
    edit(Cs, P, L, {normal, none}, N_Rs);
edit_line(Cs, {line,P,L,M}) ->
    edit(Cs, P, L, M, []).

edit_line1(Cs, {line,P,L,{blink,N_Rs}}) ->
    edit(Cs, P, L, {normal, none}, N_Rs);
edit_line1(Cs, {line,P,{B,{[],[]},A},{normal,none}}) ->
    [CurrentLine|Lines] = [string:to_graphemes(Line) || Line <- reverse(string:split(Cs, "\n",all))],
    Cont = {Lines ++ B,{reverse(CurrentLine),[]},A},
    Rs = redraw(P, Cont, []),
    {more_chars, {line,P,Cont,{normal,none}},[delete_line|Rs]};
edit_line1(Cs, {line,P,L,M}) ->
    edit(Cs, P, L, M, []).

edit([C|Cs], P, Line, {blink,_}, [_|Rs]) ->	%Remove blink here
    edit([C|Cs], P, Line, {normal,none}, Rs);
edit([], P, L, {blink,N}, Rs) ->
    {blink,{line,P,L, {blink,N}},reverse(Rs)};
edit([], P, L, EditState, Rs) ->
    {more_chars,{line,P,L,EditState},reverse(Rs)};
edit(eof, _, {_,{Bef,Aft0},LA} = L, _, Rs) ->
    Aft1 = case LA of
        [Last|_] -> Last;
        _ -> Aft0
    end,
    {done,L,[],reverse(Rs, [{move_combo,-cp_len(Bef), length(LA), cp_len(Aft1)}])};
edit(Buf, P, {LB, {Bef,Aft}, LA}=MultiLine, {ShellMode, EscapePrefix}, Rs0) ->
    case edlin_key:get_valid_escape_key(Buf, EscapePrefix) of
        {escape_prefix, EscapePrefix1} ->
            case ShellMode of
                tab_expand -> edit(Buf, P, MultiLine, {normal, none}, Rs0);
                _ -> edit([], P, MultiLine, {ShellMode, EscapePrefix1}, Rs0)
            end;
        {invalid, _I, Rest} ->
            edit(Rest, P, MultiLine, {ShellMode, none}, Rs0);
        {insert, C1, Cs2} ->
            %% If its a printable character
            %% we could in theory override it in the keymap,
            %% but lets not for now.
            Op = case ShellMode of
                normal when C1 =:= $) ->
                    {blink,$),$(};
                normal when C1 =:= $] ->
                    {blink,$],$[};
                normal when C1 =:= $} ->
                    {blink,$},${};
                normal -> {insert, C1};
                search when $\s =< C1 ->{insert_search, C1};
                search -> search_quit;
                tab_expand -> tab_expand_quit
            end,
            case Op of
                tab_expand_quit ->
                    edit(Buf, P, MultiLine, {normal,none}, Rs0);
                search_quit ->
                    {search_quit,Cs2,{line,P,MultiLine,{normal, none}},reverse(Rs0)};
                _ ->
                    case do_op(Op, MultiLine, Rs0) of
                        {blink,N,MultiLine1,Rs} ->
                            edit(Cs2, P, MultiLine1, {blink,N}, Rs);
                        {redraw, MultiLine1, Rs} ->
                            edit(Cs2, P, MultiLine1, {ShellMode, none}, redraw(P, MultiLine1, Rs));
                        {MultiLine1,Rs} ->
                            edit(Cs2, P, MultiLine1, {ShellMode, none}, Rs)
                    end
            end;
        {key, Key, Cs} ->
            KeyMap = get(key_map),
            Value = case maps:find(Key, maps:get(ShellMode, KeyMap)) of
                error ->
                    %% Default handling for shell modes
                    case maps:find(default, maps:get(ShellMode, KeyMap)) of
                        error -> none;
                        {ok, Value0} -> Value0
                    end;
                {ok, Value0} -> Value0
            end,
            case Value of
                none -> edit(Cs, P, MultiLine, {normal,none}, Rs0);
                search -> {search,Cs,{line,P,MultiLine,{normal, none}},reverse(Rs0)};
                search_found -> {search_found,Cs,{line,P,MultiLine,{normal, none}},reverse(Rs0)};
                search_cancel -> {search_cancel,Cs,{line,P,MultiLine,{normal, none}},reverse(Rs0)};
                search_quit -> {search_quit,Cs,{line,P,MultiLine,{normal, none}},reverse(Rs0)};
                open_editor -> {open_editor,Cs,{line,P,MultiLine,{normal, none}},reverse(Rs0)};
                history_up -> {history_up,Cs,{line,P,MultiLine,{normal, none}},reverse(Rs0)};
                history_down -> {history_down,Cs,{line,P,MultiLine,{normal, none}},reverse(Rs0)};
                new_line ->
                    MultiLine1 = {[lists:reverse(Bef)|LB],{[],Aft},LA},
                    edit(Cs, P, MultiLine1, {normal, none}, reverse(redraw(P, MultiLine1, Rs0)));
                new_line_finish ->
                    % Move to end
                    {{LB1,{Bef1,[]},[]}, Rs1} = do_op(end_of_expression, MultiLine, Rs0),
                    {done, {[lists:reverse(Bef1)|LB1],{[],[]},[]}, Cs, reverse(Rs1, [{insert_chars, unicode, "\n"}])};
                redraw_line ->
                    Rs1 = erase_line(Rs0),
                    Rs = redraw(P, MultiLine, Rs1),
                    edit(Cs, P, MultiLine, {normal, none}, Rs);
                clear ->
                    Rs = redraw(P, MultiLine, [clear|Rs0]),
                    edit(Cs, P, MultiLine, {normal, none}, Rs);
                tab_expand ->
                    {expand, chars_before(MultiLine), Cs,
                     {line, P, MultiLine, {tab_expand, none}},
                     reverse(Rs0)};
                tab_expand_full ->
                    {expand_full, chars_before(MultiLine), Cs,
                     {line, P, MultiLine, {tab_expand, none}},
                     reverse(Rs0)};
                tab_expand_quit ->
                    %% When exiting tab expand mode, we want to evaluate the key in normal mode
                    edit(Buf, P, MultiLine, {normal,none}, Rs0);
                Op ->
                    Op1 = case ShellMode of
                        search ->
                            {search, Op};
                        _ -> Op
                    end,
                    case do_op(Op1, MultiLine, Rs0) of
                        {blink,N,MultiLine1,Rs} ->
                            edit(Cs, P, MultiLine1, {blink,N}, Rs);
                        {redraw, MultiLine1, Rs} ->
                            edit(Cs, P, MultiLine1, {normal, none}, redraw(P, MultiLine1, Rs));
                        {MultiLine1,Rs} ->
                            edit(Cs, P, MultiLine1, {ShellMode, none}, Rs)
                    end
            end
    end.

%% do_op(Action, Before, After, Requests)
%% Before and After are of lists of type string:grapheme_cluster()
do_op({insert,C}, {LB,{[],[]},LA}, Rs) ->
    {{LB,{[C],[]},LA},[{insert_chars, unicode,[C]}|Rs]};
do_op({insert,C}, {LB,{[Bef|Bef0], []},LA}, Rs) ->
    case string:to_graphemes([Bef,C]) of
        [GC] -> {{LB,{[GC|Bef0],[]},LA},[{insert_chars, unicode,[C]}|Rs]};
        _ -> {{LB,{[C,Bef|Bef0],[]},LA},[{insert_chars, unicode,[C]}|Rs]}
    end;
do_op({insert,C}, {LB,{[], Aft},LA}, Rs) ->
    {{LB,{[C],Aft},LA},[{insert_chars, unicode,[C]}|Rs]};
do_op({insert,C}, {LB,{[Bef|Bef0], Aft},LA}, Rs) ->
    case string:to_graphemes([Bef,C]) of
        [GC] -> {{LB,{[GC|Bef0],Aft},LA},[{insert_chars, unicode,[C]}|Rs]};
        _ -> {{LB,{[C,Bef|Bef0],Aft},LA},[{insert_chars, unicode,[C]}|Rs]}
    end;
%% Search mode prompt always looks like (search)`$TERMS': $RESULT.
%% the {insert_search, _} handlings allow to share this implementation
%% correctly with group.erl. This module provides $TERMS, and group.erl
%% is in charge of providing $RESULT.
%% This require a bit of trickery. Because search disables moving around
%% on the line (left/right arrow keys and other shortcuts that just exit
%% search mode), we can use the Bef and Aft variables to hold each
%% part of the line. Bef takes charge of "(search)`$TERMS" and Aft
%% takes charge of "': $RESULT".
%%
%% Since multiline support the search mode prompt always looks like:
%% search: $TERMS
%%   $ResultLine1
%%   $ResultLine2
do_op({insert_search, C}, {LB,{Bef, []},LA}, Rs) ->
    {{LB, {[C|Bef],[]}, LA},
     [{insert_chars, unicode, [C]}, delete_after_cursor | Rs]};
do_op({insert_search, C}, {LB,{Bef, _Aft},LA}, Rs) ->
    {{LB, {[C|Bef],[]}, LA},
     [{insert_chars, unicode, [C]}, delete_after_cursor | Rs],
     search};
do_op({search, backward_delete_char}, {LB,{[_|Bef], Aft},LA}, Rs) ->
    Offset= cp_len(Aft)+1,
    {{LB, {Bef,Aft}, LA},
     [{insert_chars, unicode, Aft}, {delete_chars,-Offset}|Rs]};
do_op({search, backward_delete_char}, {LB,{[], Aft},LA}, Rs) ->
    {{LB, {[],Aft}, LA}, [{insert_chars, unicode, Aft}, {delete_chars,-cp_len(Aft)}|Rs]};
do_op({search, skip_up}, {_,{Bef, Aft},_}, Rs) ->
    Offset= cp_len(Aft),
    {{[],{[$\^R|Bef],Aft},[]}, % we insert ^R as a flag to whoever called us
     [{insert_chars, unicode, Aft}, {delete_chars,-Offset}|Rs]};
do_op({search, skip_down}, {_,{Bef, Aft},_LA}, Rs) ->
    Offset= cp_len(Aft),
    {{[],{[$\^S|Bef],Aft},[]}, % we insert ^S as a flag to whoever called us
     [{insert_chars, unicode, Aft}, {delete_chars,-Offset}|Rs]};
%% do blink after $$
do_op({blink,C,M}, {_,{[$$,$$|_], _},_} = MultiLine, Rs) ->
    blink(over_paren(chars_before(MultiLine), C, M), C, MultiLine, Rs);
%% don't blink after a $
do_op({blink,C,_}, {_,{[$$|_], _},_} = MultiLine, Rs) ->
    do_op({insert,C}, MultiLine, Rs);
do_op({blink,C,M}, MultiLine, Rs) ->
    blink(over_paren(chars_before(MultiLine), C, M), C, MultiLine, Rs);
do_op(auto_blink, MultiLine, Rs) ->
    blink(over_paren_auto(chars_before(MultiLine)), MultiLine, Rs);
do_op(forward_delete_char, {LB,{Bef, []},[NextLine|LA]}, Rs) ->
    NewLine = {LB, {Bef, NextLine}, LA},
    {redraw, NewLine, Rs};
do_op(forward_delete_char, {LB,{Bef, [GC|Aft]},LA}, Rs) ->
    {{LB, {Bef,Aft}, LA},[{delete_chars,gc_len(GC)}|Rs]};
do_op(backward_delete_char, {[PrevLine|LB],{[], Aft},LA}, Rs) ->
    NewLine = {LB, {lists:reverse(PrevLine), Aft}, LA},
    {redraw, NewLine,Rs};
do_op(backward_delete_char, {LB,{[GC|Bef], Aft},LA}, Rs) ->
    {{LB, {Bef,Aft}, LA},[{delete_chars,-gc_len(GC)}|Rs]};
do_op(forward_delete_word, {LB,{Bef, []},[NextLine|LA]}, Rs) ->
    NewLine = {LB, {Bef, NextLine}, LA},
    {redraw, NewLine, Rs};
do_op(forward_delete_word, {LB,{Bef, Aft0},LA}, Rs) ->
    {Aft1,Kill0,N0} = over_non_word(Aft0, [], 0),
    {Aft,Kill,N} = over_word(Aft1, Kill0, N0),
    put(kill_buffer, reverse(Kill)),
    {{LB, {Bef,Aft}, LA},[{delete_chars,N}|Rs]};
do_op(backward_delete_word, {[PrevLine|LB],{[], Aft},LA}, Rs) ->
    NewLine = {LB, {lists:reverse(PrevLine), Aft}, LA},
    {redraw, NewLine,Rs};
do_op(backward_delete_word, {LB,{Bef0, Aft},LA}, Rs) ->
    {Bef1,Kill0,N0} = over_non_word(Bef0, [], 0),
    {Bef,Kill,N} = over_word(Bef1, Kill0, N0),
    put(kill_buffer, Kill),
    {{LB,{Bef,Aft},LA},[{delete_chars,-N}|Rs]};
do_op(transpose_char, {LB,{[C1,C2|Bef], []},LA}, Rs) ->
    Len = gc_len(C1)+gc_len(C2),
    {{LB, {[C2,C1|Bef],[]}, LA},[{insert_chars_over, unicode,[C1,C2]},{move_rel,-Len}|Rs]};
do_op(transpose_char, {LB,{[C2|Bef], [C1|Aft]},LA}, Rs) ->
    Len = gc_len(C2),
    {{LB, {[C2,C1|Bef],Aft}, LA},[{insert_chars_over, unicode,[C1,C2]},{move_rel,-Len}|Rs]};
do_op(transpose_word, {LB,{Bef0, Aft0},LA}, Rs) ->
    {Aft1,Word2A,N0} = over_word(Aft0, [], 0),
    {Bef, TransposedWords, Aft, N} = case N0 of
        0 -> {Aft2,NonWord,N1} = over_non_word(Aft1, [], 0),
            case N1 of
                0 ->
                    {Bef1,Word2B,B0} = over_word(Bef0, [], 0),
                    {Bef2,NonWordB,B1} = over_non_word(Bef1, [], B0),
                    {Bef3,Word1,B2} = over_word(Bef2, [], B1),
                    {Bef3, Word2B++NonWordB++Word1, Aft0, B2};
                _ ->
                    {Aft3,Word2,N2} = over_word(Aft2, [], N1),
                    case N2 of
                        0 ->
                            {Bef1,Word2B,B0} = over_word(Bef0, [], 0),
                            {Bef2,NonWordB,B1} = over_non_word(Bef1, [], B0),
                            {Bef3,Word1,B2} = over_word(Bef2, [], B1),
                            {Bef3, Word2B++NonWordB++Word1, Aft0, B2};
                        _ ->
                            {Bef1, NonWord2, B0} = over_non_word(Bef0, [], 0),
                            {Bef2, Word1, B1} = over_word(Bef1, [], B0),
                            {Bef2, reverse(Word2) ++NonWord2 ++ reverse(NonWord) ++Word1, Aft3, B1}
                    end
            end;
        _ ->
            {Bef1,Word2B,B0} = over_word(Bef0, [], 0),
            {Bef2,NonWord,B1} = over_non_word(Bef1, [], B0),
            {Bef3,Word1,B2} = over_word(Bef2, [], B1),
            {Bef3, Word2B++reverse(Word2A)++NonWord++Word1, Aft1, B2}
    end,
    {{LB, {reverse(TransposedWords)++Bef, Aft}, LA},[{insert_chars_over, unicode, TransposedWords}, {move_rel, -N}|Rs]};
do_op(kill_word, {LB,{Bef, Aft0},LA}, Rs) ->
    {Aft1,Kill0,N0} = over_non_word(Aft0, [], 0),
    {Aft,Kill,N} = over_word(Aft1, Kill0, N0),
    put(kill_buffer, reverse(Kill)),
    {{LB, {Bef,Aft}, LA},[{delete_chars,N}|Rs]};
do_op(backward_kill_word, {LB,{Bef0, Aft},LA}, Rs) ->
    {Bef1,Kill0,N0} = over_non_word(Bef0, [], 0),
    {Bef,Kill,N} = over_word(Bef1, Kill0, N0),
    put(kill_buffer, Kill),
    {{LB,{Bef,Aft},LA},[{delete_chars,-N}|Rs]};
do_op(kill_line, {LB, {Bef, Aft}, LA}, Rs) ->
    put(kill_buffer, Aft),
    {{LB, {Bef,[]}, LA},[{delete_chars,cp_len(Aft)}|Rs]};
do_op(clear_line, _, Rs) ->
    {redraw, {[], {[],[]},[]}, Rs};
do_op(yank, {LB,{Bef, []},LA}, Rs) ->
    Kill = get(kill_buffer),
    {{LB, {reverse(Kill, Bef),[]}, LA},[{insert_chars, unicode,Kill}|Rs]};
do_op(yank, {LB,{Bef, Aft},LA}, Rs) ->
    Kill = get(kill_buffer),
    {{LB, {reverse(Kill, Bef),Aft}, LA},[{insert_chars, unicode,Kill}|Rs]};
do_op(forward_line, {_,_,[]} = MultiLine, Rs) ->
    {MultiLine, Rs};
do_op(forward_line, {LB,{Bef, Aft},[AL|LA]}, Rs) ->
    CL = lists:reverse(Bef, Aft),
    CursorPos = min(length(Bef), length(AL)),
    {Bef1, Aft1} = lists:split(CursorPos, AL),
    {{[CL|LB], {lists:reverse(Bef1), Aft1}, LA}, [{move_combo, -cp_len(Bef), 1, cp_len(Bef1)}|Rs]};
do_op(backward_line, {[], _, _} = MultiLine, Rs) ->
    {MultiLine, Rs};
do_op(backward_line, {[BL|LB],{Bef, Aft},LA}, Rs) ->
    CL = lists:reverse(Bef, Aft),
    CursorPos = min(length(Bef), length(BL)),
    {Bef1, Aft1} = lists:split(CursorPos, BL),
    {{LB, {lists:reverse(Bef1), Aft1}, [CL|LA]},[{move_combo, -cp_len(Bef), -1, cp_len(Bef1)}|Rs]};
do_op(forward_char, {LB,{Bef, []}, [AL|LA]}, Rs) ->
    {{[lists:reverse(Bef)|LB],{[], string:to_graphemes(AL)}, LA}, [{move_combo, -cp_len(Bef), 1, 0}|Rs]};
do_op(forward_char, {LB,{Bef, [C|Aft]},LA}, Rs) ->
    {{LB,{[C|Bef],Aft},LA},[{move_rel,gc_len(C)}|Rs]};
do_op(backward_char, {[BL|LB],{[], Aft},LA}, Rs) ->
    {{LB,{lists:reverse(string:to_graphemes(BL)), []}, [Aft|LA]}, [{move_combo, 0, -1, cp_len(BL)}|Rs]};
do_op(backward_char, {LB,{[C|Bef], Aft},LA}, Rs) ->
    {{LB, {Bef,[C|Aft]}, LA},[{move_rel,-gc_len(C)}|Rs]};
do_op(forward_word, {LB,{Bef0, []},[NextLine|LA]}, Rs) ->
    {{[reverse(Bef0)|LB], {[], NextLine}, LA},[{move_combo, -cp_len(Bef0), 1, 0}|Rs]};
do_op(forward_word, {LB,{Bef0, Aft0},LA}, Rs) ->
    {Aft1,Bef1,N0} = over_non_word(Aft0, Bef0, 0),
    {Aft, Bef, N} = over_word(Aft1, Bef1, N0),
    {{LB, {Bef,Aft}, LA},[{move_rel,N}|Rs]};
do_op(backward_word, {[PrevLine|LB],{[], Aft0},LA}, Rs) ->
    {{LB, {reverse(PrevLine), []}, [Aft0|LA]},[{move_combo, 0, -1, cp_len(PrevLine)}|Rs]};
do_op(backward_word, {LB,{Bef0, Aft0},LA}, Rs) ->
    {Bef1,Aft1,N0} = over_non_word(Bef0, Aft0, 0),
    {Bef,Aft,N} = over_word(Bef1, Aft1, N0),
    {{LB, {Bef,Aft}, LA},[{move_rel,-N}|Rs]};
do_op(beginning_of_expression, {[],{[], Aft},LA}, Rs) ->
    {{[], {[],Aft}, LA},Rs};
do_op(beginning_of_expression, {LB,{Bef, Aft},LA}, Rs) ->
    [First|Rest] = lists:reverse(LB) ++ [lists:reverse(Bef, Aft)],
    {{[], {[],First}, Rest ++ LA},[{move_combo, -cp_len(Bef), -length(LB), 0}|Rs]};
do_op(end_of_expression, {LB,{Bef, []},[]}, Rs) ->
    {{LB, {Bef,[]}, []},Rs};
do_op(end_of_expression, {LB,{Bef, Aft},LA}, Rs) ->
    [Last|Rest] = lists:reverse(LA) ++ [lists:reverse(Bef, Aft)],
    {{Rest ++ LB, {lists:reverse(Last),[]}, []},[{move_combo, -cp_len(Bef), length(LA), cp_len(Last)}|Rs]};
do_op(beginning_of_line, {LB,{[_|_]=Bef, Aft},LA}, Rs) ->
    {{LB, {[],reverse(Bef, Aft)}, LA},[{move_rel,-(cp_len(Bef))}|Rs]};
do_op(beginning_of_line, {LB,{[], Aft},LA}, Rs) ->
    {{LB, {[],Aft}, LA},Rs};
do_op(end_of_line, {LB,{Bef, [_|_]=Aft},LA}, Rs) ->
    {{LB, {reverse(Aft, Bef),[]}, LA},[{move_rel,cp_len(Aft)}|Rs]};
do_op(end_of_line, {LB,{Bef, []},LA}, Rs) ->
    {{LB, {Bef,[]}, LA},Rs};
do_op(backward_kill_line, {LB,{Bef, Aft},LA}, Rs) ->
    put(kill_buffer, reverse(Bef)),
    {{LB, {[], Aft}, LA}, [{delete_chars, -cp_len(Bef)} | Rs]};
do_op(beep, {LB,{Bef, Aft},LA}, Rs) ->
    {{LB,{Bef,Aft},LA},[beep|Rs]};
do_op(_, {LB,{Bef, Aft},LA}, Rs) ->
    {{LB,{Bef,Aft},LA},[beep|Rs]}.

blink(beep, C, {LB, {Bef, Aft}, LA}, Rs) ->
    {{LB,{[C|Bef], Aft},LA}, [beep,{insert_chars, unicode, [C]}|Rs]};
blink({N, R}, C, MultiLine, Rs) ->
    blink({N, R, C}, MultiLine, Rs).
%% same line
blink(beep, {LB,{Bef, Aft},LA}, Rs) ->
    {{LB,{Bef, Aft},LA}, [beep|Rs]};
blink({N, 0, Paren}, {LB, {Bef, Aft}, LA}, Rs) ->
    MoveBackToParen = {move_rel,-N-1},
    MoveForwardToParen = {move_rel, N+1},
    {blink,[MoveForwardToParen],{LB,{[Paren|Bef],Aft},LA},
        [MoveBackToParen,{insert_chars, unicode,[Paren]}|Rs]};
%% multiline
blink({N, R, Paren}, {LB,{Bef, Aft},LA}, Rs) ->
    LengthToClosingParen = cp_len([Paren|Bef]),
    LengthOpeningParen = cp_len(lists:nth(R,LB)) - N - 1,
    MoveToOpeningParen = {move_combo, -LengthToClosingParen, -R, LengthOpeningParen},
    MoveToClosingParen = {move_combo, -LengthOpeningParen, R, LengthToClosingParen+1},
    {blink,[MoveToClosingParen],{LB,{[Paren|Bef],Aft},LA},
        [MoveToOpeningParen,{insert_chars, unicode,[Paren]}|Rs]}.

%% over_word(Chars, InitialStack, InitialCount) ->
%%	{RemainingChars,CharStack,Count}
%% over_non_word(Chars, InitialStack, InitialCount) ->
%%	{RemainingChars,CharStack,Count}
%%  Step over word/non-word characters pushing the stepped over ones on
%%  the stack.
over_word(Cs, Stack, N) ->
    L = length([1 || $\' <- Cs]),
    case L rem 2 of
	0 ->
	    over_word1(Cs, Stack, N);
	1 ->
	    until_quote(Cs, Stack, N)
    end.

until_quote([$\'|Cs], Stack, N) ->
    {Cs, [$\'|Stack], N+1};
until_quote([C|Cs], Stack, N) ->
    until_quote(Cs, [C|Stack], N+gc_len(C)).

over_word1([$\'=C|Cs], Stack, N) ->
    until_quote(Cs, [C|Stack], N+1);
over_word1(Cs, Stack, N) ->
    over_word2(Cs, Stack, N).

over_word2([C|Cs], Stack, N) ->
    case word_char(C) of
	true -> over_word2(Cs, [C|Stack], N+gc_len(C));
	false -> {[C|Cs],Stack,N}
    end;
over_word2([], Stack, N) when is_integer(N) ->
    {[],Stack,N}.

over_non_word([C|Cs], Stack, N) ->
    case word_char(C) of
	true -> {[C|Cs],Stack,N};
	false -> over_non_word(Cs, [C|Stack], N+gc_len(C))
    end;
over_non_word([], Stack, N) ->
    {[],Stack,N}.

word_char(C) when C >= $A, C =< $Z -> true;
word_char(C) when C >= $À, C =< $Þ, C =/= $× -> true;
word_char(C) when C >= $a, C =< $z -> true;
word_char(C) when C >= $ß, C =< $ÿ, C =/= $÷ -> true;
word_char(C) when C >= $0, C =< $9 -> true;
word_char(C) when C =:= $_ -> true;
word_char([_|_]) -> true; %% Is grapheme
word_char(_) -> false.

%% over_white(Chars, InitialStack, InitialCount) ->
%%	{RemainingChars,CharStack,Count}

%% over_white([$\s|Cs], Stack, N) ->
%%     over_white(Cs, [$\s|Stack], N+1);
%% over_white([$\t|Cs], Stack, N) ->
%%     over_white(Cs, [$\t|Stack], N+1);
%% over_white(Cs, Stack, N) ->
%%     {Cs,Stack,N}.

%% over_paren(Chars, Paren, Match)
%% over_paren(Chars, Paren, Match, Depth, N)
%%  Step over parentheses until matching Paren is found at depth 0. Don't
%%  do proper parentheses matching check. Paren has NOT been added.

over_paren(Chars, Paren, Match) ->
    over_paren(Chars, Paren, Match, 1, 1, 0, []).


over_paren([C,$$,$$|Cs], Paren, Match, D, N, R, L)  ->
    over_paren([C|Cs], Paren, Match, D, N+2, R, L);
over_paren([GC,$$|Cs], Paren, Match, D, N, R, L)  ->
    over_paren(Cs, Paren, Match, D, N+1+gc_len(GC), R, L);
over_paren([$\n|Cs], Paren, Match, D, _N, R, L) ->
    over_paren(Cs, Paren, Match, D, 0, R+1, L);
over_paren([Match|_], _Paren, Match, 1, N, R, _) ->
    {N, R};
over_paren([Match|Cs], Paren, Match, D, N, R, [Match|L]) ->
    over_paren(Cs, Paren, Match, D-1, N+1, R, L);
over_paren([Paren|Cs], Paren, Match, D, N, R, L) ->
    over_paren(Cs, Paren, Match, D+1, N+1, R, [Match|L]);

over_paren([$)|Cs], Paren, Match, D, N, R, L)  ->
    over_paren(Cs, Paren, Match, D, N+1, R, [$(|L]);
over_paren([$]|Cs], Paren, Match, D, N, R, L)  ->
    over_paren(Cs, Paren, Match, D, N+1, R, [$[|L]);
over_paren([$}|Cs], Paren, Match, D, N, R, L)  ->
    over_paren(Cs, Paren, Match, D, N+1, R, [${|L]);

over_paren([$(|Cs], Paren, Match, D, N, R, [$(|L])  ->
    over_paren(Cs, Paren, Match, D, N+1, R, L);
over_paren([$[|Cs], Paren, Match, D, N, R, [$[|L])  ->
    over_paren(Cs, Paren, Match, D, N+1, R, L);
over_paren([${|Cs], Paren, Match, D, N, R, [${|L])  ->
    over_paren(Cs, Paren, Match, D, N+1, R, L);

over_paren([$(|_], _, _, _, _, _, _)  ->
    beep;
over_paren([$[|_], _, _, _, _, _, _)  ->
    beep;
over_paren([${|_], _, _, _, _, _, _)  ->
    beep;

over_paren([GC|Cs], Paren, Match, D, N, R, L)  ->
    over_paren(Cs, Paren, Match, D, N+gc_len(GC), R, L);
over_paren([], _, _, _, _, _, _) ->
    beep.

over_paren_auto(Chars) ->
    over_paren_auto(Chars, 1, 1, 0, []).


over_paren_auto([C,$$,$$|Cs], D, N, R, L)  ->
    over_paren_auto([C|Cs], D, N+2, R, L);
over_paren_auto([GC,$$|Cs], D, N, R, L)  ->
    over_paren_auto(Cs, D, N+1+gc_len(GC), R, L);
over_paren_auto([$\n|Cs], D, _N, R, L) ->
    over_paren_auto(Cs, D, 0, R+1, L);

over_paren_auto([$(|_], _, N, R, [])  ->
    {N, R, $)};
over_paren_auto([$[|_], _, N, R, [])  ->
    {N, R, $]};
over_paren_auto([${|_], _, N, R, [])  ->
    {N, R, $}};

over_paren_auto([$)|Cs], D, N, R, L)  ->
    over_paren_auto(Cs, D, N+1, R, [$(|L]);
over_paren_auto([$]|Cs], D, N, R, L)  ->
    over_paren_auto(Cs, D, N+1, R, [$[|L]);
over_paren_auto([$}|Cs], D, N, R, L)  ->
    over_paren_auto(Cs, D, N+1, R, [${|L]);

over_paren_auto([$(|Cs], D, N, R, [$(|L])  ->
    over_paren_auto(Cs, D, N+1, R, L);
over_paren_auto([$[|Cs], D, N, R, [$[|L])  ->
    over_paren_auto(Cs, D, N+1, R, L);
over_paren_auto([${|Cs], D, N, R, [${|L])  ->
    over_paren_auto(Cs, D, N+1, R, L);

over_paren_auto([GC|Cs], D, N, R, L)  ->
    over_paren_auto(Cs, D, N+gc_len(GC), R, L);
over_paren_auto([], _, _, _, _) ->
    beep.

%% erase_line(Line)
%% erase_inp(Line)
%% redraw_line(Line)
%% length_before(Line)
%% length_after(Line)
%% prompt(Line)
%% current_line(Line)
%% current_chars(Line)
%%  Various functions for accessing bits of a line.

erase_line() ->
    [delete_line].

erase_inp({line,_, L,_}) ->
    reverse(erase([], L, [])).

erase_line(Rs) ->
    [delete_line|Rs].

erase(Pbs, {_,{Bef, Aft},_}, Rs) ->
    [{delete_chars,-cp_len(Pbs)-cp_len(Bef)},{delete_chars,cp_len(Aft)}|Rs].

redraw_line({line, Pbs, L,_}) ->
    redraw(Pbs, L, []).
color({line, Pbs, {_LB,{_Bef,_},_}=L, _}) ->
    color(Pbs, multi_line_prompt(Pbs), L).
color(Pbs, MultilinePrompt, L) ->
    ColorCommand = application:get_env(stdlib, shell_syntax_highlight_func),
    Buffer = current_line(L),
    try
        case ColorCommand of
            {ok, {M,F}} when is_atom(M), is_atom(F) ->
                ColorizedCode = M:F(Buffer),
                Expression = Pbs ++ lists:flatten(
                    lists:join(
                        "\n"++MultilinePrompt, string:split(ColorizedCode, "\n"))),
                [{redraw_prompt_with_color, Expression}];
            {ok, ColorCommand1} when is_list(ColorCommand1) ->
                case color_command(Buffer, ColorCommand1) of
                    ["/bin/sh"|_] -> []; %%skip coloring
                    ColorizedCode ->
                        Expression = Pbs ++ lists:flatten(
                            lists:join(
                                "\n"++MultilinePrompt, string:split(string:trim(ColorizedCode, trailing), "\n"))),
                         [{redraw_prompt_with_color, Expression}]
                end
        end
    catch
        _:_ -> []
    end.        
    
color_command(Buffer, Command) ->
    MkTemp = case os:type() of
        {win32, _} ->
            os:cmd("powershell \"write-host (& New-TemporaryFile | Select-Object -ExpandProperty FullName)\"");
        {unix,_} ->
            os:cmd("mktemp")
    end,
    TmpFile = string:chomp(MkTemp),
    _ = file:write_file(TmpFile, unicode:characters_to_binary(Buffer, unicode)),
    Command1 = string:replace(Command, "${file}", TmpFile),
    Content = os:cmd(Command1),
    _ = file:del_dir_r(TmpFile),
    Content.
event_function({ignorableWhitespace,_},_,S) -> S;
event_function({startElement,[],"dict",{[],"dict"},[]},_,S =#{parent_stack := ParentStack, key_name := Name}) ->
                    S#{parent_stack := [#{type=>dict, key=>Name, dict => #{}}|ParentStack], key_name := undefined};
event_function({startElement,[],"array",{[],"array"},[]},_,S =#{parent_stack := ParentStack, key_name := Name}) ->
                    S#{parent_stack := [#{type=>array, key=>Name, array => []}|ParentStack], key_name := undefined};
event_function({startElement,[],"key",{[],"key"},[]}, _, S) ->
                    S#{record_key => 1};
event_function({characters,Cs}, _, S = #{record_key := 1}) ->
                    S#{key_name => Cs};
event_function({endElement,[],"key",{[],"key"}}, _, S = #{record_key := 1}) ->
                    S#{record_key => 0};
event_function({startElement,[],"string",{[],"string"},[]}, _, S) ->
                    S#{record_string => 1};
event_function({characters,Cs}, _, S = #{record_string := 1}) ->
                    S#{string_value => Cs};
event_function({endElement,[],"string",{[],"string"}}, _, S = #{parent_stack := [Parent=#{type:=dict,dict:=Dict}|Stack], key_name := Name, string_value := Value}) when Name =/= undefined ->
                    Parent1 = Parent#{dict => Dict#{Name => Value}},
                    S#{parent_stack := [Parent1|Stack], string_value := undefined, record_string := 0, key_name := undefined};
event_function({endElement,[],"string",{[],"string"}}, _, S = #{parent_stack := [Parent=#{type:=array,array:=Array}|Stack], string_value := Value}) ->
                    Parent1 = Parent#{array => Array ++ [Value]},
                    S#{parent_stack := [Parent1|Stack], string_value := undefined, record_string := 0};
event_function({endElement,[],"dict",{[],"dict"}}, _, S = #{parent_stack := [#{type:=dict, dict:=Dict}]}) ->
                    S#{final => Dict, parent_stack := []};
event_function({endElement,[],"dict",{[],"dict"}}, _,
                    S = #{parent_stack := [#{type:=dict, key:=undefined, dict:=Dict}, #{type:=array, array:=Array}=Parent|Stack]}) ->
                    S#{parent_stack := [Parent#{array := Array ++ [Dict]}|Stack]};
event_function({endElement,[],"dict",{[],"dict"}}, _,
                    S = #{parent_stack := [#{type:=dict, key:=Key, dict:=Dict}, #{type:=dict, dict:=Dict1}=Parent|Stack]}) ->
                    S#{parent_stack := [Parent#{dict := Dict1#{Key => Dict}}|Stack]};
event_function({endElement,[],"array",{[],"array"}}, _,
                    S = #{parent_stack := [#{type:=array, key:=undefined, array:=Array},#{type:=array, array:=Array1}=Parent|Stack]}) ->
                    S#{parent_stack := [Parent#{array := Array1 ++ [Array]}|Stack]};
event_function({endElement,[],"array",{[],"array"}}, _,
                    S = #{parent_stack := [#{type:=array, key := Name, array:=Array},#{type:=dict, dict:=Dict}=Parent|Stack]}) ->
                    S#{parent_stack := [Parent#{dict := Dict#{Name => Array}}|Stack]};
event_function(_, _, S) ->
                    S.
erlang_grammar(Buffer) ->
    text_mate_grammar(Buffer, "/home/efrfred/otp/lib/stdlib/src/Erlang.plist").
text_mate_grammar(Buffer, Plist) ->
    Cached = get({cache, Plist}),
    case Cached of
        undefined ->
            State = #{parent_stack => [], key_name => undefined, string_value => undefined, record_key => 0, record_string => 0, final => undefined},
            {ok, #{final := Final}, <<>>} = xmerl_sax_parser:file(Plist,
                [{event_state, State},
                 {event_fun, fun event_function/3}]),
            put({cache, Plist}, Final),
            Final;
        Final ->
            Final
    end,
    text_mate_grammar1(Buffer, Final).

text_mate_grammar1(Buffer, Final) ->
    %% Now the plist is parsed, we are now interested in testing regexes on the string and see
    %% which one we matched the most?
    #{"repository" := Repository, "patterns" := Patterns} = Final,
    Classes = classify(Buffer,Patterns, Repository),
    erlang:display(Classes),
    %% VSCode has something that they call modifiers like (exported functions) which determines if something should be underlined or not
    MarkupMap = #{
        "entity.name.function" => [{fontstyle, [underline]},
                                   {foreground, "\e[90m"}],
        "entity.name.type" => [{fontstyle, [underline]},
                               {foreground, "\e[91m"}],
        "keyword" => [{foreground, "\e[92m"}],
        "keyword.operator" =>[{foreground, "\e[93m"}],
        "string" =>[{foreground, "\e[94m"}],
        "punctuation" =>[{foreground, "\e[95m"}],
        "constant.other.symbol" => [{foreground, "\e[96m"}],
        "constant.chatacter" => [{foreground, "\e[97m"}],
        "constant.numeric" => [{foreground, "\e[97m"}],
        "variable" =>[{foreground, "\e[98m"}]
    },
    %% Now insert ansi escape sequences at the indexes provided by Classes
    MarkupActions = [{Ix, markup(Class, MarkupMap)}||{Ix, Class} <- Classes],
    erlang:display({markupactions, MarkupActions}),
    apply_markup(Buffer, 0, MarkupActions, []).

%% Clear after last markup, NOTE that we cant have nested markups if that is the case
%% the outer markup may be cleared in the end of the inner markup.
%% TODO: This no longer works because we have classes within classes...
apply_markup(Buffer, Index, [{{Index,_},clear}|MarkupList], Acc) ->
    apply_markup(Buffer, Index, MarkupList, ["\e[0m"|Acc]);
apply_markup(Buffer, Index, [{{Index,_},clear},|MarkupList], Acc) ->
    apply_markup(Buffer, Index, MarkupList, ["\e[0m"|Acc]);
apply_markup([],_,_,Acc) ->
    lists:flatten(lists:reverse(Acc));
%% Discard 0 length markups the will not be visible
apply_markup(Buffer, Index, [{{_,0},_}|MarkupList], Acc) ->
    apply_markup(Buffer, Index, MarkupList, Acc);
%% We should apply the markup here including C, NOTE that ACC is the buffer in reverse
apply_markup([C|Buffer], Index, [{{Index,Length},Markup}|MarkupList], Acc) ->
    erlang:display({insert_at, Index}),
    apply_markup(Buffer, Index+1, [{{Index+Length,ignore}, clear}|MarkupList], [C,Markup|Acc]);
apply_markup([C|Buffer], Index, MarkupList, Acc) ->
    apply_markup(Buffer, Index+1, MarkupList, [C|Acc]).
markup(Class, MarkupMap) ->
    case maps:get(Class, MarkupMap, none) of
        none ->
            case string:split(Class, ".", trailing) of
                [Class1, _] -> markup(Class1, MarkupMap);
                [Class1] -> case maps:get(Class1, MarkupMap, none) of
                    none -> "";
                    Markup -> mu(Markup)
                end
            end;
        Markup ->
            mu(Markup)
    end.
mu(Markup) ->
    AppliedFontStyle = case proplists:get_value(fontstyle, Markup, none) of
        none -> [];
        FontStyles ->
            [fontstyle2ansi(FS) || FS <- FontStyles]
    end,
    AppliedForeground = case proplists:get_value(foreground, Markup, none) of
        none -> [];
        Foreground ->
            color2ansi(fg, Foreground)
    end,
    AppliedBackground = case proplists:get_value(background, Markup, none) of
        none -> [];
        Background ->
            color2ansi(bg, Background)
    end,
    AppliedFontStyle ++ AppliedForeground ++ AppliedBackground.

fontstyle2ansi(bold) -> "\e[1m";
fontstyle2ansi(italic) -> "\e[3m";
fontstyle2ansi(underline) -> "\e[4m";
fontstyle2ansi(strikethrough) -> "\e[9m";
fontstyle2ansi(_) -> "".
color2ansi(Type, [$#,Rx1,Rx2,Gx1,Gx2,Bx1,Bx2]) ->
    R = integer_to_list(list_to_integer([Rx1,Rx2], 16), 10),
    G = integer_to_list(list_to_integer([Gx1,Gx2], 16), 10),
    B = integer_to_list(list_to_integer([Bx1,Bx2], 16), 10),
    Prefix = case Type of
        bg -> "\e[48;2;";
        fg -> "\e[38;2;"
    end,
    Prefix ++lists:flatten([R,$;,G,$;,B])++"m";
color2ansi(_, [$\e|_]=SystemColor) ->
    %% NOTE: This is when user already specified the ansi sequence
    SystemColor.
%% This below is when you have specified a system color with a name
%% implemented in prim_tty
% color2ansi(Type, SystemColor) ->
%     case Type of
%         fg -> prim_tty:fg_color(SystemColor);
%         bg -> prim_tty:bg_color(SystemColor)
%     end.

classify(Buffer, Patterns, Repository) ->
    put(run_before, #{}),
    put(repository, Repository),
    Classes = [Class || {_,_}=Class <- lists:uniq(lists:sort(lists:flatten([begin
        %erlang:display({classify, Pattern, returned, classify1(Buffer, [Pattern], [])}),
        %erlang:display({finish, Class}),
        classify1(Buffer, [Pattern], [])
        
    end || Pattern <- Patterns])))],
    %% Classes is a list of {{offset, length}, ClassName}
    %% ClassName can later be used to get a color
    erlang:display({classify, returned, Classes}),
    put(run_before, undefined),
    Classes.
classify1(_, [], _) ->
    [];
classify1(Buffer, [#{"patterns" := Patterns1}=P|_Patterns], Repository) when map_size(P) =:= 1 ->
    Classes = [Class || {_,_}=Class <- lists:uniq(lists:sort(lists:flatten([begin
        %erlang:display({patterns, Class}),
        classify1(Buffer, [Pattern], Repository)
    end || Pattern <- Patterns1 ])))],
    erlang:display({classify, patterns, Buffer, returned, Classes}),Classes;
    % case Classes of
    %     [] -> classify1(Buffer, Patterns, Repository);
    %     _ -> Classes
    % end;
classify1(Buffer, [#{"include" := [$#|Include]}|Patterns], Repository) ->
    RunBefore = get(run_before),
    %erlang:display(RunBefore),
    case maps:get({Buffer, Include}, RunBefore, none) of
        none ->
            Pattern = maps:get(Include, get(repository)),
            erlang:display({inside, Buffer, Include}),
            put(run_before, RunBefore#{{Buffer, Include} => true}),
            R = lists:flatten(classify1(Buffer, [Pattern|Patterns], Repository)),
            put(run_before, RunBefore#{{Buffer, Include,content} => R}),
            %put(run_before, RunBefore#{{Buffer, Include} => R}),
            erlang:display({classify, Buffer, Include, returned, R}),
            R;
        _ ->
            []
    end;
classify1(Buffer,[#{"begin" := BegPattern,
                     "end" := EndPattern}=P|Patterns], Repository) ->
    %% TODO, first check BegPattern, then if we match check the rest
    case re:run(Buffer, BegPattern ++ "(.*?)" ++ EndPattern, [{capture, all, index}, global]) of
        {match, ListMatch} ->
            R = lists:flatten([begin
                
            %erlang:display({match, BegPattern ++ "(.*)" ++ EndPattern, Captures}),
            {Captures1, _BeginCaptures} = case maps:get("beginCaptures", P, []) of
                [] -> case maps:get("captures", P, []) of
                    [] -> {[], []};
                    CapturesMap ->
                        Keys = maps:keys(CapturesMap),
                        {[{lists:nth(list_to_integer(Key), Captures), maps:get("name", maps:get(Key, CapturesMap))} || Key <- Keys], CapturesMap}
                    end;
                CapturesMap ->
                    Keys = maps:keys(CapturesMap),
                    {[{lists:nth(list_to_integer(Key), Captures), maps:get("name", maps:get(Key, CapturesMap))} || Key <- Keys], CapturesMap}
            end,
            NBeginCaptures = length(Captures1),
            ContentCapture = lists:nth(NBeginCaptures+1, Captures),
            {Captures2, _EndCaptures} = case maps:get("endCaptures", P, []) of
                [] -> case maps:get("captures", P, []) of
                    [] -> {[], []};
                    CapturesMap1 ->
                        Keys1 = maps:keys(CapturesMap1),
                        {[{lists:nth(list_to_integer(Key)+NBeginCaptures+1, Captures), maps:get("name", maps:get(Key, CapturesMap1))} || Key <- Keys1], CapturesMap1}
                    end;
                CapturesMap1 ->
                    Keys1 = maps:keys(CapturesMap1),
                    {[{lists:nth(list_to_integer(Key)+NBeginCaptures+1, Captures), maps:get("name", maps:get(Key, CapturesMap1))} || Key <- Keys1], CapturesMap1}
            end,
            FirstCaptureClass = case maps:get("name", P, undefined) of
                undefined ->
                    [];
                ClassName ->
                    [{First, ClassName}]
            end,
            case maps:get("contentName", P, undefined) of
                undefined ->
                    case maps:get("patterns", P, []) of
                        [] ->
                            FirstCaptureClass ++ Captures1 ++ Captures2;
                        Patterns1 ->
                            {Offset1, Length1} = First,
                            SS = string:slice(Buffer, Offset1, Length1),
                            erlang:display({checking_content_patterns, SS}),
                            %% According to documentation, patterns should be applied on where the start matches until the end matches or end of the document
                            %% But we do not want to run the function twice
                            ContentCaptures = lists:uniq(lists:sort([
                                begin
                                    Class
                                end || Pattern <- Patterns1, {_,_}=Class <- classify1(SS, [Pattern], Repository)])),
                            erlang:display({done_checking_content_patterns, ContentCaptures}),
                            %% ContentCaptures is a list of {{offset, length}, ClassName}
                            %% add the ContentOffset to the offset of each capture
                            ContentCaptures1 = [{{Offset+Offset1, Length}, ClassName} || {{Offset, Length}, ClassName} <- ContentCaptures],   
                            FirstCaptureClass ++ Captures1 ++ ContentCaptures1 ++ Captures2
                    end;
                ContentName ->
                    FirstCaptureClass ++ Captures1 ++ [{ContentCapture, ContentName}] ++ Captures2
            end
        end || [First|Captures] <- ListMatch]),
            erlang:display({classify, Buffer, BegPattern ++ "(.*)" ++ EndPattern, returned, R}),R;
        nomatch ->
            classify1(Buffer, Patterns, Repository)
    end;
classify1(Buffer, [#{"captures" := CapturesMap, "match" := Match}=P|Patterns], Repository) ->
    %% What alternative do we have to \\b, punctuations will not match, but we do not want to catch things as atoms...
    case re:run(Buffer, Match, [{capture, all, index}, global]) of
        {match, ListMatch} ->
            R = lists:flatten([begin
                %erlang:display({match, Match, Captures}),
                FirstCaptureClass = case maps:get("name", P, undefined) of
                    undefined ->
                        [];
                    ClassName ->
                        [{First, ClassName}]
                end,
                Keys = maps:keys(CapturesMap),
                Keys1 = lists:reverse(lists:nthtail(length(Captures), lists:reverse(Keys))),
                Captures1 = [{lists:nth(list_to_integer(Key), Captures), maps:get("name", maps:get(Key, CapturesMap))} || Key <- Keys1],
                FirstCaptureClass ++ Captures1
            end
            || [First|Captures] <- ListMatch]),
                erlang:display({classify, Buffer, Match, returned, R}),
                R;
        nomatch ->
            classify1(Buffer, Patterns, Repository)
    end;
classify1(Buffer, [#{"match" := Match}=P|Patterns], Repository) ->
    case re:run(Buffer, Match, [{capture, first, index}, global]) of
        {match, ListMatch} ->
            
            R = lists:flatten([begin
                case maps:get("name", P, undefined) of
                    undefined ->
                        [];
                    ClassName ->
                        [{First, ClassName}]
                end
            end || [First] <- ListMatch]),
                erlang:display({classify, Buffer, Match, returned, R}),
            R;
        nomatch ->
            classify1(Buffer, Patterns, Repository)
    end.

multi_line_prompt(Pbs) ->
    case application:get_env(stdlib, shell_multiline_prompt, default) of
        default -> %% Default multiline prompt
            shell:default_multiline_prompt(Pbs);
        {M,F} when is_atom(M), is_atom(F) ->
            case catch apply(M,F,[Pbs]) of
                Prompt when is_list(Prompt) -> Prompt;
                _ ->
                    application:set_env(stdlib, shell_multiline_prompt, default),
                    io:format("Invalid call: ~p:~p/1~n", [M,F]),
                    shell:default_multiline_prompt(Pbs)
            end;
        Prompt when is_list(Prompt) ->
            lists:duplicate(max(0,shell:prompt_width(Pbs) - shell:prompt_width(Prompt)), $\s) ++ Prompt;
        Prompt ->
            application:set_env(stdlib, shell_multiline_prompt, default),
            io:format("Invalid multiline prompt: ~p~n", [Prompt]),
            shell:default_multiline_prompt(Pbs)
    end.

redraw(Pbs, {_,{_,_},_}=L, Rs) ->
    [{redraw_prompt, Pbs, multi_line_prompt(Pbs), L} |Rs].

chars_before({[],{Bef,_},_}) ->
    Bef;
chars_before({LB,{Bef,_},_}) ->
    lists:flatten(lists:join($\n, [Bef| [reverse(Line)|| Line <- LB]])).

length_before({line,Pbs,{_,{Bef,_Aft},_},_}) ->
    cp_len(Pbs) + cp_len(Bef).

length_after({line,_,{_,{_Bef,Aft},_},_}) ->
    cp_len(Aft).

prompt({line,Pbs,_,_}) ->
    Pbs.

current_chars({line,_,MultiLine,_}) ->
    current_line(MultiLine).
current_line({line,_,MultiLine,_}) ->
    current_line(MultiLine) ++ "\n";
%% Convert a multiline tuple into a string with new lines
current_line({LinesBefore, {Before, After}, LinesAfter}) ->
    CurrentLine = lists:reverse(Before, After),
    unicode:characters_to_list(lists:flatten(
        lists:filter(
            fun (X) ->
                    X /= []
                end,
                lists:join($\n, lists:reverse(LinesBefore) ++ [CurrentLine] ++ LinesAfter)))).

%% Grapheme length in codepoints
gc_len(CP) when is_integer(CP) -> 1;
gc_len(CPs) when is_list(CPs) -> length(CPs).

%% String length in codepoints
cp_len(Str) ->
    cp_len(Str, 0).

cp_len([GC|R], Len) ->
    cp_len(R, Len + gc_len(GC));
cp_len([], Len) -> Len.
