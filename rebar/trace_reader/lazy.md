lazy



OtherPid = spawn(fun() -> receive _ -> ok after 100000 -> ok end end).
Name = "my_traces/traces".
Terms = [{Name,
        [
        {trace, self(), spawned, user},
        {trace, self(), spawn, OtherPid},
        {trace, OtherPid, spawned, self()},
        {trace, self(), send, hello, OtherPid},
        {trace, OtherPid, 'receive', hello},
        {trace, OtherPid, send, good_day, self()},
        {trace, self(), 'receive', good_day}
]},
{Name ++ ".1",
        [
        {trace, OtherPid, exit, normal},
        {trace, self(), call, {io, format, ["Test~n"]}, {test_module, test, 1, {file, 1337}}},
        {trace, self(), send, <<"Test\n">>, stdio},
        {trace, self(), return_from, {io, format, 1}, ok},
        {trace, self(), return_to, {test_module, test, 1}}
]},
{Name ++ ".2",
        [
        {trace, self(), call, {test_module, result, [ok]}, {test_module, test, 1, {file, 1339}}},
        {trace, self(), call, {test_module, result2, [ok]}, {test_module, result, 1, {file, 800}}},
        {trace, self(), return_from, {test_module, result2, 1}, ok},
        {trace, self(), return_to, {test_module, test, 1}},
        {trace, self(), exit, normal}
        ]}
].



[begin
  {ok, Fd} = file:open(File, [write, binary]),
  [file:write(Fd, term_to_binary(T)) || T <- Traces],
  file:close(Fd)
 end || {File, Traces} <- Terms ].

trace_reader:start().
trace_reader:load_traces("my_traces/").




rr(trace_reader_server).
find_clause_line_eval:get_matching_clause_line("src/trace_reader_server.erl", handle_call, [reverse_next, 0, #state{focused_pid="traces.1"}]).

record_bindings(
[{state,{attribute,{12,2},
                                record,
                                {state,[{typed_record_field,{record_field,{13,5},
                                                                          {atom,{13,5},trace_dir}},
                                                            {type,{13,18},
                                                                  union,
                                                                  [{type,{13,18},string,[]},{atom,{13,29},undefined}]}},
                                        {typed_record_field,{record_field,{14,5},
                                                                          {atom,{14,5},sorted_files}},
                                                            {type,{14,21},list,[{type,{14,22},string,[]}]}},
                                        {typed_record_field,{record_field,{15,5},
                                                                          {atom,{15,5},current_file_index}},
                                                            {type,{15,27},integer,[]}},
                                        {typed_record_field,{record_field,{16,5},
                                                                          {atom,{16,5},current_term_index}},
                                                            {type,{16,27},integer,[]}},
                                        {typed_record_field,{record_field,{18,5},
                                                                          {atom,{18,5},loaded_files}},
                                                            {type,{18,21},
                                                                  map,
                                                                  [{type,{18,46},
                                                                         map_field_assoc,
                                                                         [{ann_type,{18,23},
                                                                                    [{var,{18,23},'FileIndex'},{type,{18,36},integer,[]}]},
                                                                          {type,{18,49},
                                                                                list,
                                                                                [{type,{18,50},
                                                                                       tuple,
                                                                                       [{ann_type,{18,51},
                                                                                                  [{var,{18,51},'TermInFileIndex'},
                                                                                                   {type,{18,70},non_neg_integer,[]}]},
                                                                                        {ann_type,{18,89},
                                                                                                  [{var,{18,89},'Term'},{type,{18,97},term,[]}]}]}]}]}]}},
                                        {typed_record_field,{record_field,{19,5},
                                                                          {atom,{19,5},focused_pid}},
                                                            {type,{19,20},
                                                                  union,
                                                                  [{type,{19,20},pid,[]},{atom,{19,28},undefined}]}},
                                        {typed_record_field,{record_field,{20,5},
                                                                          {atom,{20,5},active_pids}},
                                                            {type,{20,20},list,[{type,{20,21},pid,[]}]}},
                                        {typed_record_field,{record_field,{21,5},
                                                                          {atom,{21,5},breakpoint_next_index}},
                                                            {type,{21,30},non_neg_integer,[]}},
                                        {typed_record_field,{record_field,{22,5},
                                                                          {atom,{22,5},breakpoints}},
                                                            {type,{22,20},
                                                                  list,
                                                                  [{type,{22,21},
                                                                         tuple,
                                                                         [{type,{22,22},non_neg_integer,[]},
                                                                          {ann_type,{22,41},
                                                                                    [{var,{22,41},'BPid'},
                                                                                     {type,{22,49},
                                                                                           union,
                                                                                           [{type,{22,49},pid,[]},{atom,{22,57},all}]}]},
                                                                          {ann_type,{22,64},
                                                                                    [{var,{22,64},'MFA'},
                                                                                     {type,{22,71},
                                                                                           tuple,
                                                                                           [{type,{22,72},atom,[]},
                                                                                            {type,{22,80},atom,[]},
                                                                                            {type,{22,88},arity,[]}]}]},
                                                                          {ann_type,{22,98},
                                                                                    [{var,{22,98},'StopFun'},{user_type,{22,109},break_fun,[]}]},
                                                                          {ann_type,{22,122},
                                                                                    [{var,{22,122},'SkipCount'},
                                                                                     {type,{22,135},non_neg_integer,[]}]}]}]}},
                                        {typed_record_field,{record_field,{23,5},
                                                                          {atom,{23,5},callstack}},
                                                            {type,{23,18},
                                                                  list,
                                                                  [{type,{23,19},
                                                                         tuple,
                                                                         [{ann_type,{23,20},
                                                                                    [{var,{23,20},'MFA'},
                                                                                     {type,{23,27},
                                                                                           tuple,
                                                                                           [{type,{23,28},atom,[]},
                                                                                            {type,{23,35},atom,[]},
                                                                                            {type,{23,42},arity,[]}]}]},
                                                                          {type,{23,52},
                                                                                tuple,
                                                                                [{ann_type,{23,53},
                                                                                           [{var,{23,53},'File'},
                                                                                            {type,{23,61},
                                                                                                  union,
                                                                                                  [{type,{23,61},string,[]},{atom,{23,72},undefined}]}]},
                                                                                 {ann_type,{23,83},
                                                                                           [{var,{23,83},'Line'},
                                                                                            {type,{23,91},
                                                                                                  union,
                                                                                                  [{type,{23,91},non_neg_integer,[]},
                                                                                                   {atom,{23,111},undefined}]}]}]}]}]}},
                                        {typed_record_field,{record_field,{24,5},
                                                                          {atom,{24,5},last_reply}},
                                                            {type,{24,19},
                                                                  union,
                                                                  [{type,{24,19},
                                                                         tuple,
                                                                         [{type,{24,20},atom,[]},{type,{24,28},term,[]}]},
                                                                   {type,{24,38},atom,[]},
                                                                   {atom,{24,47},undefined}]}}]}}}],
[{'A',{state,undefined,undefined,undefined,
                          undefined,undefined,undefined,undefined,undefined,undefined,
                          undefined,undefined}},
 {'B',{state,undefined,undefined,undefined,
                          undefined,undefined,undefined,undefined,undefined,undefined,
                          undefined,undefined}}])