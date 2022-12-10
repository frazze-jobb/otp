# coding=utf-8
#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2013-2023. All Rights Reserved.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 
# %CopyrightEnd%
#
import gdb
from etp_gdb_lib import eterm
class EtermMIPrettyPrinter:
    hint = None
    eterm = None
    eterm_header = None
    eterm_type = None
    eterm_string = None    
    eterm_children = None
    def __init__(self, val):
        self.eterm = val # the eterm number
        etpDict = eterm(val)
        self.eterm_header = etpDict['header']
        self.eterm_type = etpDict['type']
        self.eterm_string = etpDict['string']
        self.hint = etpDict['hint']
        if self.hint == 'array' or self.hint == 'map':
            self.eterm_children = etpDict['children']

    # GDB expects these functions to be implemented
    def to_string(self):
        return self.eterm_type
    def display_hint(self):
        return self.hint
    def children(self):
        if self.hint == 'array':
            for i, item in enumerate(self.eterm_children):
                yield str(i), item
        if self.hint == 'map':
            for i in range(len(self.eterm_children)):
                entry = self.eterm_children[i]
                key = entry['first']
                value = entry['second']
                yield str(key), value

    def eterm_pretty_printer(val):
        if str(val.type) == 'Eterm' or\
           str(val.type) == 'ErtsTracer':
           return EtermMIPrettyPrinter(val)
        return None

gdb.pretty_printers.append(EtermMIPrettyPrinter.eterm_pretty_printer)
