import sys
import re
from pygments import highlight
from pygments.lexers import ErlangLexer
from pygments.formatters import TerminalFormatter

def remove_ansi_escape_sequences(s):
       ansi_escape = re.compile(r'\x1b\[[0-9;]*[A-Za-z]')
       return ansi_escape.sub('', s)

def colorize_erlang_code(code):
   code = remove_ansi_escape_sequences(code)
   return highlight(code, ErlangLexer(), TerminalFormatter())

if __name__ == '__main__':
   code_file = sys.argv[1]
   with open(code_file) as f:
      code = f.read()
      print(colorize_erlang_code(code), end="")