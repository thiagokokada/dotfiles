import atexit
import os
import readline
import rlcompleter
import sys
from pprint import pprint

# Use python history file in REPL
histfile = os.path.join(os.path.expanduser("~"), '.python_history')
try:
    readline.read_history_file(histfile)
except FileNotFoundError:
    pass
readline.set_history_length(10000)
readline.parse_and_bind("tab: complete")
atexit.register(readline.write_history_file, histfile)
