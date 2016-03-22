# pyinit.py --- A Python startup file that improves the REPL
# functionality
#
# Copyright (C) 2013, 2014 Brandon Invergo <brandon@invergo.net>
#
# Author: Brandon Invergo <brandon@invergo.net>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.


from __future__ import print_function

import os.path
import sys
import atexit
import readline
import rlcompleter
from keyword import iskeyword


ANSI_COLS = {"black": 30, "red": 31, "green": 32, "yellow": 33, "blue": 34,
             "purple": 35, "cyan": 36, "white": 37}


class printable_function(object):
    """This class will hold a function that includes the lines that began
    and ended its definition in the readline history, so that the
    function's definition may be easily recalled.

    """
    def __init__(self, func, start, end):
        self.start = start
        self.end = end
        self.func = func

    def __call__(self, *args):
        self.func(*args)

    def __str__(self):
        """Just return a string joining each of the lines between the
        function's beginning and end.

        """
        lines = []
        for i in range(self.start, self.end + 1):
            lines.append(readline.get_history_item(i))
        return "\n".join(lines)


class saved_function(object):
    """A function decorator that defines the start and end of the function
    definition in the readline history.  You can then, for example,
    "print" the function, which will show you its definition.  i.e.:

    [26]> @saved_function(27)
    (26)> def foo(bar):
    (26)>     print(bar)
    [29]> print(foo)
    def foo(bar):
        print(bar)

    """
    def __init__(self, start):
        """The function start is taken as an argument.  The function end is
        figured out from the current history length

        """
        self.start = start
        self.end = readline.get_current_history_length()

    def __call__(self, func):
        """Return a printable_function instance of the function

        """
        return printable_function(func, self.start, self.end)


def ansi_colorize(text, color, state, prompt=False):
    """Wrap a string in ANSI color escape characters.  This function also
    adds some extra escapes specifically so Readline will be able to
    accurately judge the length of the color prompt.  The "state" can
    be regular, bold, underline or bright.

    """
    mod = {"regular": "0;", "bold": "1;", "underline": "4;"}.get(state)
    if mod is None:
        mod = ""
    col = ANSI_COLS.get(color)
    if col is None:
        col = 0
        mod = ""
    elif state == "bright":
        col += 60
        mod = "0;"
    if prompt:
        return "\001\033[{0}{1}m\002{2}\001\033[00m\002".format(mod, col, text)
    return "\033[{0}{1}m{2}\033[00m".format(mod, col, text)


class CustomPS1:
    prompt = "[{0}]> "

    def __str__(self, prompt=True):
        """Return a bold green string containing the current place in Readline
        history.

        """
        line_no = readline.get_current_history_length() + 1
        cur_prompt = self.prompt.format(line_no)
        return "{0}".format(ansi_colorize(cur_prompt, "green", "bold",
                                          prompt=prompt))

    def __len__(self):
        """Return the length of the prompt without any escape sequences."""
        line_no = readline.get_current_history_length() + 1
        cur_prompt = self.prompt.format(line_no)
        return len(cur_prompt)


# define custom secondary prompt based on readline history number
class CustomPS2:
    prompt = "({0})> "

    def __str__(self, prompt=True):
        """Return a bold yellow string containing the current place in
        Readline history.

        """
        line_no = readline.get_current_history_length() + 1
        cur_prompt = self.prompt.format(line_no)
        return "{0}".format(ansi_colorize(cur_prompt, "yellow", "bold",
                                          prompt=prompt))

    def __len__(self):
        """Return the length of the prompt without any escape sequences."""
        line_no = readline.get_current_history_length() + 1
        cur_prompt = self.prompt.format(line_no)
        return len(cur_prompt)


def setup_prompt():
    """Set up the prompts."""
    sys.ps1 = CustomPS1()
    sys.ps2 = CustomPS2()


def rl_autoindent():
    """Auto-indent upon typing a new line according to the contents of the
    previous line.  This function will be used as Readline's
    pre-input-hook.

    """
    hist_len = readline.get_current_history_length()
    last_input = readline.get_history_item(hist_len)
    try:
        last_indent_index = last_input.rindex("    ")
    except:
        last_indent = 0
    else:
        last_indent = int(last_indent_index / 4) + 1
    if len(last_input.strip()) > 1:
        if last_input.count("(") > last_input.count(")"):
            indent = ''.join(["    " for n in range(last_indent + 2)])
        elif last_input.count(")") > last_input.count("("):
            indent = ''.join(["    " for n in range(last_indent - 1)])
        elif last_input.count("[") > last_input.count("]"):
            indent = ''.join(["    " for n in range(last_indent + 2)])
        elif last_input.count("]") > last_input.count("["):
            indent = ''.join(["    " for n in range(last_indent - 1)])
        elif last_input.count("{") > last_input.count("}"):
            indent = ''.join(["    " for n in range(last_indent + 2)])
        elif last_input.count("}") > last_input.count("{"):
            indent = ''.join(["    " for n in range(last_indent - 1)])
        elif last_input[-1] == ":":
            indent = ''.join(["    " for n in range(last_indent + 1)])
        else:
            indent = ''.join(["    " for n in range(last_indent)])
    readline.insert_text(indent)


def comp_disp_matches(substitution, matches, longest_match_length):
    """Display completion matches, colorized according to their syntactic
    role.

    """
    hist_len = readline.get_current_history_length()
    last_input = readline.get_history_item(hist_len)
    for n, m in enumerate(matches):
        (mod, d, w) = m.rpartition('.')
        w_strip = w.strip('(')
        # Parent modules: bold yellow, with the children highlighted
        # according to their syntactic role
        if mod != "":
            mod_str = "".join([ansi_colorize(mod, "yellow", "bold"), '.'])
        else:
            mod_str = ""
        # Function: bright cyan
        if w[-1] == "(":
            w_str = ansi_colorize(w, "cyan", "bright")
        # Keywords: bright yellow
        elif iskeyword(w_strip):
            w_str = ansi_colorize(w, "yellow", "bright")
        # Modules: bold yellow
        elif w_strip in sys.modules:
            w_str = ansi_colorize(w, "yellow", "bold")
        # Hidden: red
        elif w_strip[:2] == "__" and w_strip[-2:] == "__":
            w_str = ansi_colorize(w, "red", "regular")
        # Private: bright purple
        elif w_strip[0] == "_":
            w_str = ansi_colorize(w, "purple", "bright")
        # Otherwise: normal
        else:
            w_str = w
        # 3 columns of matches per line
        if n % 3 == 0:
            sys.stdout.write("\n")
        # Get the total length of escape sequences
        col_len = (len(mod_str) + len(w_str)) - (len(mod) + len(w))
        # Write columns with widths set by the longest match length
        sys.stdout.write("{0:{1}} ".format("{0}{1}".format(mod_str, w_str),
                                           longest_match_length + col_len))
    # Redisplay the prompt
    if last_input.strip()[-1] in ["(", "[", "{", ":"]:
        sys.stdout.write("\n{0}{1}".format(sys.ps2.__str__(prompt=False),
                                           readline.get_line_buffer()))
    else:
        sys.stdout.write("\n{0}{1}".format(sys.ps1.__str__(prompt=False),
                                           readline.get_line_buffer()))


def setup_readline():
    """Initialize the readline module."""
    histpath = os.path.join(os.path.expanduser("~"), ".local", "share",
                            "python")
    if sys.version[0] == '2':
        histfile = os.path.join(histpath, "py2hist")
    else:
        histfile = os.path.join(histpath, "py3hist")
    if not os.path.exists(histpath):
        os.mkdir(histpath)
    atexit.register(readline.write_history_file, histfile)
    try:
        readline.read_history_file(histfile)
    except IOError:
        pass
    # Complete with the tab key. M-tab completes on local file names.
    readline.parse_and_bind("tab: complete")
    # readline indentation keybinding
    # Control-j: indent 4 spaces
    # Control-u: unindent 4 spaces
    # First, create some dummy shortcuts:
    # Add four spaces:
    readline.parse_and_bind(r'"\M-j": "    "')
    # Delete four characters (behind point):
    readline.parse_and_bind(r'"\M-h": delete-char')
    readline.parse_and_bind(r'"\M-k": "\M-h\M-h\M-h\M-h"')
    # Move point forward four characters:
    readline.parse_and_bind(r'"\M-e": "\C-f\C-f\C-f\C-f"')
    # Move point backward four characters:
    readline.parse_and_bind(r'"\M-g": "\C-b\C-b\C-b\C-b"')
    # Second, define another set-mark shortcut, since it only seems to
    # work when bound to a letter key.
    readline.parse_and_bind(r'"\M-\C-j": set-mark')
    # C-j macro: set mark, go to the beginning of the line, add four
    # spaces, exchange point and mark, and then move forward four
    # characters to the same point in the text where you were before,
    # regardless of the new indentation.
    readline.parse_and_bind(r'"\C-j": "\M-\C-j\C-a\M-j\C-x\C-x\M-e"')
    # C-u macro: Move back four characters, set mark, move to the
    # beginning of the line, move forward four characters, delete four
    # characters, then exchange mark and point. This would be shorter
    # readline.parse_and_bind(r'"\C-u": "\M-g\M-\C-j\C-a\M-e\M-k\C-x\C-x"')
    readline.parse_and_bind(r'"\C-u": "\M-g\M-\C-j\C-a\M-k\C-x\C-x"')
    # load readline history
    readline.set_pre_input_hook(rl_autoindent)
    readline.set_completion_display_matches_hook(comp_disp_matches)
    # write the history file on exit


if __name__ == "__main__":
    setup_readline()
    setup_prompt()
