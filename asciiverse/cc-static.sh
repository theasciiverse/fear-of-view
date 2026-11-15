#!/bin/sh
# The C compiler GHC links through for the static release build: the same
# cc, with ncursesw and tinfo named once more at the end of the command, as
# a static link needs them after hscurses (some distributions keep curses
# functions such as define_key in libtinfo).
exec "${CC:-cc}" "$@" -lncursesw -ltinfo
