# For upstream

Hi Martin. One small change on this branch could be useful to you as it
stands; the rest is our build.

## The state folder

`GameName.hs` gains `stateDir`, and `Main.hs` and `HighscoreFile.hs` use it
for `gamestate` and `highscores`. It is `getXdgDirectory XdgState
"fear-of-view"` (`$XDG_STATE_HOME/fear-of-view`, else
`~/.local/state/fear-of-view`; `%LOCALAPPDATA%` on Windows), unless
`$XDG_DATA_HOME/fearOfView` already exists, in which case that is kept so
nobody loses a game in progress or their scores. The XDG spec puts state
that persists between runs but is not portable data there. It needs
`directory >= 1.3.7`, so the cabal bound rises. If you prefer the folder
name `fearOfView`, it is one string.

## The static build

`asciiverse/build.sh` links everything statically
(`-pgml asciiverse/cc-static.sh -optl-static -optl-pthread`), so the binary
runs on any Linux with nothing installed. The only trick is the linker
wrapper, which repeats `-lncursesw -ltinfo` at the end of the link line:
on current Debian and Ubuntu `define_key` lives in libtinfo, and a static
link resolves libraries in order. If you would like a ready Linux binary on
your page, this is all it takes.
