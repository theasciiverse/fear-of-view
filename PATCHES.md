# Changes from upstream

This is the Asciiverse packaging of [Fear of View](https://mbays.sdf.org/fov/)
by Martin Bays (AGPL-3.0-or-later), built from upstream's `master` at
`2e3363a` (0.2.0.0, 8 August 2026; upstream publishes releases on Hackage
and its own page, not as git tags). Upstream is the `upstream` remote
(https://thegonz.net/~fov/fearOfView.git); the `asciiverse` branch carries
the changes below. `UPSTREAM.md` is the same list written for Martin.

The catalog builds the curses edition only (`-f curses -f -bear`): it plays
in the terminal it is started from. The game, its keys and its screen are
upstream's, unchanged; it is drawn in an area of about 60x20 and plays at
80x24 and up.

## Ours

- **The player's files in the state folder.** The game state (saved after
  every move) and the highscores went to `$XDG_DATA_HOME/fearOfView`. A new
  player's now go to `$XDG_STATE_HOME/fear-of-view`, else
  `~/.local/state/fear-of-view` (`%LOCALAPPDATA%\fear-of-view` on
  Windows). A folder made by an earlier version is still used, and
  `--statefile` still overrides the game state's path (`GameName.hs`,
  `Main.hs`, `HighscoreFile.hs`; the `directory` lower bound rises to
  1.3.7, the first with `XdgState`).

## Packaging

- `asciiverse/build.sh`: the release build. One static binary, glibc,
  ncursesw, gmp and the rest linked in, so players install nothing.
  Terminfo comes from the player's system, as for any curses program.
- `asciiverse/cc-static.sh`: the linker GHC uses for that build; it names
  ncursesw and tinfo again at the end, which a static link needs after
  hscurses.
- `.github/workflows/asciiverse-release.yml`: builds Linux x86-64 and
  ARM64 on Ubuntu 22.04 with GHC 9.8.2, smoke-tests each binary, and
  attaches archives and SHA256SUMS to the release.
- `asciiverse/smoke.py`: the smoke test (80x24, empty HOME, scratch
  `XDG_STATE_HOME`).
- `asciiverse/fear-of-view.toml`, `.md`, `.cast`: the catalog manifest,
  store page and 100x30 preview, drafts for `hub/catalog`.
- `.gitignore`: build folders.

## Not changed

- No session log: the game keeps its state after every move already, and a
  log would mean threading a handle through the UI monad, which is game
  code, not packaging.
