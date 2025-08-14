# Fear of View
A terminal broughlike game about manipulating vision.

Released under the AGPL, version 3 or later. See COPYING.

![Screenshot](fov-shot.png "Screenshot")

## Building
Dependencies:
* GHC and cabal: package `cabal-install` on Debian, `dev-haskell/cabal` on Gentoo, etc.
* ncurses

Run with `cabal run`.
Install with `cabal install`.

### Bearlibterminal
In case you don't have or like ncurses, the game also supports this alternative terminal library which runs in graphical mode (via OpenGL).
Install [bearlibterminal](http://foo.wyrd.name/en:bearlibterminal), then run with `cabal run -fbear`.
