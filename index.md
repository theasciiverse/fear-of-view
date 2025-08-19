# Fear of View
A terminal broughlike game about manipulating vision.

![Screenshot](fov-shot.png "Screenshot")

* [Gameplay animation](fov-anim.gif)

Released under the AGPL, version 3 or later.

* [Changelog](CHANGELOG.md)

## Play by ssh
* ssh fov@sshgames.thegonz.net
* Password: fov

## Binaries
### Linux binary
Unpack the following tarball and run the script `fearOfView.sh` in a terminal.

* [fearOfView-linux-x86\_32-glibc-2.31.tgz](fearOfView-linux-x86_32-glibc-2.31.tgz)

It should work on any reasonably up-to-date system (Debian 11+); please let me know if it doesn't.

### Windows binary
Unpack the following zip and run `fearOfView.exe`.

* [fearOfView-x86\_64-mingw-bear.zip](fearOfView-x86_64-mingw-bear.zip)

## Cabal
This is the most versatile installation method, but requires you to have cabal-install and ncurses installed. It should then just be a matter of:

```
cabal update && cabal install fearOfView
```

If you want to build with Bearlibterminal instead of ncurses, obtain the source and read the README.

## Source
```
git clone https://thegonz.net/~fov/fearOfView.git
```

* [Browsable source tree on Hackage](https://hackage.haskell.org/package/fearOfView/src/)

## Feedback
I welcome feedback by email to [mbays@sdf.org](mailto:mbays@sdf.org).
