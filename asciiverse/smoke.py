"""smoke.py FEAROFVIEW_BINARY: start the game in an 80x24 pseudo-terminal
from another directory, with an empty HOME and a scratch XDG_STATE_HOME,
start a game and take a step, and fail unless the board was drawn and the
game state was saved in XDG_STATE_HOME/fear-of-view."""
import fcntl, os, pty, select, signal, struct, sys, tempfile, termios, time

binary = os.path.abspath(sys.argv[1])
home = tempfile.mkdtemp()
state = tempfile.mkdtemp()
os.chdir(tempfile.mkdtemp())
pid, fd = pty.fork()
if pid == 0:
    os.environ.clear()
    os.environ.update(TERM="xterm-256color", HOME=home, XDG_STATE_HOME=state,
                      PATH="/usr/bin:/bin", LANG="C.UTF-8")
    os.execv(binary, [binary])
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))

def read_for(seconds):
    out, end = b"", time.time() + seconds
    while time.time() < end:
        r, _, _ = select.select([fd], [], [], 0.2)
        if r:
            try:
                out += os.read(fd, 65536)
            except OSError:
                break
    return out

out = read_for(2)
for key in (b" ", b"T", b"\x1b[B"):
    os.write(fd, key)
    out += read_for(1)
os.write(fd, b"Q")
out += read_for(1)
os.kill(pid, signal.SIGKILL)
text = out.decode("utf-8", "replace")
print(f"{len(out)} bytes of output")
if "Press Space" not in text or "Life:" not in text:
    print(text[-2000:])
    sys.exit("smoke test failed: the board was not drawn")
saved = os.path.join(state, "fear-of-view", "gamestate")
if not os.path.exists(saved):
    sys.exit(f"smoke test failed: no game state at {saved}")
print("ok: the game drew its board and saved in the state folder")
