"""Real local/remote PTY checks for ssh-open-repl.ss; requires a built cspp."""
import sys, os
sys.path = [p for p in sys.path if p != os.path.dirname(__file__)]
import fcntl, pathlib, pty, runpy, select, signal, struct, subprocess, termios, time

ROOT = pathlib.Path(__file__).resolve().parent.parent
api = runpy.run_path(str(ROOT / 'tests/ssh-examples-integration.py'), run_name='ssh_fixture')


def verify_shell():
    with api['fixture'](shell_command="env 'PS1=CSPP-PROMPT>' /bin/bash --noprofile --norc -i") as (_, env, args):
        env['TERM'] = 'xterm-256color'
        master, slave = pty.openpty()
        original = termios.tcgetattr(slave)
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 37, 119, 0, 0))
        def controlling_terminal():
            os.setsid()
            fcntl.ioctl(0, termios.TIOCSCTTY, 0)
        process = subprocess.Popen(
            ['./bin/cspp', '--script=examples/net/ssh-open-repl.ss', '--', *args], env=env,
            stdin=slave, stdout=slave, stderr=slave, preexec_fn=controlling_terminal)
        transcript = bytearray()
        def until(predicate, description, timeout=8):
            deadline = time.monotonic() + timeout
            received = bytearray()
            while time.monotonic() < deadline:
                if predicate(bytes(received)):
                    return bytes(received)
                ready, _, _ = select.select([master], [], [], 0.05)
                if ready:
                    try:
                        data = os.read(master, 65536)
                    except OSError:
                        break
                    received.extend(data)
                    transcript.extend(data)
                assert process.poll() is None, (description, process.returncode, bytes(transcript))
            raise AssertionError((description, bytes(received), bytes(transcript)))
        try:
            until(lambda data: b'CSPP-PROMPT>' in data, 'remote prompt')
            flags = termios.tcgetattr(slave)[3]
            assert not flags & (termios.ICANON | termios.ECHO | termios.ISIG), (
                'local terminal must relay input immediately without echo or signals', flags)
            # The remote editor receives each key immediately and echoes it exactly once.
            os.write(master, b'printf "shell-%s\\n" OK')
            until(lambda data: b'printf' in data, 'key echo before Enter')
            os.write(master, b'\r')
            data = until(lambda data: b'CSPP-PROMPT>' in data, 'command completion')
            assert b'shell-OK' in data and b'\r\r\n' not in transcript, bytes(transcript)
            os.write(master, b'printf "term=%s\\n" "$TERM"; stty size\r')
            data = until(lambda data: b'CSPP-PROMPT>' in data, 'terminal type and size')
            assert b'term=xterm-256color' in data and b'37 119' in data, data
            # Backspace and cursor movement belong to the remote line editor.
            os.write(master, b'printf "edit-%s\\n" OKX\x7f\x1b[D\x1b[C\r')
            data = until(lambda data: b'CSPP-PROMPT>' in data, 'remote line editing')
            assert b'edit-OK\r\n' in data, data
            fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 43, 101, 0, 0))
            time.sleep(0.3)
            os.write(master, b'stty size\r')
            data = until(lambda data: b'43 101' in data and b'CSPP-PROMPT>' in data,
                         'terminal resize')
            assert b'43 101' in data, data
            # Error case: Ctrl-C interrupts the remote foreground command, keeping SSH open.
            os.write(master, b'sleep 30\r')
            until(lambda data: b'sleep 30' in data, 'remote sleep started')
            time.sleep(0.1)
            os.write(master, b'\x03')
            until(lambda data: b'CSPP-PROMPT>' in data, 'prompt after remote Ctrl-C')
            os.write(master, b'exit 7\r')
            assert process.wait(timeout=8) == 7
            assert termios.tcgetattr(slave) == original, 'terminal not restored after exit'
            for ending in ('eof', 'interrupt', 'terminate', 'hangup', 'quit'):
                process = subprocess.Popen(
                    ['./bin/cspp', '--script=examples/net/ssh-open-repl.ss', '--', *args], env=env,
                    stdin=slave, stdout=slave, stderr=slave, preexec_fn=controlling_terminal)
                until(lambda data: b'CSPP-PROMPT>' in data, 'prompt before ' + ending)
                if ending == 'eof':
                    os.write(master, b'\x04')
                else:
                    # Error case: a local process interrupt must unwind terminal cleanup.
                    process.send_signal({'interrupt': signal.SIGINT, 'terminate': signal.SIGTERM,
                                         'hangup': signal.SIGHUP, 'quit': signal.SIGQUIT}[ending])
                status = process.wait(timeout=8)
                assert (status == 0) == (ending == 'eof'), (ending, status)
                assert termios.tcgetattr(slave) == original, ('terminal not restored', ending)
        finally:
            if process.poll() is None:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait(timeout=5)
            termios.tcsetattr(slave, termios.TCSANOW, original)
            os.close(master)
            os.close(slave)
    # Default piped input must not allocate a PTY or emit an interactive prompt.
    with api['fixture']() as (_, env, args):
        result = api['run']('ssh-open-repl', env, args, input=b'printf pipe-ok\n')
        assert result.returncode == 0 and result.stdout == b'pipe-ok', result


def verify_bidirectional_backpressure():
    # A full remote stdout window must not prevent forwarding a larger local stdin.
    import tempfile
    with tempfile.TemporaryDirectory(prefix='cspp-shell-backpressure-') as directory:
        root = pathlib.Path(directory)
        worker = root / 'worker.py'
        worker.write_text('import sys\n'
                          'sys.stdout.buffer.write(b"x" * (8 * 1024 * 1024))\n'
                          'sys.stdout.buffer.flush()\n'
                          'data = sys.stdin.buffer.read()\n'
                          'assert data == b"y" * (16 * 1024 * 1024)\n'
                          'sys.stderr.write("input-complete\\n")\n')
        source = root / 'input'
        source.write_bytes(b'y' * (16 * 1024 * 1024))
        with api['fixture'](shell_command=f'python3 {worker}') as (_, env, args):
            with source.open('rb') as input_port, tempfile.TemporaryFile() as output:
                process = subprocess.Popen(
                    ['./bin/cspp', '--script=examples/net/ssh-open-repl.ss', '--', *args, '--pty=no'],
                    env=env, stdin=input_port, stdout=output, stderr=subprocess.PIPE,
                    start_new_session=True)
                try:
                    _, errors = process.communicate(timeout=15)
                    assert process.returncode == 0 and b'input-complete' in errors, errors
                    output.seek(0)
                    assert output.read() == b'x' * (8 * 1024 * 1024)
                finally:
                    if process.poll() is None:
                        os.killpg(process.pid, signal.SIGKILL)
                        process.communicate()


if __name__ == '__main__':
    os.chdir(ROOT)
    verify_shell()
    verify_bidirectional_backpressure()
