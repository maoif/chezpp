"""Loopback SSH CLI integration; run --password with optional Paramiko installed.

The default run needs sshd, ssh-keygen, ssh-agent, ssh-add and a built chez++.
All host keys, authorized keys and known-hosts files belong to a temporary directory.
"""

import sys, os
sys.path = [path for path in sys.path if path != os.path.dirname(__file__)]
import contextlib, pathlib, pwd, select, shutil, socket, subprocess, tempfile, time


def free_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


@contextlib.contextmanager
def fixture():
    with tempfile.TemporaryDirectory(prefix="chezpp-ssh-examples-") as d:
        root = pathlib.Path(d)
        home = root / "home"
        ssh = home / ".ssh"
        ssh.mkdir(parents=True)
        key = ssh / "id_ed25519"
        host = root / "host"
        port = free_port()
        user = pwd.getpwuid(os.getuid()).pw_name
        for path in (key, host):
            subprocess.run(
                ["ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-f", str(path)],
                check=True,
                timeout=20,
            )
        (ssh / "known_hosts").write_text(
            f"[127.0.0.1]:{port} " + host.with_suffix(".pub").read_text()
        )
        config = root / "sshd_config"
        config.write_text(
            f"Port {port}\nListenAddress 127.0.0.1\nHostKey {host}\nPidFile {root}/sshd.pid\nAuthorizedKeysFile {key}.pub\nPasswordAuthentication no\nKbdInteractiveAuthentication no\nUsePAM no\nStrictModes no\nAllowUsers {user}\nSubsystem sftp internal-sftp\nLogLevel ERROR\n"
        )
        server = subprocess.Popen(
            [shutil.which("sshd") or "/usr/sbin/sshd", "-D", "-e", "-f", str(config)],
            stderr=subprocess.PIPE,
        )
        try:
            for _ in range(100):
                try:
                    with socket.create_connection(("127.0.0.1", port), 0.1):
                        break
                except OSError:
                    time.sleep(0.03)
            else:
                raise RuntimeError(
                    f"local sshd did not become ready (status {server.poll()})"
                )
            env = os.environ.copy()
            env["HOME"] = str(home)
            args = [
                f"--host=127.0.0.1",
                f"--port={port}",
                f"--user={user}",
                "--auth=publickey",
            ]
            yield root, env, args
        finally:
            server.terminate()
            server.wait(timeout=5)


def run(script, env, args, *extra, input=None):
    return subprocess.run(
        ["./chez++", "--script", "examples/net/" + script + ".ss", *args, *extra],
        env=env,
        input=input,
        capture_output=True,
        timeout=20,
    )


def wait_for_log(process, fragment, timeout=10):
    """Wait for a readiness log without connecting to and consuming a relay channel."""
    deadline = time.monotonic() + timeout
    captured = bytearray()
    while fragment not in captured:
        remaining = deadline - time.monotonic()
        assert remaining > 0, f"readiness timeout: {bytes(captured)!r}"
        ready, _, _ = select.select([process.stderr], [], [], remaining)
        assert ready, f"readiness timeout: {bytes(captured)!r}"
        data = os.read(process.stderr.fileno(), 4096)
        assert data, f"process exited before readiness: {bytes(captured)!r}"
        captured.extend(data)


def verify():
    with fixture() as (root, env, args):
        p = run(
            "ssh-run-cmd",
            env,
            args,
            "--command=printf stdout; printf stderr >&2; exit 7",
        )
        assert p.returncode == 7 and p.stdout == b"stdout" and b"stderr" in p.stderr, p
        p = run("ssh-run-cmd", env, args, "--command=cat; printf eof-observed")
        assert p.returncode == 0 and p.stdout == b"eof-observed", p
        # Both output streams exceed typical SSH windows; drain them concurrently.
        p = run(
            "ssh-run-cmd",
            env,
            args,
            '--command=python3 -c "import os; os.write(1,bytes([97])*262144); os.write(2,bytes([98])*262144)"',
        )
        assert (
            p.returncode == 0
            and p.stdout == b"a" * 262144
            and b"b" * 262144 in p.stderr
        ), p.stderr
        p = run("ssh-run-cmd", env, args, "--command=printf", "--", "positional")
        assert p.returncode == 0 and p.stdout == b"positional", p
        keyargs = [x for x in args if x != "--auth=publickey"] + [
            "--auth=publickey",
            f"--private-key={root}/home/.ssh/id_ed25519",
        ]
        p = run("ssh-run-cmd", env, keyargs, "--command=printf explicit-key")
        assert p.returncode == 0 and p.stdout == b"explicit-key", p
        with tempfile.TemporaryDirectory(prefix="chezpp-agent-") as agentdir:
            sock = str(pathlib.Path(agentdir) / "agent.sock")
            agent = subprocess.Popen(
                ["ssh-agent", "-D", "-a", sock],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            try:
                for _ in range(100):
                    if pathlib.Path(sock).exists():
                        break
                    time.sleep(0.01)
                agentenv = env.copy()
                agentenv["SSH_AUTH_SOCK"] = sock
                subprocess.run(
                    ["ssh-add", str(root / "home/.ssh/id_ed25519")],
                    env=agentenv,
                    check=True,
                    timeout=20,
                    capture_output=True,
                )
                p = run(
                    "ssh-run-cmd",
                    agentenv,
                    [x if x != "--auth=publickey" else "--auth=agent" for x in args],
                    "--command=printf agent",
                )
                assert p.returncode == 0 and p.stdout == b"agent", p
            finally:
                agent.terminate()
                agent.wait(timeout=5)
        p = run("ssh-open-repl", env, args, "--pty=no", input=b"printf shell-eof\n")
        assert p.returncode == 0 and b"shell-eof" in p.stdout, p
        source = root / "source"
        source.write_bytes(bytes(range(256)) * 1024)
        remote = root / "remote"
        destination = root / "destination"
        for direction in ("upload", "download"):
            p = run(
                "scp",
                env,
                keyargs,
                f"--direction={direction}",
                "--kind=file",
                f'--local={source if direction=="upload" else destination}',
                f"--remote={remote}",
            )
            assert p.returncode == 0, p.stderr
            assert b"100%" in p.stderr and b"262144/262144 bytes" in p.stderr, p.stderr
            assert not p.stdout, p.stdout
        assert source.read_bytes() == destination.read_bytes()
        # Error case: existing destinations must not be overwritten by default.
        p = run(
            "scp",
            env,
            args,
            "--direction=upload",
            "--kind=file",
            f"--local={source}",
            f"--remote={remote}",
        )
        assert p.returncode != 0, p
        # Error case: invalid progress values fail before connecting.
        p = run("scp", env, args, "--direction=upload", "--kind=file",
                f"--local={source}", f"--remote={remote}", "--progress=maybe")
        assert p.returncode != 0 and b"--progress" in p.stderr, p
        source.write_bytes(b"replaced")
        p = run(
            "scp",
            env,
            args,
            "--direction=upload",
            "--kind=file",
            f"--local={source}",
            f"--remote={remote}",
            "--overwrite=skip",
        )
        assert p.returncode == 0 and remote.read_bytes() != b"replaced", p
        assert b"bytes" not in p.stderr, p.stderr
        p = run(
            "scp",
            env,
            args,
            "--direction=upload",
            "--kind=file",
            f"--local={source}",
            f"--remote={remote}",
            "--overwrite=replace",
            "--progress=no",
        )
        assert p.returncode == 0 and remote.read_bytes() == b"replaced", p
        assert b"bytes" not in p.stderr and b"100%" not in p.stderr, p.stderr
        tree = root / "tree"
        (tree / "nested").mkdir(parents=True)
        (tree / "nested/data").write_bytes(b"tree data")
        (tree / "empty").mkdir()
        (tree / "zero").touch()
        remote_tree = root / "remote-tree"
        copy = root / "copy"
        for direction in ("upload", "download"):
            p = run(
                "scp",
                env,
                args,
                f"--direction={direction}",
                "--kind=directory",
                f'--local={tree if direction=="upload" else copy}',
                f"--remote={remote_tree}",
            )
            assert p.returncode == 0, p.stderr
            assert b"9/9 bytes" in p.stderr and b"0/0 bytes" in p.stderr, p.stderr
        assert (copy / "nested/data").read_bytes() == b"tree data" and (
            copy / "empty"
        ).is_dir()
        (tree / "link").symlink_to("nested/data")
        # Error case: symlinks inside directory trees are rejected.
        p = run(
            "scp",
            env,
            args,
            "--direction=upload",
            "--kind=directory",
            f"--local={tree}",
            f"--remote={remote_tree}",
            "--overwrite=replace",
        )
        assert p.returncode != 0 and b"symbolic links" in p.stderr, p
        remote_link = root / "remote-link"
        remote_link.symlink_to("remote")
        # Error case: an existing remote source symlink must not be followed by file SCP.
        p = run(
            "scp",
            env,
            args,
            "--direction=download",
            "--kind=file",
            f"--local={root / 'link-output'}",
            f"--remote={remote_link}",
        )
        assert p.returncode != 0 and b"symlinks are rejected" in p.stderr, p
        verify_scp_path_links(root, env, args)
        verify_scp_interruption(root, env, args)
        verify_forwarding(env, args)
        verify_forwarding_recovery(env, args)
        verify_forwarding_cancellation(env, args)
        # Explicit encrypted private keys use a separate passphrase environment variable.
        key = root / "home/.ssh/id_ed25519"
        subprocess.run(
            [
                "ssh-keygen",
                "-q",
                "-p",
                "-P",
                "",
                "-N",
                "fixture-passphrase",
                "-f",
                str(key),
            ],
            check=True,
            timeout=20,
            capture_output=True,
        )
        keyenv = env.copy()
        keyenv["CHEZPP_SSH_PASSPHRASE"] = "fixture-passphrase"
        p = run("ssh-run-cmd", keyenv, keyargs, "--command=printf encrypted-key")
        assert p.returncode == 0 and p.stdout == b"encrypted-key", p
        for script in ("ssh-run-cmd", "ssh-open-repl", "ssh-port-forward", "scp"):
            for flag in ("-h", "--help"):
                p = run(script, env, [], flag)
                assert p.returncode == 0 and p.stdout, p
            # Error case: unrecognized options must fail before opening network resources.
            p = run(script, env, [], "--unknown-option=x")
            assert p.returncode != 0, p
        # Error case: SCP restart is rejected instead of silently starting over.
        p = run(
            "scp",
            env,
            args,
            "--direction=upload",
            "--kind=file",
            f"--local={source}",
            f"--remote={remote}",
            "--resume=resume",
        )
        assert p.returncode != 0 and b"unsupported" in p.stderr, p
        # Error case: untrusted host keys must fail before authentication.
        badenv = env.copy()
        badenv["HOME"] = str(root / "untrusted")
        pathlib.Path(badenv["HOME"]).mkdir()
        p = run("ssh-run-cmd", badenv, args, "--command=true")
        assert p.returncode != 0 and b"host key is not trusted" in p.stderr, p


def verify_scp_path_links(root, env, args):
    for direction in ("upload", "download"):
        for kind in ("file", "directory"):
            for scenario in ("source-parent", "source-dot-parent", "target-parent",
                             "target-missing-parent", "target-root", "target-child",
                             "target-dangling-child", "target-directory"):
                if kind == "file" and scenario in ("target-child", "target-dangling-child", "target-directory"):
                    continue
                case = root / f"links-{direction}-{kind}-{scenario}"
                source, target, outside = case / "source", case / "target", case / "outside"
                (source / "nested").mkdir(parents=True)
                (source / "nested/data").write_bytes(b"new content")
                outside.mkdir()
                (outside / "data").write_bytes(b"unrelated content")
                source_path = source / "nested/data" if kind == "file" else source
                target_path = target
                if scenario == "source-parent":
                    (case / "source-link").symlink_to(source, target_is_directory=True)
                    source_path = case / "source-link" / ("nested/data" if kind == "file" else "nested")
                elif scenario == "source-dot-parent":
                    (case / "source-link").symlink_to(source / "nested", target_is_directory=True)
                    source_path = case / "source-link/.." / ("nested/data" if kind == "file" else "nested")
                elif scenario in ("target-parent", "target-missing-parent"):
                    target.symlink_to(outside, target_is_directory=True)
                    target_path = target / ("data" if kind == "file" else "new-tree")
                    if scenario == "target-missing-parent":
                        target_path = case / "missing/../target" / target_path.name
                elif scenario == "target-root":
                    target.symlink_to(outside / "data" if kind == "file" else outside,
                                      target_is_directory=(kind == "directory"))
                elif scenario in ("target-child", "target-dangling-child"):
                    (target / "nested").mkdir(parents=True)
                    (target / "nested/data").symlink_to(
                        outside / ("data" if scenario == "target-child" else "missing"))
                else:
                    target.mkdir()
                    (target / "nested").symlink_to(outside, target_is_directory=True)
                local, remote = ((source_path, target_path) if direction == "upload"
                                 else (target_path, source_path))
                # Error case: symlinked ancestors and destination entries must never be followed.
                p = run("scp", env, args, f"--direction={direction}", f"--kind={kind}",
                        f"--local={local}", f"--remote={remote}", "--overwrite=replace")
                assert p.returncode != 0, (direction, kind, scenario, p)
                assert (outside / "data").read_bytes() == b"unrelated content", scenario
                assert not (outside / "new-tree").exists(), scenario
                assert not (outside / "missing").exists(), scenario


def verify_forwarding_recovery(env, args):
    import threading

    for direction in ("local", "remote"):
        with socket.socket() as target:
            target.bind(("127.0.0.1", 0))
            port = free_port()
            process = subprocess.Popen(
                ["./chez++", "--script", "examples/net/ssh-port-forward.ss", *args,
                 f"--direction={direction}", f"--listen-port={port}",
                 f"--target-port={target.getsockname()[1]}", "--duration=4000"],
                env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            try:
                wait_for_log(process, b"forwarding listening")
                # Error case: one refused target connection must leave the listener available.
                with socket.create_connection(("127.0.0.1", port), 5) as client:
                    client.settimeout(3)
                    try:
                        assert client.recv(1) == b""
                    except ConnectionResetError:
                        pass
                assert process.poll() is None, process.communicate(timeout=2)
                target.listen()
                target.settimeout(5)

                def echo():
                    conn, _ = target.accept()
                    with conn:
                        conn.sendall(conn.recv(32))

                thread = threading.Thread(target=echo, daemon=True)
                thread.start()
                with socket.create_connection(("127.0.0.1", port), 5) as client:
                    client.settimeout(5)
                    client.sendall(b"recovered")
                    assert client.recv(32) == b"recovered", direction
                thread.join(5)
                assert not thread.is_alive(), direction
                _, errors = process.communicate(timeout=8)
                assert process.returncode == 0, errors
            finally:
                if process.poll() is None:
                    process.kill()
                    process.wait(timeout=5)


def verify_scp_interruption(root, env, args):
    import signal

    source = root / "large-remote"
    destination = root / "interrupted-download"
    with source.open("wb") as output:
        output.truncate(256 * 1024 * 1024)
    process = subprocess.Popen(
        [
            "./chez++",
            "--script",
            "examples/net/scp.ss",
            *args,
            "--direction=download",
            "--kind=file",
            f"--local={destination}",
            f"--remote={source}",
        ],
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    try:
        deadline = time.monotonic() + 10
        while time.monotonic() < deadline:
            if destination.exists() and destination.stat().st_size > 0:
                break
            assert process.poll() is None, process.communicate(timeout=2)
            time.sleep(0.002)
        else:
            raise AssertionError("SCP transfer never created its partial output")
        process.send_signal(signal.SIGINT)
        _, errors = process.communicate(timeout=10)
        assert process.returncode != 0 and b"closing SSH session" in errors, errors
        assert (
            not destination.exists()
        ), "interrupted SCP download leaked a partial output"
    finally:
        if process.poll() is None:
            process.kill()
            process.wait(timeout=5)
        source.unlink()


def verify_forwarding(env, args):
    import threading

    for direction in ("local", "remote"):
        target = socket.socket()
        target.bind(("127.0.0.1", 0))
        target.listen()

        def echo():
            conn, _ = target.accept()
            with conn:
                # Reply only after stdin EOF to verify end-to-end half-close.
                chunks = []
                while True:
                    chunk = conn.recv(32768)
                    if not chunk:
                        break
                    chunks.append(chunk)
                conn.sendall(b"".join(chunks) + b":eof")

        thread = threading.Thread(target=echo, daemon=True)
        thread.start()
        port = free_port()
        process = subprocess.Popen(
            [
                "./chez++",
                "--script",
                "examples/net/ssh-port-forward.ss",
                *args,
                f"--direction={direction}",
                f"--listen-port={port}",
                f"--target-port={target.getsockname()[1]}",
                "--duration=2500",
            ],
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        try:
            wait_for_log(process, b"forwarding listening")
            payload = bytes(range(256)) * 8192
            with socket.create_connection(("127.0.0.1", port), 5) as conn:
                conn.settimeout(10)
                conn.sendall(payload)
                conn.shutdown(socket.SHUT_WR)
                chunks = []
                while True:
                    chunk = conn.recv(65536)
                    if not chunk:
                        break
                    chunks.append(chunk)
                assert b"".join(chunks) == payload + b":eof", direction
            thread.join(5)
            assert not thread.is_alive()
            _, err = process.communicate(timeout=10)
            assert process.returncode == 0, err
            # Bounded shutdown releases the listening port.
            with socket.socket() as probe:
                probe.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                probe.bind(("127.0.0.1", port))
        finally:
            target.close()
            if process.poll() is None:
                process.kill()
                process.wait(timeout=5)


def verify_forwarding_cancellation(env, args):
    import signal

    for direction in ("local", "remote"):
        target = socket.socket()
        target.bind(("127.0.0.1", 0))
        target.listen()
        port = free_port()
        process = subprocess.Popen(
            [
                "./chez++",
                "--script",
                "examples/net/ssh-port-forward.ss",
                *args,
                f"--direction={direction}",
                f"--listen-port={port}",
                f"--target-port={target.getsockname()[1]}",
            ],
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        conn = None
        accepted = None
        try:
            wait_for_log(process, b"forwarding listening")
            conn = socket.create_connection(("127.0.0.1", port), 5)
            conn.sendall(b"active traffic")
            target.settimeout(5)
            accepted, _ = target.accept()
            accepted.settimeout(5)
            assert accepted.recv(100) == b"active traffic"
            # Ctrl-C must unwind an active relay and cancel both listener directions.
            process.send_signal(signal.SIGINT)
            _, err = process.communicate(timeout=10)
            assert process.returncode == 0 and b"forwarding stopped" in err, err
            assert accepted.recv(100) == b""
            with socket.socket() as probe:
                probe.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                probe.bind(("127.0.0.1", port))
        finally:
            if conn:
                conn.close()
            if accepted:
                accepted.close()
            target.close()
            if process.poll() is None:
                process.kill()
                process.wait(timeout=5)


def verify_password():
    """Optional isolated protocol server; never changes a system account password."""
    import paramiko, threading

    class Server(paramiko.ServerInterface):
        def __init__(self):
            self.command = threading.Event()

        def get_allowed_auths(self, user):
            return "password"

        def check_auth_password(self, user, password):
            return (
                paramiko.AUTH_SUCCESSFUL
                if user == "fixture" and password == "fixture-secret"
                else paramiko.AUTH_FAILED
            )

        def check_channel_request(self, kind, channel):
            return (
                paramiko.OPEN_SUCCEEDED
                if kind == "session"
                else paramiko.OPEN_FAILED_ADMINISTRATIVELY_PROHIBITED
            )

        def check_channel_exec_request(self, channel, command):
            self.command.set()
            return True

    with tempfile.TemporaryDirectory(prefix="chezpp-password-") as directory:
        home = pathlib.Path(directory)
        (home / ".ssh").mkdir()
        key = paramiko.RSAKey.generate(2048)
        listener = socket.socket()
        listener.bind(("127.0.0.1", 0))
        listener.listen()
        port = listener.getsockname()[1]
        (home / ".ssh/known_hosts").write_text(
            f"[127.0.0.1]:{port} {key.get_name()} {key.get_base64()}\n"
        )

        def serve():
            conn, _ = listener.accept()
            with conn:
                transport = paramiko.Transport(conn)
                try:
                    transport.add_server_key(key)
                    server = Server()
                    transport.start_server(server=server)
                    channel = transport.accept(10)
                    assert channel and server.command.wait(10)
                    channel.sendall(b"password-auth")
                    channel.send_stderr(b"password-stderr")
                    channel.send_exit_status(9)
                    channel.shutdown_write()
                    channel.close()
                    time.sleep(0.1)
                finally:
                    transport.close()

        thread = threading.Thread(target=serve, daemon=True)
        thread.start()
        try:
            env = os.environ.copy()
            env["HOME"] = str(home)
            env["CHEZPP_SSH_PASSWORD"] = "fixture-secret"
            p = run(
                "ssh-run-cmd",
                env,
                [
                    "--host=127.0.0.1",
                    f"--port={port}",
                    "--user=fixture",
                    "--auth=password",
                ],
                "--command=fixture",
            )
            assert (
                p.returncode == 9
                and p.stdout == b"password-auth"
                and b"password-stderr" in p.stderr
            ), p
            thread.join(10)
            assert not thread.is_alive()
        finally:
            listener.close()


if __name__ == "__main__":
    import sys

    os.chdir(pathlib.Path(__file__).resolve().parent.parent)
    if "--password" in sys.argv:
        verify_password()
    else:
        verify()
