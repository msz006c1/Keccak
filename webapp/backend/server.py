#!/usr/bin/env python3
"""Python HTTP backend for Keccak/SHA3 webapp.

Uses only stdlib modules. Listens on 127.0.0.1:8080.
Dispatches hash computations to CLI tools or Erlang via subprocess.
"""

import json
import os
import subprocess
import time
from http.server import HTTPServer, BaseHTTPRequestHandler

PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
BUILD_DIR = os.path.join(PROJECT_ROOT, "build")

VALID_ALGORITHMS = {"sha3-256", "sha3-512", "sha3-1024"}
VALID_IMPLEMENTATIONS = {"c", "cpp", "erlang-nif", "erlang-pure"}
VALID_PLATFORMS = {"native", "linux-arm64", "android-arm64"}


def check_binary(path):
    """Check if a binary exists and is executable."""
    return os.path.isfile(path) and os.access(path, os.X_OK)


def check_qemu():
    """Check if qemu-aarch64-static is available."""
    try:
        subprocess.run(["qemu-aarch64-static", "--version"],
                       capture_output=True, timeout=5)
        return True
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return False


def check_erlang():
    """Check if erl is available."""
    try:
        subprocess.run(["erl", "-noshell", "-eval", "halt()."],
                       capture_output=True, timeout=5)
        return True
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return False


def get_available():
    """Return dict of available implementations and platforms."""
    has_qemu = check_qemu()
    has_erl = check_erlang()

    implementations = {}

    # C
    platforms = []
    if check_binary(os.path.join(BUILD_DIR, "hash_cli")):
        platforms.append("native")
    if has_qemu and check_binary(os.path.join(BUILD_DIR, "hash_cli_linux_arm64")):
        platforms.append("linux-arm64")
    if has_qemu and check_binary(os.path.join(BUILD_DIR, "hash_cli_android_arm64")):
        platforms.append("android-arm64")
    if platforms:
        implementations["c"] = platforms

    # C++
    platforms = []
    if check_binary(os.path.join(BUILD_DIR, "hash_cli_cpp")):
        platforms.append("native")
    if platforms:
        implementations["cpp"] = platforms

    # Erlang NIF
    ebin_path = os.path.join(PROJECT_ROOT, "ebin")
    priv_path = os.path.join(PROJECT_ROOT, "priv")
    if has_erl and os.path.isdir(ebin_path) and os.path.isdir(priv_path):
        implementations["erlang-nif"] = ["native"]

    # Erlang pure
    pure_ebin = os.path.join(PROJECT_ROOT, "Keccak_erl", "ebin")
    if has_erl and os.path.isdir(pure_ebin):
        implementations["erlang-pure"] = ["native"]

    return implementations


def hex_encode_for_erlang(input_str):
    """Hex-encode input bytes for safe Erlang eval embedding."""
    encoded = input_str.encode("utf-8")
    hex_list = ", ".join(f"16#{b:02x}" for b in encoded)
    return f"list_to_binary([{hex_list}])"


def build_command(algorithm, implementation, platform, input_str):
    """Build the subprocess command list for a given dispatch combo."""
    algo_arg = algorithm  # sha3-256, sha3-512, sha3-1024

    if implementation == "c":
        if platform == "native":
            return [os.path.join(BUILD_DIR, "hash_cli"), algo_arg, input_str]
        elif platform == "linux-arm64":
            return ["qemu-aarch64-static",
                    os.path.join(BUILD_DIR, "hash_cli_linux_arm64"),
                    algo_arg, input_str]
        elif platform == "android-arm64":
            return ["qemu-aarch64-static",
                    os.path.join(BUILD_DIR, "hash_cli_android_arm64"),
                    algo_arg, input_str]

    elif implementation == "cpp":
        if platform == "native":
            return [os.path.join(BUILD_DIR, "hash_cli_cpp"), algo_arg, input_str]

    elif implementation == "erlang-nif":
        if platform == "native":
            input_expr = hex_encode_for_erlang(input_str)
            # Map algo name to function
            func_map = {
                "sha3-256": "sha3_256",
                "sha3-512": "sha3_512",
                "sha3-1024": "sha3_1024",
            }
            func = func_map[algorithm]
            eval_str = (
                f"Input = {input_expr}, "
                f"Hash = keccak:{func}(Input), "
                f"Hex = lists:flatten([io_lib:format(\"~2.16.0b\", [B]) || <<B>> <= Hash]), "
                f"io:format(\"~s~n\", [Hex]), "
                f"halt()."
            )
            return ["erl", "-pa", os.path.join(PROJECT_ROOT, "ebin"),
                    "-noshell", "-eval", eval_str]

    elif implementation == "erlang-pure":
        if platform == "native":
            input_expr = hex_encode_for_erlang(input_str)
            func_map = {
                "sha3-256": "sha3_256",
                "sha3-512": "sha3_512",
                "sha3-1024": "sha3_1024",
            }
            func = func_map[algorithm]
            eval_str = (
                f"Input = {input_expr}, "
                f"Hash = keccak_real_fixed:{func}(Input), "
                f"Hex = lists:flatten([io_lib:format(\"~2.16.0b\", [B]) || <<B>> <= Hash]), "
                f"io:format(\"~s~n\", [Hex]), "
                f"halt()."
            )
            return ["erl", "-pa",
                    os.path.join(PROJECT_ROOT, "Keccak_erl", "ebin"),
                    "-noshell", "-eval", eval_str]

    return None


def run_hash(algorithm, implementation, platform, input_str):
    """Run the hash command and return (hex_hash, exec_time_ms)."""
    cmd = build_command(algorithm, implementation, platform, input_str)
    if cmd is None:
        raise ValueError(
            f"Unsupported combination: {implementation}/{platform}")

    start = time.monotonic()
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
    elapsed_ms = (time.monotonic() - start) * 1000

    if result.returncode != 0:
        stderr = result.stderr.strip()
        raise RuntimeError(
            f"Command failed (exit {result.returncode}): {stderr}")

    hex_hash = result.stdout.strip()
    return hex_hash, elapsed_ms


class HashHandler(BaseHTTPRequestHandler):
    """HTTP request handler for the hash API."""

    def _send_json(self, status, data):
        body = json.dumps(data).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        if self.path == "/api/health":
            available = get_available()
            self._send_json(200, {
                "status": "ok",
                "algorithms": sorted(VALID_ALGORITHMS),
                "implementations": available,
            })
        else:
            self._send_json(404, {"error": "Not found"})

    def do_POST(self):
        if self.path != "/api/hash":
            self._send_json(404, {"error": "Not found"})
            return

        content_length = int(self.headers.get("Content-Length", 0))
        if content_length == 0:
            self._send_json(400, {"error": "Empty request body"})
            return

        try:
            body = json.loads(self.rfile.read(content_length))
        except json.JSONDecodeError:
            self._send_json(400, {"error": "Invalid JSON"})
            return

        algorithm = body.get("algorithm", "")
        implementation = body.get("implementation", "")
        platform = body.get("platform", "")
        input_str = body.get("input", "")

        if algorithm not in VALID_ALGORITHMS:
            self._send_json(400, {
                "error": f"Invalid algorithm: {algorithm}. "
                         f"Valid: {sorted(VALID_ALGORITHMS)}"
            })
            return

        if implementation not in VALID_IMPLEMENTATIONS:
            self._send_json(400, {
                "error": f"Invalid implementation: {implementation}. "
                         f"Valid: {sorted(VALID_IMPLEMENTATIONS)}"
            })
            return

        if platform not in VALID_PLATFORMS:
            self._send_json(400, {
                "error": f"Invalid platform: {platform}. "
                         f"Valid: {sorted(VALID_PLATFORMS)}"
            })
            return

        try:
            hex_hash, exec_time_ms = run_hash(
                algorithm, implementation, platform, input_str)
            self._send_json(200, {
                "algorithm": algorithm,
                "implementation": implementation,
                "platform": platform,
                "input": input_str,
                "hash": hex_hash,
                "exec_time_ms": round(exec_time_ms, 3),
            })
        except ValueError as e:
            self._send_json(400, {"error": str(e)})
        except subprocess.TimeoutExpired:
            self._send_json(504, {"error": "Hash computation timed out"})
        except Exception as e:
            self._send_json(500, {"error": str(e)})

    def log_message(self, format, *args):
        print(f"[{self.log_date_time_string()}] {format % args}")


def main():
    host = "127.0.0.1"
    port = 8080
    server = HTTPServer((host, port), HashHandler)
    print(f"Backend listening on {host}:{port}")
    print(f"Project root: {PROJECT_ROOT}")
    print(f"Build dir: {BUILD_DIR}")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down.")
        server.server_close()


if __name__ == "__main__":
    main()
