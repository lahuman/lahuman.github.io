#!/usr/bin/env bash
set -Eeuo pipefail
# Read-only inventory. Specify the real service binary, not necessarily PATH's nginx.
BINARY="$(command -v nginx || true)"
if [[ $# -gt 0 ]]; then BINARY="$1"; fi
[[ -n "$BINARY" && -x "$BINARY" ]] || { echo "Usage: $0 /absolute/path/to/nginx" >&2; exit 1; }
echo "[nginx binary] $BINARY"
"$BINARY" -V 2>&1
echo "[processes]"
ps -eo pid,ppid,user,args | grep '[n]ginx' || true
echo "[systemd units]"
if command -v systemctl >/dev/null 2>&1; then systemctl list-units --all --type=service '*nginx*' --no-pager || true; fi
echo "Confirm /proc/<master-pid>/exe and systemd ExecStart to find the actually running binary."
