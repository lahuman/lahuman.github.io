#!/usr/bin/env bash
set -Eeuo pipefail

BUNDLE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=/dev/null
source "$BUNDLE_DIR/versions.env"
NGINX_PREFIX="${NGINX_PREFIX:-/opt/nginx-offline}"
NGINX_USER="${NGINX_USER:-nginx}"
NGINX_GROUP="${NGINX_GROUP:-nginx}"
JOBS="${JOBS:-2}"
BUILD_ROOT="${BUILD_ROOT:-$BUNDLE_DIR/build}"

[[ "$NGINX_PREFIX" = /* && "$NGINX_PREFIX" != / ]] || { echo "NGINX_PREFIX must be an absolute non-root path" >&2; exit 1; }
[[ "$BUILD_ROOT" = /* && "$BUILD_ROOT" != / ]] || { echo "BUILD_ROOT must be an absolute non-root path" >&2; exit 1; }
[[ "$JOBS" =~ ^[1-9][0-9]*$ ]] || { echo "JOBS must be a positive integer" >&2; exit 1; }
[[ "$(uname -s)" = Linux ]] || { echo "This install script targets Linux only" >&2; exit 1; }
for tool in gcc make perl tar sha256sum getent id; do
  command -v "$tool" >/dev/null 2>&1 || { echo "Missing build prerequisite: $tool" >&2; exit 1; }
done
id "$NGINX_USER" >/dev/null 2>&1 || { echo "Create the worker account first: $NGINX_USER" >&2; exit 1; }
getent group "$NGINX_GROUP" >/dev/null || { echo "Create the worker group first: $NGINX_GROUP" >&2; exit 1; }
[[ ! -e "$NGINX_PREFIX/sbin/nginx" ]] || { echo "Refusing to overwrite: $NGINX_PREFIX/sbin/nginx" >&2; exit 1; }

cd "$BUNDLE_DIR/sources"
sha256sum -c SHA256SUMS
if [[ -n "${NGINX_TRUSTED_KEYRING:-}" ]]; then
  command -v gpgv >/dev/null || { echo "gpgv required" >&2; exit 1; }
  gpgv --keyring "$NGINX_TRUSTED_KEYRING" "nginx-$NGINX_VERSION.tar.gz.asc" "nginx-$NGINX_VERSION.tar.gz"
else
  echo "WARNING: verify Nginx PGP signature with trusted keys before production use" >&2
fi
for name in "nginx-$NGINX_VERSION" "pcre2-$PCRE2_VERSION" "openssl-$OPENSSL_VERSION" "zlib-$ZLIB_VERSION"; do
  [[ -f "$name.tar.gz" ]] || { echo "Source missing: $name.tar.gz" >&2; exit 1; }
done
[[ ! -e "$BUILD_ROOT" ]] || { echo "BUILD_ROOT already exists: $BUILD_ROOT" >&2; exit 1; }
mkdir -p "$BUILD_ROOT"
for name in "nginx-$NGINX_VERSION" "pcre2-$PCRE2_VERSION" "openssl-$OPENSSL_VERSION" "zlib-$ZLIB_VERSION"; do
  # Each trusted source archive should extract into its single named directory.
  tar -xzf "$name.tar.gz" -C "$BUILD_ROOT" --no-same-owner
  [[ -d "$BUILD_ROOT/$name" ]] || { echo "Unexpected archive layout: $name" >&2; exit 1; }
done
cd "$BUILD_ROOT/nginx-$NGINX_VERSION"
./configure \
  --prefix="$NGINX_PREFIX" \
  --user="$NGINX_USER" \
  --group="$NGINX_GROUP" \
  --with-pcre="$BUILD_ROOT/pcre2-$PCRE2_VERSION" \
  --with-pcre-jit \
  --with-zlib="$BUILD_ROOT/zlib-$ZLIB_VERSION" \
  --with-openssl="$BUILD_ROOT/openssl-$OPENSSL_VERSION" \
  --with-http_ssl_module \
  --with-http_v2_module \
  --with-http_sub_module
make -j "$JOBS"
make install
"$NGINX_PREFIX/sbin/nginx" -V
"$NGINX_PREFIX/sbin/nginx" -t
echo "Installed nginx to $NGINX_PREFIX"
echo "Not started; no running service or systemd unit was changed."
echo "Staged install only: operating service remains unpatched until properly transitioned."
echo "Build sources retained in $BUILD_ROOT. Review the config before starting."
