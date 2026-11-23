#!/usr/bin/env bash
set -Eeuo pipefail

# Run on a connected Linux/macOS workstation. Produces a source bundle.
NGINX_VERSION="${NGINX_VERSION:-1.31.9}"
PCRE2_VERSION="${PCRE2_VERSION:-10.49}"
OPENSSL_VERSION="${OPENSSL_VERSION:-3.5.9}"
ZLIB_VERSION="${ZLIB_VERSION:-1.4}"
OUTPUT_DIR="${OUTPUT_DIR:-$PWD/nginx-offline-bundle}"

for tool in curl tar sha256sum; do
  command -v "$tool" >/dev/null 2>&1 || { echo "Missing command: $tool" >&2; exit 1; }
done
mkdir -p "$OUTPUT_DIR/sources"
if [[ -e "$OUTPUT_DIR/install.sh" ]]; then
  echo "Refusing to overwrite existing bundle: $OUTPUT_DIR/install.sh" >&2
  exit 1
fi
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cp "$SCRIPT_DIR/install.sh" "$OUTPUT_DIR/install.sh"
cp "$SCRIPT_DIR/audit.sh" "$OUTPUT_DIR/audit.sh"
chmod 755 "$OUTPUT_DIR/audit.sh"
chmod 755 "$OUTPUT_DIR/install.sh"

download() {
  local url="$1" name="$2"
  if [[ -e "$OUTPUT_DIR/sources/$name" ]]; then
    echo "Already exists (not overwritten): $name"
    return
  fi
  echo "Downloading $name"
  curl --fail --location --retry 3 --proto '=https' --tlsv1.2 --output "$OUTPUT_DIR/sources/$name.part" "$url"
  mv "$OUTPUT_DIR/sources/$name.part" "$OUTPUT_DIR/sources/$name"
}
download "https://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz" "nginx-${NGINX_VERSION}.tar.gz"
download "https://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz.asc" "nginx-${NGINX_VERSION}.tar.gz.asc"
download "https://github.com/PCRE2Project/pcre2/releases/download/pcre2-${PCRE2_VERSION}/pcre2-${PCRE2_VERSION}.tar.gz" "pcre2-${PCRE2_VERSION}.tar.gz"
download "https://www.openssl.org/source/openssl-${OPENSSL_VERSION}.tar.gz" "openssl-${OPENSSL_VERSION}.tar.gz"
download "https://zlib.net/zlib-${ZLIB_VERSION}.tar.gz" "zlib-${ZLIB_VERSION}.tar.gz"
(
  cd "$OUTPUT_DIR/sources"
  sha256sum ./*.tar.gz > SHA256SUMS
)
printf 'NGINX_VERSION=%q\nPCRE2_VERSION=%q\nOPENSSL_VERSION=%q\nZLIB_VERSION=%q\n' \
  "$NGINX_VERSION" "$PCRE2_VERSION" "$OPENSSL_VERSION" "$ZLIB_VERSION" > "$OUTPUT_DIR/versions.env"
echo "Created: $OUTPUT_DIR"
echo "Nginx detached signature included. Verify using independently trusted official signing key."
echo "Bundle checksums verify transfer integrity, not supplier authenticity."
echo "Transfer the directory and run ./install.sh inside it."
