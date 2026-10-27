#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

usage="usage: ./build.sh [--no-test]"
run_tests=true
case "${1:-}" in
    "") ;;
    --no-test) run_tests=false ;;
    *) echo "$usage"; exit 2 ;;
esac

elm make src/Main.elm --optimize --output=elm.js
$run_tests || exit 0

(cd tests && elm make Check.elm --output=check.js)

find_browser() {
    local candidates=(
        "/c/Program Files/Google/Chrome/Application/chrome.exe"
        "/c/Program Files (x86)/Google/Chrome/Application/chrome.exe"
        "${LOCALAPPDATA:-}/Google/Chrome/Application/chrome.exe"
        "/c/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
        "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
        "$(command -v google-chrome chromium chromium-browser 2>/dev/null | head -n 1)"
    )
    for candidate in "${candidates[@]}"; do
        if [ -n "$candidate" ] && [ -x "$candidate" ]; then echo "$candidate"; return; fi
    done
}

browser="$(find_browser)"
if [ -z "$browser" ]; then
    echo "No Chrome or Edge found. Open tests/index.html in a browser to see the results."
    exit 0
fi

test_page="$(pwd)/tests/index.html"
if command -v cygpath >/dev/null; then test_page="$(cygpath -m "$test_page")"; fi
browser_profile="${TMPDIR:-${TEMP:-/tmp}}/regex-tracklayer-test-browser"
rendered="$("$browser" --headless=new --disable-gpu --user-data-dir="$browser_profile" --virtual-time-budget=20000 --dump-dom "file:///${test_page#/}" 2>/dev/null)"

html_unescape() { sed -e 's/&lt;/</g; s/&gt;/>/g; s/&quot;/"/g; s/&#39;/'"'"'/g; s/&amp;/\&/g'; }
text_of() { grep -o "<$1[^>]*>[^<]*" | sed 's/<[^>]*>//'; }

printf '%s' "$rendered" | tr '\n' '\r' | text_of 'pre class="failure"' | tr '\r' '\n' | html_unescape || true
summary="$(printf '%s' "$rendered" | text_of 'h1 id="summary"')"
if [ -z "$summary" ]; then echo "Could not read the test results."; exit 1; fi
echo "$summary"
case "$summary" in *" 0 failures") exit 0 ;; *) exit 1 ;; esac
