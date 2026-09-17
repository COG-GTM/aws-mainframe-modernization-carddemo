#!/usr/bin/env bash
# Run the Spring Batch posting candidate from pull request #9
# (modernization/posttran-cycle/transaction-posting-service) against a golden
# input set and reconcile its outputs with compare.py.
#
#   bash tests/golden/run_candidate_pr9.sh <candidate-checkout-dir> [--set named|volume] [--out DIR]
#
# <candidate-checkout-dir> is a SEPARATE clone of the repository at the
# candidate commit (this branch does not contain or depend on it).  The script
# never edits the candidate: it builds it with Maven as-is, points it at copies
# of the golden inputs through its own configuration properties, freezes the
# JVM wall clock with faketime(1) so TRAN-PROC-TS is comparable with the frozen
# COB_CURRENT_DATE of the reference run, and captures exit code + console.
#
# Requirements: JDK 21, Maven (network access to a Maven repository, or
# GS_MVN_ARGS="-s settings.xml" pointing at a mirror), faketime (libfaketime), python3.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE/../.."   # repo root: reports then show repo-relative paths
CAND=""
SET="named"
OUT=""
while [ $# -gt 0 ]; do
  case "$1" in
    --set) SET="$2"; shift 2 ;;
    --out) OUT="$2"; shift 2 ;;
    -*) echo "unknown option $1" >&2; exit 64 ;;
    *) CAND="$1"; shift ;;
  esac
done
[ -n "$CAND" ] || { echo "usage: $0 <candidate-checkout-dir> [--set NAME] [--out DIR]" >&2; exit 64; }
case "$SET" in named|volume) ;; *) echo "--set must be named or volume" >&2; exit 64 ;; esac
MOD="$CAND/modernization/posttran-cycle"
[ -f "$MOD/pom.xml" ] || { echo "no $MOD/pom.xml" >&2; exit 65; }
INPUT="$HERE/sets/$SET/input"
EXPECTED="$HERE/sets/$SET/expected"
[ -f "$INPUT/DALYTRAN" ] || { echo "no golden input at $INPUT; run run_reference.sh first" >&2; exit 65; }
[ -z "$OUT" ] && OUT="tests/golden/sets/$SET/candidate-pr9"
mkdir -p "$OUT"; OUT="$(cd "$OUT" && pwd)"
command -v faketime >/dev/null || { echo "faketime not installed (apt-get install faketime)" >&2; exit 67; }
command -v mvn >/dev/null || { echo "mvn not installed" >&2; exit 67; }

# evidence must not embed one machine's paths or account name
REPO="$(pwd)"; CAND_ABS="$(cd "$CAND" && pwd)"
redact() { sed -e "s#$REPO#<repo>#g" -e "s#$CAND_ABS#<candidate-checkout>#g" -e "s#started by [^ ]* in#started by <user> in#"; }

FROZEN="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["frozen_clock"]["COB_CURRENT_DATE"])' "$INPUT/manifest.json")"
CAND_SHA="$(git -C "$CAND" rev-parse HEAD 2>/dev/null || echo unknown)"

# 1. build (skip tests: they are the candidate's own; this harness is the external check)
JAR="$(ls "$MOD"/transaction-posting-service/target/transaction-posting-service-*.jar 2>/dev/null | grep -v original | head -1 || true)"
if [ -z "$JAR" ]; then
  echo "== building candidate ($CAND_SHA) with Maven"
  # GS_MVN_ARGS: extra Maven arguments, e.g. "-s settings.xml" naming a repository mirror
  # shellcheck disable=SC2086
  (cd "$MOD" && mvn -q ${GS_MVN_ARGS:-} -pl carddemo-recordio,transaction-posting-service -am -DskipTests package)
  JAR="$(ls "$MOD"/transaction-posting-service/target/transaction-posting-service-*.jar | grep -v original | head -1)"
fi
echo "== jar: $(basename "$JAR")"

# 2. stage inputs (the candidate rewrites ACCTFILE/TCATBALF in place, so work on copies)
rm -rf "$OUT"; mkdir -p "$OUT/work"
cp "$INPUT/DALYTRAN" "$OUT/work/DALYTRAN"
cp "$INPUT/XREFFILE" "$OUT/work/XREFFILE"
cp "$INPUT/ACCTFILE" "$OUT/work/ACCTFILE"
cp "$INPUT/TCATBALF" "$OUT/work/TCATBALF"

# 3. run with frozen clock; properties override application.yml, nothing in the candidate is edited
FAKE="@$(echo "$FROZEN" | sed 's/\.[0-9]*$//') x0"   # faketime: start at YYYY-MM-DD HH:MM:SS, rate 0 = frozen
JAVA_ARGS=(
  "--carddemo.posting.encoding=ASCII"
  "--carddemo.posting.daily-transactions=$OUT/work/DALYTRAN"
  "--carddemo.posting.card-xref=$OUT/work/XREFFILE"
  "--carddemo.posting.account-master=$OUT/work/ACCTFILE"
  "--carddemo.posting.category-balances=$OUT/work/TCATBALF"
  "--carddemo.posting.transaction-master=$OUT/work/TRANSACT"
  "--carddemo.posting.rejects=$OUT/work/DALYREJS"
  "--logging.level.root=WARN"
  "--logging.level.com.carddemo=INFO"
  "--logging.pattern.console=%msg%n"
)
echo "== running: faketime -f \"$FAKE\" java -jar $(basename "$JAR") ${JAVA_ARGS[*]}" | redact
set +e
FAKETIME_DONT_FAKE_MONOTONIC=1 DONT_FAKE_MONOTONIC=1 TZ=UTC \
  timeout 600 faketime -f "$FAKE" java -jar "$JAR" "${JAVA_ARGS[@]}" > "$OUT/SYSOUT.raw" 2>&1
RC=$?
set -e
echo "$RC" > "$OUT/RETURN-CODE"
redact < "$OUT/SYSOUT.raw" > "$OUT/SYSOUT"; rm -f "$OUT/SYSOUT.raw"
JAR_SHOWN="$(echo "$JAR" | redact)"
for f in TRANSACT DALYREJS ACCTFILE TCATBALF; do
  [ -f "$OUT/work/$f" ] && cp "$OUT/work/$f" "$OUT/$f"
done
rm -rf "$OUT/work"
python3 - "$OUT/run.json" "$CAND_SHA" "$JAR_SHOWN" "$FAKE" "$RC" "$SET" <<'EOF'
import json, subprocess, sys
out, sha, jar, fake, rc, s = sys.argv[1:7]
info = {"candidate": "pull request #9 transaction-posting-service", "candidate_commit": sha,
        "jar": jar, "set": s, "faketime": fake, "exit_code": int(rc),
        "java": subprocess.run(["java", "-version"], capture_output=True, text=True).stderr.strip().splitlines()[0],
        "note": "candidate built and run unmodified; file locations, ASCII encoding and clock supplied externally"}
json.dump(info, open(out, "w"), indent=2); open(out, "a").write("\n")
EOF
echo "== candidate exit code $RC; console:"
sed 's/^/   /' "$OUT/SYSOUT" | tail -8

# 4. reconcile
echo "== compare expected vs candidate"
set +e
python3 "$HERE/compare.py" "$EXPECTED" "$OUT" --out-dir "$OUT" | redact
CRC=${PIPESTATUS[0]}
set -e
echo "== compare exit $CRC; report $(echo "$OUT" | redact)/reconciliation.md"
exit "$CRC"
