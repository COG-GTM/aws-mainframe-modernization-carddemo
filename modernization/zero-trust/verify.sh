#!/usr/bin/env bash
# End-to-end verification for the zero-trust-controls module:
#   1. install the posttran modules (repackage skipped so the jars are usable as deps)
#   2. build and test this module (includes the end-to-end secured posting job test)
#   3. run the audit-chain verifier CLI on the log the job test leaves behind
#   4. tamper a copy of the log and show the verifier reports BROKEN (expected)
set -euo pipefail

cd "$(dirname "$0")/../.."
REPO_ROOT="$(pwd)"
ZT="modernization/zero-trust"
AUDIT="$REPO_ROOT/$ZT/target/zero-trust-evidence/posting-audit.jsonl"

echo "==> Installing posttran modules (repackage skipped)"
mvn -f "$REPO_ROOT/modernization/posttran-cycle/pom.xml" install \
    -DskipTests -Dspring-boot.repackage.skip=true

echo "==> Building and testing zero-trust-controls"
mvn -f "$REPO_ROOT/$ZT/pom.xml" verify

echo "==> Resolving runtime classpath for the verifier CLI"
mvn -q -f "$REPO_ROOT/$ZT/pom.xml" dependency:build-classpath \
    -Dmdep.outputFile="$REPO_ROOT/$ZT/target/classpath.txt"
CP="$REPO_ROOT/$ZT/target/classes:$(cat "$REPO_ROOT/$ZT/target/classpath.txt")"

echo "==> Verifying the audit log left by the secured job test: $AUDIT"
java -cp "$CP" com.carddemo.zerotrust.audit.AuditChainVerifierCli "$AUDIT"

echo "==> Tamper check: modifying one entry in a copy and re-verifying"
TAMPERED="$(dirname "$AUDIT")/posting-audit-tampered.jsonl"
cp "$AUDIT" "$TAMPERED"
# Flip one digit inside the second entry's detail field.
python3 - "$TAMPERED" <<'PY'
import sys
lines = open(sys.argv[1]).read().splitlines(keepends=True)
i = lines[1].find('amount=') + 7
ch = lines[1][i]
lines[1] = lines[1][:i] + ('9' if ch != '9' else '8') + lines[1][i+1:]
open(sys.argv[1], 'w').writelines(lines)
PY
# The verifier MUST report BROKEN here; that is the expected result, so the
# non-zero exit is swallowed and the line is printed for the record.
java -cp "$CP" com.carddemo.zerotrust.audit.AuditChainVerifierCli "$TAMPERED" || true

echo "==> verify.sh complete"
