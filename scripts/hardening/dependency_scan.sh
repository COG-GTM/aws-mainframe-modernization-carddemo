#!/usr/bin/env bash
# s6.4 dependency scan (make dependency-scan; not a CI gate, the vulnerability feeds are network-flaky).
#
#   scripts/hardening/dependency_scan.sh [out-dir]      default build/dependency-scan
#
# 1. npm audit --omit=dev in modernization/carddemo-ui (production dependencies of the shipped bundle).
# 2. Java (modernization/ reactor): Snyk CLI when `snyk` is installed and authenticated (SNYK_TOKEN or `snyk auth`),
#    else OWASP dependency-check-maven `aggregate` (set NVD_API_KEY: without a key the NVD download takes hours).
#    DEPENDENCY_SCAN_TOOL=owasp|snyk forces one.
# Exits non-zero when a scanner reports a high or critical finding; docs/validation/hardening/dependency-scan.md
# records the triage of every remaining finding.
set -uo pipefail
cd "$(dirname "$0")/../.."
OUT="$(realpath -m "${1:-build/dependency-scan}")"
mkdir -p "$OUT"
export JAVA_HOME="${JAVA_HOME:-/usr/lib/jvm/java-21-openjdk-amd64}"
export PATH="$JAVA_HOME/bin:$PATH"
status=0

echo "== npm audit --omit=dev (modernization/carddemo-ui)"
(cd modernization/carddemo-ui && npm audit --omit=dev --audit-level=high) | tee "$OUT/npm-audit.txt"
[ "${PIPESTATUS[0]}" = 0 ] || status=1

tool="${DEPENDENCY_SCAN_TOOL:-}"
if [ -z "$tool" ]; then
    if command -v snyk >/dev/null && snyk whoami >/dev/null 2>&1; then tool=snyk; else tool=owasp; fi
fi
echo "== Java dependencies ($tool)"
if [ "$tool" = snyk ]; then
    (cd modernization && snyk test --all-projects --severity-threshold=high \
        --json-file-output="$OUT/snyk.json") | tee "$OUT/snyk.txt"
    [ "${PIPESTATUS[0]}" = 0 ] || status=1
else
    (cd modernization && mvn -B -ntp org.owasp:dependency-check-maven:12.1.0:aggregate \
        -DfailBuildOnCVSS=7 -Dformats=HTML,JSON -DoutputDirectory="$OUT/owasp" \
        ${NVD_API_KEY:+-DnvdApiKey="$NVD_API_KEY"} -DnvdApiDelay="${NVD_API_DELAY:-8000}" \
        -DossindexAnalyzerEnabled=false -DnodeAuditAnalyzerEnabled=false -DretireJsAnalyzerEnabled=false) \
        | tee "$OUT/owasp.txt"
    [ "${PIPESTATUS[0]}" = 0 ] || status=1
fi
echo "reports in $OUT (triage: docs/validation/hardening/dependency-scan.md)"
exit $status
