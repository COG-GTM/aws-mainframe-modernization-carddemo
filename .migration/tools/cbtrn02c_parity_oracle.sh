#!/usr/bin/env bash
# Independent CBTRN02C parity pass: builds every case of cbtrn02c_parity_cases.py (plus baseline, empty, CP037,
# rerun, restart and missing-DALYTRAN variants) with the GnuCOBOL oracle (posttran_gnucobol_oracle.sh) and copies
# inputs + unchanged oracle outputs into DEST (default modernization/carddemo-app/src/test/resources/parity/cbtrn02c).
# Reads app/** only. Usage: .migration/tools/cbtrn02c_parity_oracle.sh WORK_DIR [DEST]   (WORK_DIR must not exist)
set -euo pipefail
REPO=$(cd "$(dirname "$0")/../.." && pwd)
WORK=$(realpath -m -- "${1:?usage: cbtrn02c_parity_oracle.sh WORK_DIR [DEST]}")
DEST=$(realpath -m -- "${2:-$REPO/modernization/carddemo-app/src/test/resources/parity/cbtrn02c}")
ORACLE="$REPO/.migration/tools/posttran_gnucobol_oracle.sh"
export ORACLE_NOW=${ORACLE_NOW-2022/07/18 10:30:45}
[ -e "$WORK" ] && { echo "refusing WORK_DIR '$WORK': it must not exist" >&2; exit 2; }
mkdir -p "$WORK" "$DEST"
python3 "$REPO/.migration/tools/cbtrn02c_parity_cases.py" "$WORK/in" > /dev/null

oracle() { # out_name input_dir dalytran_file [encoding]
  local env=(DALYTRAN_ENCODING="${4:-ASCII}")
  for f in acctdata cardxref tcatbal; do
    [ -f "$2/$f.txt" ] && env+=("$(echo "$f" | tr a-z A-Z)_FILE=$2/$f.txt")
  done
  env "${env[@]}" "$ORACLE" "$WORK/$1" "$3" > "$WORK/$1.log" 2>&1 || { cat "$WORK/$1.log" >&2; exit 1; }
}
publish() { # case oracle_run input_dir kind [charset] [daly_src]
  local c=$1 run=$2 in=$3 kind=$4 charset=${5:-ISO-8859-1}
  [ -e "$DEST/$c" ] && { echo "refusing case '$c': $DEST/$c exists" >&2; exit 2; }
  mkdir -p "$DEST/$c/input" "$DEST/$c/expected"
  if [ -n "${6:-}" ]; then cp "$6" "$DEST/$c/input/DALYTRAN"; else cp "$WORK/$run/data/DALYTRAN" "$DEST/$c/input/DALYTRAN"; fi
  for f in acctdata cardxref tcatbal; do [ -f "$in/$f.txt" ] && cp "$in/$f.txt" "$DEST/$c/input/"; done
  cp "$WORK/$run/expected/"* "$DEST/$c/expected/"
  printf 'kind=%s\ndalytran.charset=%s\noracle.now=%s\n' "$kind" "$charset" "$ORACLE_NOW" > "$DEST/$c/case.properties"
  echo "$c RC=$(cat "$DEST/$c/expected/RETURN-CODE") rejects=$(( $(wc -c < "$DEST/$c/expected/DALYREJS") / 430 ))"
}

for c in limit-exact expiry-boundary reason-precedence same-account-multi tcatbal-create-rewrite ovf-temp-bal \
         ovf-acct-bal ovf-cyc ovf-tcatbal negative-zero dup-rejected-id; do
  oracle "$c" "$WORK/in/$c" "$WORK/in/$c/dailytran.txt"; publish "$c" "$c" "$WORK/in/$c" single
done
mkdir -p "$WORK/in/baseline" "$WORK/in/empty"; : > "$WORK/in/empty/dailytran.txt"
oracle baseline "$WORK/in/baseline" "$REPO/app/data/ASCII/dailytran.txt"; publish baseline baseline "$WORK/in/baseline" single
oracle empty "$WORK/in/empty" "$WORK/in/empty/dailytran.txt"; publish empty empty "$WORK/in/empty" single

# dup-abend-newkey: DALYREJS/SYSOUT/RC from the full run, tables = run on the records before the abending one (D-042);
# the full-run (half-posted) tables are kept in expected-full/ for the divergence report.
oracle dup-abend-newkey "$WORK/in/dup-abend-newkey" "$WORK/in/dup-abend-newkey/dailytran.txt"
oracle dup-atomic "$WORK/in/dup-abend-newkey.atomic" "$WORK/in/dup-abend-newkey.atomic/dailytran.txt"
publish dup-abend-newkey dup-abend-newkey "$WORK/in/dup-abend-newkey" single
mkdir -p "$DEST/dup-abend-newkey/expected-atomic" "$DEST/dup-abend-newkey/expected-full"
cp "$WORK/dup-abend-newkey/expected/"{TRANSACT,ACCTDATA,TCATBALF}.unl "$DEST/dup-abend-newkey/expected-full/"
cp "$WORK/dup-atomic/expected/"{TRANSACT,ACCTDATA,TCATBALF}.unl "$DEST/dup-abend-newkey/expected-atomic/"
echo 'tables.expected=expected-atomic' >> "$DEST/dup-abend-newkey/case.properties"

# restart-fixed: leg 1 = dup-abend-newkey input (abends); leg 2 = restart of the same instance after the operator fixed
# record 3 in place. Expected final DALYREJS/tables/counters = fresh oracle run on the fixed file (D-012).
oracle restart-fixed "$WORK/in/restart-fixed" "$WORK/in/restart-fixed/dailytran.txt"
publish restart-fixed restart-fixed "$WORK/in/restart-fixed" restart
cp "$WORK/dup-abend-newkey/data/DALYTRAN" "$DEST/restart-fixed/input/DALYTRAN.leg1"
mkdir -p "$DEST/restart-fixed/expected-leg1"; cp "$WORK/dup-abend-newkey/expected/"{RETURN-CODE,DALYREJS,SYSOUT} "$DEST/restart-fixed/expected-leg1/"

# rerun: two consecutive CBTRN02C runs on the same masters (TRANSACT OPEN OUTPUT again, ACCTDATA/TCATBALF kept)
oracle rerun "$WORK/in/rerun" "$WORK/in/rerun/dailytran.txt"
mkdir -p "$WORK/rerun/run2"
( cd "$WORK/rerun/bin"; export COB_LIBRARY_PATH="$WORK/rerun/bin" DD_DALYTRAN="$WORK/rerun/data/DALYTRAN" \
    DD_TRANFILE="$WORK/rerun/data/TRANSACT" DD_XREFFILE="$WORK/rerun/data/CARDXREF" DD_DALYREJS="$WORK/rerun/run2/DALYREJS" \
    DD_ACCTFILE="$WORK/rerun/data/ACCTDATA" DD_TCATBALF="$WORK/rerun/data/TCATBALF"
  set +e; COB_CURRENT_DATE="$ORACLE_NOW" ./CBTRN02C > "$WORK/rerun/run2/SYSOUT" 2>&1; echo $? > "$WORK/rerun/run2/RETURN-CODE"; set -e
  INFILE="$WORK/rerun/data/TRANSACT" OUTFILE="$WORK/rerun/run2/TRANSACT.unl" ./ULTRAN
  INFILE="$WORK/rerun/data/ACCTDATA" OUTFILE="$WORK/rerun/run2/ACCTDATA.unl" ./ULACCT
  INFILE="$WORK/rerun/data/TCATBALF" OUTFILE="$WORK/rerun/run2/TCATBALF.unl" ./ULTCAT )
publish rerun rerun "$WORK/in/rerun" rerun
mkdir -p "$DEST/rerun/expected-run1"; mv "$DEST/rerun/expected/"* "$DEST/rerun/expected-run1/"; cp "$WORK/rerun/run2/"* "$DEST/rerun/expected/"
echo "rerun run2 RC=$(cat "$DEST/rerun/expected/RETURN-CODE")"

# cp037: same-account-multi and ovf-temp-bal re-encoded as RECFM=FB CP037; oracle must equal the ASCII run
for c in same-account-multi ovf-temp-bal; do
  python3 -c 'import sys; d=open(sys.argv[1],"rb").read().decode("latin-1"); open(sys.argv[2],"wb").write(d.encode("cp037"))' \
    "$WORK/$c/data/DALYTRAN" "$WORK/in/$c/DALYTRAN.cp037"
  oracle "$c-cp037" "$WORK/in/$c" "$WORK/in/$c/DALYTRAN.cp037" CP037
  cmp "$WORK/$c-cp037/expected/DALYREJS" "$WORK/$c/expected/DALYREJS"
  publish "$c-cp037" "$c-cp037" "$WORK/in/$c" single IBM037 "$WORK/in/$c/DALYTRAN.cp037"
done

# dalytran-missing: DD_DALYTRAN names no file -> OPEN INPUT status 35 -> abend before TRANSACT is opened (cbl:195-196)
mkdir -p "$WORK/missing/expected"
cp -r "$WORK/empty/bin" "$WORK/missing/bin"; mkdir -p "$WORK/missing/data"
for f in ACCTDATA CARDXREF TCATBALF; do cp -r "$WORK/baseline/data/$f"* "$WORK/missing/data/" 2>/dev/null || true; done
( cd "$WORK/missing/bin"; export COB_LIBRARY_PATH="$WORK/missing/bin" DD_DALYTRAN="$WORK/missing/data/NO-SUCH-DALYTRAN" \
    DD_TRANFILE="$WORK/missing/data/TRANSACT" DD_XREFFILE="$WORK/missing/data/CARDXREF" DD_DALYREJS="$WORK/missing/expected/DALYREJS" \
    DD_ACCTFILE="$WORK/missing/data/ACCTDATA" DD_TCATBALF="$WORK/missing/data/TCATBALF"
  set +e; COB_CURRENT_DATE="$ORACLE_NOW" ./CBTRN02C > "$WORK/missing/expected/SYSOUT" 2>&1; echo $? > "$WORK/missing/expected/RETURN-CODE"; set -e )
mkdir -p "$DEST/dalytran-missing/expected"
cp "$WORK/missing/expected/"{SYSOUT,RETURN-CODE} "$DEST/dalytran-missing/expected/"
printf 'kind=missing\ndalytran.charset=ISO-8859-1\noracle.now=%s\n' "$ORACLE_NOW" > "$DEST/dalytran-missing/case.properties"
echo "dalytran-missing RC=$(cat "$DEST/dalytran-missing/expected/RETURN-CODE") DALYREJS created: $( [ -e "$WORK/missing/expected/DALYREJS" ] && echo yes || echo no) TRANSACT created: $( [ -e "$WORK/missing/data/TRANSACT" ] && echo yes || echo no)"
