#!/bin/bash
# Runs inside the IBM Db2 Community container (mounted at /var/custom, executed
# after "Setup has completed"). Applies the legacy CardDemo DDL and load
# control files straight from app/app-transaction-type-db2 (mounted read-only
# at /carddemo-legacy). Idempotent: skips if the schema already exists.
set -euo pipefail

LEGACY=${CARDDEMO_LEGACY_DIR:-/carddemo-legacy}
DB=${DBNAME:-CARDDEMO}
WORK=/tmp/carddemo-sql
MARKER=/database/config/.carddemo-ready
mkdir -p "$WORK"

# z/OS-only index options (ERASE NO / CLOSE NO) are not valid on Db2 LUW.
cp "$LEGACY/ddl/TRNTYPE.ddl" "$LEGACY/ddl/TRNTYCAT.ddl" "$WORK/"
for f in XTRNTYPE XTRNTYCAT; do
  sed -E 's/\b(ERASE|CLOSE)[[:space:]]+NO\b//g' "$LEGACY/ddl/$f.ddl" > "$WORK/$f.ddl"
done
# Strip the /* licence */ banner lines from the DSNTEP4 load control files.
for f in DB2LTTYP DB2LTCAT; do
  sed -E '/^[[:space:]]*\/\*.*\*\/[[:space:]]*$/d' "$LEGACY/ctl/$f.ctl" > "$WORK/$f.sql"
done
chmod -R a+rwX "$WORK"

su - db2inst1 -c "
  set -e
  db2 connect to $DB >/dev/null
  # CLP connections belong to this shell: never run db2 inside \$(...), read results from a file instead.
  count() { db2 -x \"\$1\" > $WORK/count.out; N=\$(tr -dc '0-9' < $WORK/count.out); }
  table() { count \"SELECT COUNT(*) FROM SYSCAT.TABLES WHERE TABSCHEMA='CARDDEMO' AND TABNAME='\$1'\"; }
  table TRANSACTION_TYPE; if [ \"\$N\" = \"0\" ]; then
    db2 -tvf $WORK/TRNTYPE.ddl
    db2 -tvf $WORK/XTRNTYPE.ddl || true   # SQL0605W: PK index already covers TR_TYPE
  fi
  table TRANSACTION_TYPE_CATEGORY; if [ \"\$N\" = \"0\" ]; then
    db2 -tvf $WORK/TRNTYCAT.ddl
    db2 -tvf $WORK/XTRNTYCAT.ddl || true
  fi
  count 'SELECT COUNT(*) FROM CARDDEMO.TRANSACTION_TYPE'; if [ \"\$N\" = \"0\" ]; then
    db2 -tvf $WORK/DB2LTTYP.sql
  fi
  count 'SELECT COUNT(*) FROM CARDDEMO.TRANSACTION_TYPE_CATEGORY'; if [ \"\$N\" = \"0\" ]; then
    db2 -tvf $WORK/DB2LTCAT.sql
  fi
  db2 \"SELECT COUNT(*) AS TRANSACTION_TYPES FROM CARDDEMO.TRANSACTION_TYPE\"
  db2 \"SELECT COUNT(*) AS CATEGORIES FROM CARDDEMO.TRANSACTION_TYPE_CATEGORY\"
  db2 connect reset >/dev/null
"
touch "$MARKER"
echo "CardDemo transaction-type schema ready"
