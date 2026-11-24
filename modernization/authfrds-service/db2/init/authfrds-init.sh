#!/bin/bash
# Runs inside the IBM Db2 Community container (mounted at /var/custom, executed after
# "Setup has completed"). Applies the legacy DDL straight from app/app-authorization-ims-db2-mq
# (mounted read-only at /carddemo-legacy). Idempotent: skips if the table already exists.
# The legacy module ships no Db2 load cards for AUTHFRDS, so the table starts empty.
set -euo pipefail

LEGACY=${CARDDEMO_LEGACY_DIR:-/carddemo-legacy}
DB=${DBNAME:-CARDDEMO}
WORK=/tmp/authfrds-sql
MARKER=/database/config/.authfrds-ready
mkdir -p "$WORK"

# z/OS-only clauses are not valid on Db2 LUW: index COPY YES / ERASE NO / CLOSE NO,
# and table-space placement (IN DATABASE / STOGROUP / BUFFERPOOL) if present.
for f in AUTHFRDS XAUTHFRD; do
  sed -E -e 's/\b(COPY[[:space:]]+YES|ERASE[[:space:]]+NO|CLOSE[[:space:]]+NO)\b//g' \
         -e 's/\b(IN[[:space:]]+DATABASE[[:space:]]+[A-Z0-9_.]+|STOGROUP[[:space:]]+[A-Z0-9_]+|BUFFERPOOL[[:space:]]+[A-Z0-9_]+)\b//g' \
         "$LEGACY/ddl/$f.ddl" > "$WORK/$f.ddl"
done
chmod -R a+rwX "$WORK"

su - db2inst1 -c "
  set -e
  db2 connect to $DB >/dev/null
  # CLP connections belong to this shell: never run db2 inside \$(...), read results from a file instead.
  count() { db2 -x \"\$1\" > $WORK/count.out; N=\$(tr -dc '0-9' < $WORK/count.out); }
  count \"SELECT COUNT(*) FROM SYSCAT.TABLES WHERE TABSCHEMA='CARDDEMO' AND TABNAME='AUTHFRDS'\"
  if [ \"\$N\" = \"0\" ]; then
    db2 -tvf $WORK/AUTHFRDS.ddl
  fi
  count \"SELECT COUNT(*) FROM SYSCAT.INDEXES WHERE INDSCHEMA='CARDDEMO' AND INDNAME='XAUTHFRD'\"
  if [ \"\$N\" = \"0\" ]; then
    # The PRIMARY KEY already created an equivalent index, so Db2 answers SQL0605W (warning, rc 2).
    # Tolerate exactly that; any other failure stops the script before the marker is touched.
    db2 -tvf $WORK/XAUTHFRD.ddl > $WORK/xauthfrd.out || { cat $WORK/xauthfrd.out; grep -q SQL0605W $WORK/xauthfrd.out || exit 1; }
  fi
  db2 \"SELECT COUNT(*) AS AUTHFRDS_ROWS FROM CARDDEMO.AUTHFRDS\"
  db2 \"SELECT INDNAME, COLNAMES, UNIQUERULE FROM SYSCAT.INDEXES WHERE TABNAME='AUTHFRDS'\"
  db2 connect reset >/dev/null
"
touch "$MARKER"
echo "CardDemo AUTHFRDS schema ready"
