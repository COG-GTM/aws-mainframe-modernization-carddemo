#!/bin/sh
# Runs once on first container start (empty volume): creates schema carddemo and applies aws/db/schema.sql
# (mounted at /schema) when the data-migration session has produced it.
set -eu
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" -c "CREATE SCHEMA IF NOT EXISTS carddemo"
if [ -f /schema/schema.sql ]; then
  echo "carddemo: applying /schema/schema.sql"
  psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" \
    -c "SET search_path TO carddemo, public" -f /schema/schema.sql
else
  echo "carddemo: /schema/schema.sql not found (aws/db/schema.sql); schema left empty"
fi
