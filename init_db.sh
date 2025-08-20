#!/bin/bash
set -e

psql -U postgres -d "$MYMAILER_DB" -h "$MYMAILER_DB_HOST" -p 5432 -f schema.sql
