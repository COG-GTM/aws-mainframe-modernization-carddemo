package com.carddemo.loader;

import com.carddemo.common.record.Field;

/**
 * One copybook field and the column it is stored in (docs/migration/schema-mapping.md).
 *
 * @param key whether the column is part of the table's primary key (the VSAM record key)
 */
record Column(Field field, String name, Kind kind, boolean key) {

    /** How the field's value is converted between the record and the column. */
    enum Kind {
        /** {@code PIC X(n)} to {@code VARCHAR(n)}: blank padding trimmed, all blanks to null. */
        TEXT,
        /** {@code PIC 9(n)} to {@code BIGINT}. */
        LONG,
        /** {@code PIC 9(n)} code to {@code INTEGER}. */
        INT,
        /** {@code PIC S9(n)V99} (zoned, sign overpunched) to {@code NUMERIC}. */
        DECIMAL,
        /** {@code PIC X(10)} {@code YYYY-MM-DD} to {@code DATE}. */
        DATE,
        /** {@code PIC X(26)} {@code YYYY-MM-DD HH:MM:SS.ffffff} to {@code TIMESTAMP}. */
        TIMESTAMP,
        /** {@code SEC-USR-PWD}: stored as the BCrypt hash, the plain value is never persisted. */
        PASSWORD
    }

    static Column key(Field field, String name, Kind kind) {
        return new Column(field, name, kind, true);
    }

    static Column of(Field field, String name, Kind kind) {
        return new Column(field, name, kind, false);
    }
}
