package com.carddemo.common.copybook;

import java.util.List;
import java.util.Locale;

/** All 01/77-level records described by one copybook source. */
public record Copybook(String name, List<RecordLayout> records) {

    public RecordLayout first() {
        if (records.isEmpty()) {
            throw new IllegalStateException("Copybook " + name + " has no record descriptions");
        }
        return records.get(0);
    }

    public RecordLayout record(String recordName) {
        String wanted = recordName.toUpperCase(Locale.ROOT);
        return records.stream().filter(r -> r.name().equals(wanted)).findFirst()
                .orElseThrow(() -> new IllegalArgumentException("No record " + recordName + " in copybook " + name));
    }
}
