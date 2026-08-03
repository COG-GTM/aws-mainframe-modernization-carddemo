package com.carddemo.interestcalc.file;

import com.carddemo.interestcalc.copybook.DisclosureGroupRecord;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

/**
 * DISCGRP: {@code ORGANIZATION IS INDEXED, ACCESS MODE IS RANDOM, RECORD KEY IS
 * FD-DISCGRP-KEY}. A miss returns file status {@code '23'}, which is the trigger for the
 * {@code 'DEFAULT'} group retry in {@code 1200-GET-INTEREST-RATE}.
 */
public final class DisclosureGroupFile {

    private final Map<String, DisclosureGroupRecord> byKey;

    private DisclosureGroupFile(Map<String, DisclosureGroupRecord> byKey) {
        this.byKey = byKey;
    }

    public static DisclosureGroupFile load(Path file) {
        Map<String, DisclosureGroupRecord> map = new HashMap<>();
        DatasetFiles.read(file, DisclosureGroupRecord::parse).forEach(record -> map.put(record.key(), record));
        return new DisclosureGroupFile(map);
    }

    /** {@code READ DISCGRP-FILE INTO DIS-GROUP-RECORD}. */
    public KeyedRead<DisclosureGroupRecord> read(String key) {
        DisclosureGroupRecord record = byKey.get(key);
        return record == null ? KeyedRead.notFound() : KeyedRead.found(record);
    }
}
