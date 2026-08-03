package com.carddemo.interestcalc.file;

import com.carddemo.interestcalc.copybook.TranCatBalanceRecord;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;

/**
 * TCATBALF: {@code ORGANIZATION IS INDEXED, ACCESS MODE IS SEQUENTIAL, RECORD KEY IS
 * FD-TRAN-CAT-KEY}. A sequential read of a KSDS returns records in ascending key order,
 * regardless of the order they were loaded in, so the dataset is sorted on load.
 */
public final class TranCatBalanceFile {

    private final List<TranCatBalanceRecord> records;
    private int cursor;

    private TranCatBalanceFile(List<TranCatBalanceRecord> records) {
        this.records = records;
    }

    public static TranCatBalanceFile load(Path file) {
        List<TranCatBalanceRecord> records = DatasetFiles.read(file, TranCatBalanceRecord::parse).stream()
                .sorted(Comparator.comparing(TranCatBalanceRecord::key))
                .toList();
        return new TranCatBalanceFile(records);
    }

    /** {@code READ TCATBAL-FILE INTO ...}: status {@code '00'} or {@code '10'} at end of file. */
    public KeyedRead<TranCatBalanceRecord> readNext() {
        if (cursor >= records.size()) {
            return KeyedRead.endOfFile();
        }
        return KeyedRead.found(records.get(cursor++));
    }

    public int size() {
        return records.size();
    }
}
