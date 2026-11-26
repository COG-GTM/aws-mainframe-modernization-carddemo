package com.carddemo.programs.cbtrn02c;

import com.carddemo.programs.cbtrn02c.io.IndexedFile;
import com.carddemo.programs.cbtrn02c.io.OpenMode;
import com.carddemo.programs.cbtrn02c.io.ReadResult;
import com.carddemo.programs.cbtrn02c.io.SequentialReader;
import com.carddemo.programs.cbtrn02c.io.SequentialWriter;

import java.util.HashMap;
import java.util.Map;

/** Fault-injecting decorators: force a FILE STATUS for a given operation. */
final class Fakes {
    private Fakes() {
    }

    static final class FaultyIndexedFile implements IndexedFile {
        final IndexedFile delegate;
        final Map<String, String> forced = new HashMap<>();

        FaultyIndexedFile(IndexedFile delegate) {
            this.delegate = delegate;
        }

        FaultyIndexedFile fail(String op, String status) {
            forced.put(op, status);
            return this;
        }

        @Override
        public String open(OpenMode mode) {
            return forced.containsKey("open") ? forced.get("open") : delegate.open(mode);
        }

        @Override
        public ReadResult read(String key) {
            return forced.containsKey("read") ? ReadResult.of(forced.get("read")) : delegate.read(key);
        }

        @Override
        public String write(String record) {
            return forced.containsKey("write") ? forced.get("write") : delegate.write(record);
        }

        @Override
        public String rewrite(String record) {
            return forced.containsKey("rewrite") ? forced.get("rewrite") : delegate.rewrite(record);
        }

        @Override
        public String close() {
            return forced.containsKey("close") ? forced.get("close") : delegate.close();
        }
    }

    static final class FaultyReader implements SequentialReader {
        final SequentialReader delegate;
        final Map<String, String> forced = new HashMap<>();

        FaultyReader(SequentialReader delegate) {
            this.delegate = delegate;
        }

        FaultyReader fail(String op, String status) {
            forced.put(op, status);
            return this;
        }

        @Override
        public String open() {
            return forced.containsKey("open") ? forced.get("open") : delegate.open();
        }

        @Override
        public ReadResult read() {
            return forced.containsKey("read") ? ReadResult.of(forced.get("read")) : delegate.read();
        }

        @Override
        public String close() {
            return forced.containsKey("close") ? forced.get("close") : delegate.close();
        }
    }

    static final class FaultyWriter implements SequentialWriter {
        final SequentialWriter delegate;
        final Map<String, String> forced = new HashMap<>();

        FaultyWriter(SequentialWriter delegate) {
            this.delegate = delegate;
        }

        FaultyWriter fail(String op, String status) {
            forced.put(op, status);
            return this;
        }

        @Override
        public String open() {
            return forced.containsKey("open") ? forced.get("open") : delegate.open();
        }

        @Override
        public String write(String record) {
            return forced.containsKey("write") ? forced.get("write") : delegate.write(record);
        }

        @Override
        public String close() {
            return forced.containsKey("close") ? forced.get("close") : delegate.close();
        }
    }
}
