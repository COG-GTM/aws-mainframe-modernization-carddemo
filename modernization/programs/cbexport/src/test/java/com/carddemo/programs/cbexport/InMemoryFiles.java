package com.carddemo.programs.cbexport;

import com.carddemo.programs.cbexport.io.FileStatus;
import com.carddemo.programs.cbexport.io.ReadResult;
import com.carddemo.programs.cbexport.io.RecordReader;
import com.carddemo.programs.cbexport.io.RecordWriter;
import java.util.ArrayList;
import java.util.List;

/** In-memory readers/writers that can inject COBOL FILE STATUS failures. */
final class InMemoryFiles {

    private InMemoryFiles() {
    }

    static final class Reader implements RecordReader {
        final List<byte[]> records;
        String openStatus = FileStatus.OK;
        int failAtRead = -1;
        String failStatus;
        int reads;
        boolean opened;
        boolean closed;

        Reader(List<byte[]> records) {
            this.records = records;
        }

        Reader failOpen(String status) {
            openStatus = status;
            return this;
        }

        Reader failRead(int index, String status) {
            failAtRead = index;
            failStatus = status;
            return this;
        }

        @Override
        public String open() {
            opened = true;
            return openStatus;
        }

        @Override
        public ReadResult read() {
            int i = reads++;
            if (i == failAtRead) {
                return ReadResult.status(failStatus);
            }
            return i < records.size() ? ReadResult.ok(records.get(i)) : ReadResult.status(FileStatus.EOF);
        }

        @Override
        public void close() {
            closed = true;
        }
    }

    static final class Writer implements RecordWriter {
        final List<byte[]> written = new ArrayList<>();
        String openStatus = FileStatus.OK;
        int failAtWrite = -1;
        String failStatus;
        boolean opened;
        boolean closed;

        Writer failOpen(String status) {
            openStatus = status;
            return this;
        }

        Writer failWrite(int index, String status) {
            failAtWrite = index;
            failStatus = status;
            return this;
        }

        @Override
        public String open() {
            opened = true;
            return openStatus;
        }

        @Override
        public String write(byte[] record) {
            if (written.size() == failAtWrite) {
                return failStatus;
            }
            written.add(record);
            return FileStatus.OK;
        }

        @Override
        public void close() {
            closed = true;
        }
    }
}
