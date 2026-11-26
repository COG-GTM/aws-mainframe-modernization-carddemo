package com.carddemo.programs.cobswait;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.Reader;
import java.io.StringReader;
import java.util.Optional;
import org.junit.jupiter.api.Test;

class LineSysinReaderTest {

    @Test
    void readsRecordsAndSignalsEof() {
        LineSysinReader r = new LineSysinReader(new StringReader("00003600\r\n00000001"));
        assertEquals(Optional.of("00003600"), r.acceptRecord());
        assertEquals(Optional.of("00000001"), r.acceptRecord());
        assertEquals(Optional.empty(), r.acceptRecord());
    }

    @Test
    void emptyLineIsARecord() {
        LineSysinReader r = new LineSysinReader(new StringReader("\n"));
        assertEquals(Optional.of(""), r.acceptRecord());
        assertEquals(Optional.empty(), r.acceptRecord());
    }

    @Test
    void truncatesToLreclAndSkipsRestOfLine() {
        LineSysinReader r = new LineSysinReader(new StringReader("1".repeat(200) + "\n00000002\n"));
        assertEquals(Optional.of("1".repeat(LineSysinReader.SYSIN_LRECL)), r.acceptRecord());
        assertEquals(Optional.of("00000002"), r.acceptRecord());
    }

    @Test
    void oversizedRecordIsNotBuffered() {
        Reader endless = new Reader() {
            private long remaining = 50_000_000L;

            @Override
            public int read(char[] buf, int off, int len) {
                if (remaining <= 0) {
                    return -1;
                }
                int n = (int) Math.min(len, remaining);
                java.util.Arrays.fill(buf, off, off + n, '9');
                remaining -= n;
                return n;
            }

            @Override
            public void close() {}
        };
        assertEquals(Optional.of("9".repeat(80)), new LineSysinReader(endless).acceptRecord());
    }

    @Test
    void rejectsNonPositiveLrecl() {
        assertThrows(IllegalArgumentException.class, () -> new LineSysinReader(new StringReader(""), 0));
    }
}
