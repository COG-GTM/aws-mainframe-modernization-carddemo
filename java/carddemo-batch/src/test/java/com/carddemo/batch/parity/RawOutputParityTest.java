package com.carddemo.batch.parity;

import com.carddemo.batch.io.RecordPrefix;
import com.carddemo.batch.program.Cbact01c;
import com.carddemo.batch.support.Cbact01cRun;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

/** Byte-level diff of the Java output files against golden-files/CBACT01C/raw/. */
class RawOutputParityTest {

    static void assertSameBytes(String file, byte[] expected, byte[] actual) {
        int n = Math.min(expected.length, actual.length);
        for (int i = 0; i < n; i++) {
            if (expected[i] != actual[i]) {
                int from = Math.max(0, i - 8);
                int to = Math.min(n, i + 8);
                fail(String.format("%s differs at byte offset %d (0x%X): golden %02X '%c' vs java %02X '%c'; "
                                + "golden[%d..%d]=%s java[%d..%d]=%s", file, i, i, expected[i] & 0xFF,
                        printable(expected[i]), actual[i] & 0xFF, printable(actual[i]), from, to,
                        hex(expected, from, to), from, to, hex(actual, from, to)));
            }
        }
        assertEquals(expected.length, actual.length, file + " total length");
    }

    private static char printable(byte b) {
        return b >= 0x20 && b < 0x7F ? (char) b : '.';
    }

    private static String hex(byte[] b, int from, int to) {
        StringBuilder sb = new StringBuilder();
        for (int i = from; i < to; i++) {
            sb.append(String.format("%02X", b[i] & 0xFF));
        }
        return sb.toString();
    }

    @Test
    void outfileIsByteIdentical() {
        Cbact01cRun run = Cbact01cRun.sample();
        assertSameBytes("OUTFILE", run.bytes(Repo.GOLDEN_CBACT01C.resolve("raw/OUTFILE")), run.bytes(run.outfile));
    }

    @Test
    void arryfileIsByteIdentical() {
        Cbact01cRun run = Cbact01cRun.sample();
        assertSameBytes("ARRYFILE", run.bytes(Repo.GOLDEN_CBACT01C.resolve("raw/ARRYFILE")), run.bytes(run.arryfile));
    }

    @Test
    void vbrcfileIsByteIdenticalIncludingGnuCobolLengthPrefixes() {
        Cbact01cRun run = Cbact01cRun.sample();
        assertSameBytes("VBRCFILE", run.bytes(Repo.GOLDEN_CBACT01C.resolve("raw/VBRCFILE")), run.bytes(run.vbrcfile));
    }

    @Test
    void vbrcfileLengthPrefixesAre12And39() {
        Cbact01cRun run = Cbact01cRun.sample();
        ByteBuffer bb = ByteBuffer.wrap(run.bytes(run.vbrcfile));
        int i = 0;
        while (bb.hasRemaining()) {
            int len = bb.getInt();
            assertEquals(i % 2 == 0 ? 12 : 39, len, "VBRCFILE record #" + (i + 1) + " RDW/length prefix");
            bb.position(bb.position() + len);
            i++;
        }
        assertEquals(100, i, "VBRCFILE records");
        assertEquals(50 * (4 + 12 + 4 + 39), run.bytes(run.vbrcfile).length, "VBRCFILE total bytes");
    }

    @Test
    void vbrcfileWithoutPrefixIsTheConcatenatedPayloads(@TempDir Path tmp) throws IOException {
        Cbact01cRun run = Cbact01cRun.sample();
        PrintStream sink = new PrintStream(new ByteArrayOutputStream(), true, StandardCharsets.ISO_8859_1);
        new Cbact01c(Repo.SAMPLE_ACCTDATA, tmp.resolve("OUTFILE"), tmp.resolve("ARRYFILE"), tmp.resolve("VBRCFILE"),
                RecordPrefix.NONE, sink).run();
        byte[] noPrefix = Files.readAllBytes(tmp.resolve("VBRCFILE"));
        assertEquals(50 * (12 + 39), noPrefix.length, "VBRCFILE payload-only length");
        ByteArrayOutputStream expected = new ByteArrayOutputStream();
        for (byte[] p : run.vbrcfilePayloads()) {
            expected.write(p);
        }
        assertSameBytes("VBRCFILE (payload only)", expected.toByteArray(), noPrefix);

        new Cbact01c(Repo.SAMPLE_ACCTDATA, tmp.resolve("OUTFILE"), tmp.resolve("ARRYFILE"), tmp.resolve("VBRCFILE.rdw"),
                RecordPrefix.ZOS_RDW, sink).run();
        byte[] rdw = Files.readAllBytes(tmp.resolve("VBRCFILE.rdw"));
        assertEquals(50 * (4 + 12 + 4 + 39), rdw.length, "VBRCFILE z/OS RDW length");
        assertEquals(Arrays.toString(new byte[] {0, 16, 0, 0}), Arrays.toString(Arrays.copyOf(rdw, 4)), "first RDW = 12 + 4");
    }
}
