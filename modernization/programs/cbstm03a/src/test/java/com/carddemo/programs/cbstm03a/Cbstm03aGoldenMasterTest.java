package com.carddemo.programs.cbstm03a;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.carddemo.programs.cbstm03a.io.FixedWidthFileWriter;
import com.carddemo.programs.cbstm03a.io.RecordWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Fixture test: app/data/ASCII samples through CREASTMT STEP010 (reshape) and CBSTM03A,
 * compared byte-for-byte with output of the original COBOL under GnuCOBOL
 * (golden/generate-golden.sh).
 */
class Cbstm03aGoldenMasterTest {

    @TempDir
    Path tmp;

    @Test
    void sampleDataProducesExactlyTheCobolStatementAndHtmlFiles() {
        List<String> trnx = TestData.lines(TestData.ASCII.resolve("dailytran.txt")).stream()
                .map(TestData::reshapeDailyTran).toList();
        TestData.Run run = TestData.run(trnx,
                TestData.lines(TestData.ASCII.resolve("cardxref.txt")),
                TestData.lines(TestData.ASCII.resolve("custdata.txt")),
                TestData.lines(TestData.ASCII.resolve("acctdata.txt")));

        assertNull(run.abend());
        List<String> stmtGolden = TestData.golden("STMTFILE.txt");
        List<String> htmlGolden = TestData.golden("HTMLFILE.txt");
        assertEquals(1250, stmtGolden.size());
        assertEquals(6500, htmlGolden.size());
        assertEquals(stmtGolden.size(), run.stmt().size());
        assertEquals(htmlGolden.size(), run.html().size());
        for (int i = 0; i < stmtGolden.size(); i++) {
            assertEquals(stmtGolden.get(i), run.stmt().get(i), "STMTFILE record " + (i + 1));
        }
        for (int i = 0; i < htmlGolden.size(); i++) {
            assertEquals(htmlGolden.get(i), run.html().get(i), "HTMLFILE record " + (i + 1));
        }
    }

    @Test
    void recfmFbWritersProduceTheExactFixedBlockBytesWithEveryRecordFullWidth() throws IOException {
        List<String> trnx = TestData.lines(TestData.ASCII.resolve("dailytran.txt")).stream()
                .map(TestData::reshapeDailyTran).toList();
        Cbstm03bFiles files = Cbstm03bFiles.inMemory(trnx, TestData.lines(TestData.ASCII.resolve("cardxref.txt")),
                TestData.lines(TestData.ASCII.resolve("custdata.txt")),
                TestData.lines(TestData.ASCII.resolve("acctdata.txt")));
        Path stmtPath = tmp.resolve("stmt.fb");
        Path htmlPath = tmp.resolve("html.fb");
        List<String> rawHtml = new ArrayList<>();
        RecordWriter htmlFb = new FixedWidthFileWriter(htmlPath, 100, false);
        RecordWriter strictHtml = new RecordWriter() {
            @Override
            public void write(String record) {
                rawHtml.add(record);
                htmlFb.write(record);
            }

            @Override
            public void close() {
                htmlFb.close();
            }
        };
        new Cbstm03a(new JobContext("CREASTMT", "STEP040", List.of()), files,
                new FixedWidthFileWriter(stmtPath, 80, false), strictHtml, s -> { }).run();

        rawHtml.forEach(r -> assertEquals(100, r.length(), r));
        assertArrayEquals(String.join("", TestData.golden("STMTFILE.txt")).getBytes(StandardCharsets.ISO_8859_1),
                Files.readAllBytes(stmtPath));
        assertArrayEquals(String.join("", TestData.golden("HTMLFILE.txt")).getBytes(StandardCharsets.ISO_8859_1),
                Files.readAllBytes(htmlPath));
    }
}
