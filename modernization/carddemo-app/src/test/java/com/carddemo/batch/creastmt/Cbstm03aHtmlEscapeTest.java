package com.carddemo.batch.creastmt;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.harness.BufferedSink;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.TestData;
import com.carddemo.customer.CustomerRecord;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * {@code carddemo.batch.creastmt.html-escape} (rules doc CBSTM03A.md, D-1): off, a name or address with markup is
 * written raw as by the legacy program; on, the name and address lines of STATEMNT.HTML are escaped, STATEMNT.PS and
 * every other HTML line stay the same.
 */
class Cbstm03aHtmlEscapeTest {

    @TempDir
    Path dir;

    /** CUSTDATA with customer 1's first name and address line 1 replaced by markup. */
    private Path custdataWithMarkup() throws IOException {
        List<String> lines = Files.readAllLines(CreastmtTestSupport.CUSTDATA, StandardCharsets.ISO_8859_1);
        FixedWidthRecord first = FixedWidthRecord.fromLine(CustomerRecord.MAPPER.layout(), lines.get(0),
                CreastmtTestSupport.ASCII);
        CustomerRecord c = CustomerRecord.MAPPER.fromRecord(first);
        CustomerRecord marked = new CustomerRecord(c.custId(), "<b>Tom&Jerry</b>", c.middleName(), c.lastName(),
                "1 \"Main\" St <script>x</script>", c.addrLine2(), c.addrLine3(), c.addrStateCd(), c.addrCountryCd(),
                c.addrZip(), c.phoneNum1(), c.phoneNum2(), c.ssn(), c.govtIssuedId(), c.dob(), c.eftAccountId(),
                c.priCardHolderInd(), c.ficoCreditScore());
        lines.set(0, CustomerRecord.MAPPER.toRecord(marked, CreastmtTestSupport.ASCII).text());
        Path file = dir.resolve("CUSTDATA-" + System.nanoTime() + ".txt");
        Files.write(file, lines, StandardCharsets.ISO_8859_1);
        return file;
    }

    private static List<FixedWidthRecord> trxfl() {
        return CreastmtJobConfiguration.trxfl(Dataset.TRANSACT.read(
                TestData.resolve("docs/validation/baseline/COMBTRAN/TRANSACT.ksds.txt"), CreastmtTestSupport.ASCII),
                CreastmtTestSupport.ASCII);
    }

    private record Out(List<String> stmt, List<String> html) {
    }

    private Out run(Path custdata, boolean escape) throws IOException {
        BufferedSink stmt = new BufferedSink(Cbstm03a.STMTFILE);
        BufferedSink html = new BufferedSink(Cbstm03a.HTMLFILE);
        try (Sysout sysout = Sysout.open(dir.resolve("sysout-" + System.nanoTime() + ".txt"))) {
            new Cbstm03a(CreastmtTestSupport.files(custdata, Cbstm03aHtmlEscapeTest::trxfl,
                    CreastmtTestSupport.XREFDATA), stmt, html, CreastmtTestSupport.ASCII, sysout, "CREASTMT",
                    "STEP040", escape).run();
        }
        return new Out(stmt.records().stream().map(FixedWidthRecord::text).toList(),
                html.records().stream().map(FixedWidthRecord::text).toList());
    }

    @Test
    void offWritesTheMarkupRawAndOnEscapesNamesAndAddresses() throws IOException {
        Path custdata = custdataWithMarkup();
        Out raw = run(custdata, false);
        Out escaped = run(custdata, true);

        assertThat(raw.html()).anySatisfy(l -> assertThat(l)
                .startsWith("<p style=\"font-size:16px\"><b>Tom&Jerry</b> "));
        assertThat(raw.html()).anySatisfy(l -> assertThat(l).startsWith("<p>1 \"Main\" St <script>x</script>  </p>"));
        assertThat(escaped.html()).anySatisfy(l -> assertThat(l)
                .startsWith("<p style=\"font-size:16px\">&lt;b&gt;Tom&amp;Jerry&lt;/b&gt; "));
        assertThat(escaped.html()).anySatisfy(l -> assertThat(l)
                .startsWith("<p>1 \"Main\" St &lt;script&gt;x&lt;/script&gt;  </p>"));
        assertThat(escaped.html()).noneSatisfy(l -> assertThat(l).contains("<script>"));
        assertThat(escaped.html()).allSatisfy(l -> assertThat(l).hasSize(Cbstm03a.HTML_LRECL));

        assertThat(escaped.stmt()).isEqualTo(raw.stmt());
        assertThat(escaped.html()).hasSameSizeAs(raw.html());
        int changed = 0;
        for (int i = 0; i < raw.html().size(); i++) {
            if (!raw.html().get(i).equals(escaped.html().get(i))) {
                changed++;
            }
        }
        assertThat(changed).as("only lines with markup change").isEqualTo(
                raw.html().stream().filter(l -> l.contains("Tom&Jerry") || l.contains("<script>")).count());
    }

    @Test
    void withoutMarkupBothModesWriteTheSameBytes() throws IOException {
        Out raw = run(CreastmtTestSupport.CUSTDATA, false);
        Out escaped = run(CreastmtTestSupport.CUSTDATA, true);
        assertThat(escaped.stmt()).isEqualTo(raw.stmt());
        assertThat(differences(raw.html(), escaped.html())).isEmpty();
    }

    private static List<String> differences(List<String> raw, List<String> escaped) {
        List<String> out = new java.util.ArrayList<>();
        for (int i = 0; i < Math.min(raw.size(), escaped.size()); i++) {
            if (!raw.get(i).equals(escaped.get(i))) {
                out.add(i + ": [" + raw.get(i) + "] vs [" + escaped.get(i) + "]");
            }
        }
        return out;
    }

    @Test
    void escapingNeverCutsAnEntity() {
        assertThat(Cbstm03a.escape("a&b", 5)).isEqualTo("a");
        assertThat(Cbstm03a.escape("a&b", 6)).isEqualTo("a&amp;");
        assertThat(Cbstm03a.escape("O'Neil \"Jr\"", 20)).isEqualTo("O'Neil \"Jr\"");
        assertThat(Cbstm03a.escape("plain", 3)).isEqualTo("pla");
    }
}
