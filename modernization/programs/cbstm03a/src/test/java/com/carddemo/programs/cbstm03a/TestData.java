package com.carddemo.programs.cbstm03a;

import com.carddemo.programs.cbstm03a.copybook.AccountRecord;
import com.carddemo.programs.cbstm03a.copybook.CardXrefRecord;
import com.carddemo.programs.cbstm03a.copybook.CustomerRecord;
import com.carddemo.programs.cbstm03a.copybook.TrnxRecord;
import com.carddemo.programs.cbstm03a.io.InMemoryRecordWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

final class TestData {

    static final Path ASCII = Path.of("../../../app/data/ASCII");

    private TestData() {
    }

    static String xref(String card, long custId, long acctId) {
        return new CardXrefRecord(card, custId, acctId, "").toFixedWidth();
    }

    static String customer(long id, String first, String middle, String last, String addr1, String addr2,
            String addr3, String state, String country, String zip, int fico) {
        return new CustomerRecord(id, first, middle, last, addr1, addr2, addr3, state, country, zip, "", "", 0L,
                "", "", "", "Y", fico, "").toFixedWidth();
    }

    static String customer(long id) {
        return customer(id, "Jane", "Q", "Public", "1 Main St", "Apt 2", "Springfield", "IL", "USA", "62701", 700);
    }

    static String account(long id, String currBal) {
        BigDecimal z = BigDecimal.ZERO.setScale(2);
        return new AccountRecord(id, "Y", new BigDecimal(currBal), z, z, "", "", "", z, z, "", "", "").toFixedWidth();
    }

    static String trnx(String card, String id, String desc, String amount) {
        return new TrnxRecord(card, id, "01", 1, "POS TERM", desc, new BigDecimal(amount), 0L, "", "", "", "", "", "")
                .toFixedWidth();
    }

    /** CREASTMT STEP010 OUTREC FIELDS=(1:263,16,17:1,262,279:279,50) applied to a TRANSACT record. */
    static String reshapeDailyTran(String line) {
        String r = String.format("%-350s", line);
        return r.substring(262, 278) + r.substring(0, 262) + r.substring(278, 328);
    }

    static List<String> lines(Path path) {
        try (Stream<String> s = Files.lines(path, StandardCharsets.ISO_8859_1)) {
            return s.toList();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    static List<String> golden(String name) {
        return lines(Path.of("src/test/resources/golden", name));
    }

    /** Result of one in-memory run. */
    record Run(List<String> stmt, List<String> html, List<String> display, Cbstm03aAbendException abend,
            InMemoryRecordWriter stmtWriter, InMemoryRecordWriter htmlWriter) {
    }

    static Run run(Cbstm03bCall files) {
        InMemoryRecordWriter stmt = new InMemoryRecordWriter(80);
        InMemoryRecordWriter html = new InMemoryRecordWriter(100);
        List<String> display = new ArrayList<>();
        JobContext job = new JobContext("CREASTMT", "STEP040", List.of());
        Cbstm03aAbendException abend = null;
        try {
            new Cbstm03a(job, files, stmt, html, display::add).run();
        } catch (Cbstm03aAbendException e) {
            abend = e;
        }
        return new Run(stmt.records(), html.records(), display, abend, stmt, html);
    }

    static Run run(List<String> trnx, List<String> xref, List<String> cust, List<String> acct) {
        return run(Cbstm03bFiles.inMemory(trnx, xref, cust, acct));
    }

    static String rtrim(String s) {
        return s.replaceAll("\\s+$", "");
    }
}
