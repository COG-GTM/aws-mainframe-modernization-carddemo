package com.carddemo.programs.cbstm03b;

import com.carddemo.programs.cbstm03b.copybook.FdAcctFileRec;
import com.carddemo.programs.cbstm03b.copybook.FdCustFileRec;
import com.carddemo.programs.cbstm03b.copybook.FdTrnxFileRec;
import com.carddemo.programs.cbstm03b.copybook.FdXrefFileRec;
import com.carddemo.programs.cbstm03b.copybook.LkM03bArea;
import com.carddemo.programs.cbstm03b.copybook.M03bOper;
import com.carddemo.programs.cbstm03b.io.EbcdicCollation;
import com.carddemo.programs.cbstm03b.io.FixedWidthRecordReader;
import com.carddemo.programs.cbstm03b.io.FixedWidthRecordWriter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Drives CBSTM03B the way CBSTM03A does in CREASTMT STEP040, against app/data/ASCII.
 * TRNXFILE is built from dailytran.txt with the STEP010 SORT/OUTREC of CREASTMT.JCL.
 */
class Cbstm03bFixtureTest {

    static final Path DATA = Path.of("../../../app/data/ASCII");

    @TempDir
    Path tmp;

    Cbstm03b pgm;
    List<String> trxfl;

    /** SORT FIELDS=(263,16,CH,A,1,16,CH,A); OUTREC FIELDS=(1:263,16,17:1,262,279:279,50); LRECL=350. */
    static List<String> creastmtStep010(List<String> transact) {
        List<String> sorted = new ArrayList<>(transact);
        sorted.sort(Comparator.comparing((String r) -> r.substring(262, 278), EbcdicCollation.ORDER)
                .thenComparing(r -> r.substring(0, 16), EbcdicCollation.ORDER));
        List<String> out = new ArrayList<>();
        for (String r : sorted) {
            String rec = r.substring(262, 278) + r.substring(0, 262) + r.substring(278, 328);
            out.add(rec + " ".repeat(350 - rec.length()));
        }
        return out;
    }

    @BeforeEach
    void setUp() throws Exception {
        assertTrue(Files.isDirectory(DATA), "sample data not found at " + DATA.toAbsolutePath());
        trxfl = creastmtStep010(new FixedWidthRecordReader(350).readAll(DATA.resolve("dailytran.txt")));
        Path trxflFile = tmp.resolve("TRXFL.txt");
        new FixedWidthRecordWriter(350).writeAll(trxflFile, trxfl);
        pgm = Cbstm03b.fromDatasets(trxflFile, DATA.resolve("cardxref.txt"),
                DATA.resolve("custdata.txt"), DATA.resolve("acctdata.txt"));
    }

    LkM03bArea call(String dd, M03bOper op) {
        LkM03bArea lk = new LkM03bArea().dd(dd).set(op).rc("00");
        pgm.call(lk);
        return lk;
    }

    @Test
    void statementJobFlowReadsEveryXrefWithItsCustomerAndAccount() {
        for (String dd : List.of("TRNXFILE", "XREFFILE", "CUSTFILE", "ACCTFILE")) {
            assertEquals("00", call(dd, M03bOper.OPEN).rc(), dd);
        }

        int xrefs = 0;
        String previousCard = "";
        while (true) {
            LkM03bArea x = new LkM03bArea().dd("XREFFILE").set(M03bOper.READ).rc("00").fldt("");
            pgm.call(x);
            if (x.rc().equals("10")) {
                break;
            }
            assertEquals("00", x.rc());
            xrefs++;
            FdXrefFileRec xref = FdXrefFileRec.parse(x.fldt().substring(0, FdXrefFileRec.RECORD_LENGTH));
            assertTrue(EbcdicCollation.compare(xref.fdXrefCardNum(), previousCard) > 0, "ascending card order");
            previousCard = xref.fdXrefCardNum();
            String custId = xref.fdXrefData().substring(0, 9);   // XREF-CUST-ID PIC 9(09)
            String acctId = xref.fdXrefData().substring(9, 20);  // XREF-ACCT-ID PIC 9(11)

            LkM03bArea c = new LkM03bArea().dd("CUSTFILE").set(M03bOper.READ_K).key(custId).keyLn(9).rc("00").fldt("");
            pgm.call(c);
            assertEquals("00", c.rc(), "customer " + custId);
            assertEquals(custId, FdCustFileRec.parse(c.fldt().substring(0, 500)).fdCustId());

            LkM03bArea a = new LkM03bArea().dd("ACCTFILE").set(M03bOper.READ_K).key(acctId).keyLn(11).rc("00").fldt("");
            pgm.call(a);
            assertEquals("00", a.rc(), "account " + acctId);
            assertEquals(acctId, FdAcctFileRec.parse(a.fldt().substring(0, 300)).fdAcctId());
            assertEquals(" ".repeat(700), a.fldt().substring(300));
        }
        assertEquals(50, xrefs);

        int trnx = 0;
        String previousKey = "";
        LkM03bArea t;
        while ((t = call("TRNXFILE", M03bOper.READ)).rc().equals("00")) {
            FdTrnxFileRec rec = FdTrnxFileRec.parse(t.fldt().substring(0, 350));
            assertTrue(EbcdicCollation.compare(rec.fdTrnxsId(), previousKey) > 0, "ascending FD-TRNXS-ID order");
            previousKey = rec.fdTrnxsId();
            assertEquals(trxfl.get(trnx), rec.toRecord());
            trnx++;
        }
        assertEquals("10", t.rc());
        assertEquals(300, trnx);

        for (String dd : List.of("TRNXFILE", "XREFFILE", "CUSTFILE", "ACCTFILE")) {
            assertEquals("00", call(dd, M03bOper.CLOSE).rc(), dd);
        }
    }

    @Test
    void firstSampleCustomerAndAccountByKey() {
        call("CUSTFILE", M03bOper.OPEN);
        call("ACCTFILE", M03bOper.OPEN);
        LkM03bArea c = new LkM03bArea().dd("CUSTFILE").set(M03bOper.READ_K).key("000000001").keyLn(9);
        pgm.call(c);
        assertEquals("00", c.rc());
        assertTrue(c.fldt().startsWith("000000001"));

        LkM03bArea a = new LkM03bArea().dd("ACCTFILE").set(M03bOper.READ_K).key("1").keyLn(1);
        pgm.call(a);
        assertEquals("00", a.rc());
        assertTrue(a.fldt().startsWith("00000000001"));

        LkM03bArea missing = new LkM03bArea().dd("CUSTFILE").set(M03bOper.READ_K).key("999999999").keyLn(9);
        pgm.call(missing);
        assertEquals("23", missing.rc());
    }

    /**
     * Exact CBSTM03A call order with its single WS-M03B-AREA: 8100-TRNXFILE-OPEN (OPEN + first READ),
     * 8500-READTRNX-READ until '10', 8200/8300/8400 OPENs, 1000-MAINLINE, 9100-9400 CLOSEs.
     * CBSTM03A accepts '00' or '04' from OPEN/CLOSE and abends otherwise.
     */
    @Test
    void cbstm03aCallSequenceWithOneSharedArea() {
        LkM03bArea ws = new LkM03bArea();

        // 8100-TRNXFILE-OPEN
        ws.dd("TRNXFILE").set(M03bOper.OPEN).rc("00");
        pgm.call(ws);
        assertOpenOk(ws);
        ws.set(M03bOper.READ).fldt("");
        pgm.call(ws);
        assertOpenOk(ws);
        List<String> trnx = new ArrayList<>();
        trnx.add(ws.fldt().substring(0, 350));

        // 8500-READTRNX-READ
        while (true) {
            ws.dd("TRNXFILE").set(M03bOper.READ).fldt("");
            pgm.call(ws);
            if (ws.rc().equals("10")) {
                break;
            }
            assertEquals("00", ws.rc());
            trnx.add(ws.fldt().substring(0, 350));
        }
        assertEquals(trxfl, trnx, "all 300 TRXFL records in key order, first one read by 8100");
        assertEquals(" ".repeat(1000), ws.fldt(), "FLDT stays as moved SPACES after the '10'");

        // 8200-XREFFILE-OPEN, 8300-CUSTFILE-OPEN, 8400-ACCTFILE-OPEN
        for (String dd : List.of("XREFFILE", "CUSTFILE", "ACCTFILE")) {
            ws.dd(dd).set(M03bOper.OPEN).rc("00");
            pgm.call(ws);
            assertOpenOk(ws);
        }

        // 1000-MAINLINE: 1000-XREFFILE-GET-NEXT / 2000-CUSTFILE-GET / 3000-ACCTFILE-GET
        int statements = 0;
        while (true) {
            ws.dd("XREFFILE").set(M03bOper.READ).rc("00").fldt("");
            pgm.call(ws);
            if (ws.rc().equals("10")) {
                break;
            }
            assertEquals("00", ws.rc());
            String xref = ws.fldt().substring(0, 50);

            ws.dd("CUSTFILE").set(M03bOper.READ_K).key(xref.substring(16, 25)).keyLn(9).rc("00").fldt("");
            pgm.call(ws);
            assertEquals("00", ws.rc());
            assertEquals(xref.substring(16, 25), ws.fldt().substring(0, 9));

            ws.dd("ACCTFILE").set(M03bOper.READ_K).key(xref.substring(25, 36)).keyLn(11).rc("00").fldt("");
            pgm.call(ws);
            assertEquals("00", ws.rc());
            assertEquals(xref.substring(25, 36), ws.fldt().substring(0, 11));
            statements++;
        }
        assertEquals(50, statements);

        // 9100-TRNXFILE-CLOSE .. 9400-ACCTFILE-CLOSE
        for (String dd : List.of("TRNXFILE", "XREFFILE", "CUSTFILE", "ACCTFILE")) {
            ws.dd(dd).set(M03bOper.CLOSE).rc("00");
            pgm.call(ws);
            assertOpenOk(ws);
        }
    }

    private static void assertOpenOk(LkM03bArea ws) {
        assertTrue(ws.rc().equals("00") || ws.rc().equals("04"), ws.dd() + " RETURN CODE: " + ws.rc());
    }
}
