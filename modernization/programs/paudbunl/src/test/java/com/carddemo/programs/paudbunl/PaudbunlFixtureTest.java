package com.carddemo.programs.paudbunl;

import com.carddemo.programs.paudbunl.copybook.PendingAuthDetails;
import com.carddemo.programs.paudbunl.copybook.PendingAuthSummary;
import com.carddemo.programs.paudbunl.ims.ImsUnloadFile;
import com.carddemo.programs.paudbunl.ims.InMemoryPendingAuthDatabase;
import com.carddemo.programs.paudbunl.io.FixedWidthFileRecordReader;
import com.carddemo.programs.paudbunl.io.FixedWidthFileRecordWriter;
import com.carddemo.programs.paudbunl.support.CobolData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Runs PAUDBUNL against the CardDemo sample IMS unload of DBPAUTP0
 * (app/app-authorization-ims-db2-mq/data/EBCDIC/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat)
 * with RECFM=FB file writers, as JCL UNLDPADB does.
 */
class PaudbunlFixtureTest {

    private static final long[] EXPECTED_ACCTS = {1, 5, 7, 13, 15, 16, 17, 18, 23, 29, 30, 31, 32, 33, 34, 38, 42, 45, 46, 47, 48};
    private static final int[] EXPECTED_CHILDREN = {6, 1, 50, 58, 17, 11, 2, 5, 5, 1, 1, 1, 3, 2, 6, 2, 8, 2, 6, 2, 13};

    @TempDir
    Path tmp;

    private InMemoryPendingAuthDatabase db;
    private Path outfil1;
    private Path outfil2;
    private PaudbunlResult result;
    private final List<String> sysout = new ArrayList<>();

    @BeforeEach
    void runUnload() throws Exception {
        byte[] data;
        try (InputStream in = getClass().getResourceAsStream("/fixtures/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat")) {
            data = in.readAllBytes();
        }
        db = ImsUnloadFile.load(data);
        outfil1 = tmp.resolve("PAUTDB.ROOT.FILEO");
        outfil2 = tmp.resolve("PAUTDB.CHILD.FILEO");
        result = new Paudbunl(db, Paudbunl.pautbunlPcb(),
                new FixedWidthFileRecordWriter(outfil1, 100),
                new FixedWidthFileRecordWriter(outfil2, 206),
                Clock.fixed(Instant.parse("2026-01-05T00:00:00Z"), ZoneOffset.UTC),
                sysout::add, CobolData.EBCDIC).run();
    }

    @Test
    void fixtureLoadsAllSegments() {
        assertEquals(22, db.rootCount());
        assertEquals(202, db.childCount());
    }

    @Test
    void completesWithRc0AndExpectedFileSizes() throws Exception {
        assertEquals(0, result.returnCode());
        assertTrue(result.endOfAuthdb());
        assertEquals(224, result.wsNoSumryRead());
        assertEquals(224, result.wsAuthSmryProcCnt());
        // 22nd root has PA-ACCT-ID = spaces: IS NUMERIC fails, so it is not written
        assertEquals(21 * 100, Files.size(outfil1));
        assertEquals(202 * 206, Files.size(outfil2));
        assertEquals(21, sysout.stream().filter("CHILD SEG FLAG GE : Y"::equals).count());
        assertEquals("TODAYS DATE            :260105", sysout.get(2));
    }

    @Test
    void rootFileHoldsNumericAccountsInDatabaseOrder() throws Exception {
        List<byte[]> roots = readAll(outfil1, 100);
        assertEquals(EXPECTED_ACCTS.length, roots.size());
        for (int i = 0; i < roots.size(); i++) {
            assertEquals(BigDecimal.valueOf(EXPECTED_ACCTS[i]), CobolData.decodePacked(roots.get(i), 0, 6, 0));
        }
        PendingAuthSummary first = PendingAuthSummary.fromBytes(roots.get(0), CobolData.EBCDIC);
        assertEquals(1, first.custId());
        assertEquals(List.of("  ", "  ", "  ", "  ", "00"), first.accountStatus());
        assertEquals(new BigDecimal("2022.00"), first.creditLimit());
        assertEquals(new BigDecimal("1020.00"), first.cashLimit());
        assertEquals(new BigDecimal("9.44"), first.creditBalance());
        assertEquals(6, first.approvedAuthCnt());
        assertEquals(new BigDecimal("9.44"), first.approvedAuthAmt());
    }

    @Test
    void childFileCarriesParentKeyForEveryDetail() throws Exception {
        List<byte[]> children = readAll(outfil2, 206);
        int idx = 0;
        for (int r = 0; r < EXPECTED_ACCTS.length; r++) {
            for (int c = 0; c < EXPECTED_CHILDREN[r]; c++) {
                assertEquals(BigDecimal.valueOf(EXPECTED_ACCTS[r]), Opfil2Rec.rootSegKeyOf(children.get(idx)),
                        "child " + idx);
                idx++;
            }
        }
        assertEquals(children.size(), idx);
    }

    @Test
    void firstChildDecodesPerCipaudty() throws Exception {
        byte[] rec = readAll(outfil2, 206).get(0);
        PendingAuthDetails d = PendingAuthDetails.fromBytes(Opfil2Rec.childSegRecOf(rec), CobolData.EBCDIC);
        assertEquals(new BigDecimal("76699"), d.authDate9c());
        assertEquals(new BigDecimal("998747444"), d.authTime9c());
        assertEquals("231027", d.authOrigDate());
        assertEquals("041252", d.authOrigTime());
        assertEquals("0100", d.authType());
        assertTrue(d.isAuthApproved());
        assertEquals(new BigDecimal("1.24"), d.transactionAmt());
        assertEquals(new BigDecimal("1.24"), d.approvedAmt());
        assertEquals("5442", d.merchantCategoryCode());
        assertEquals("Amazon.com            ", d.merchantName());
        assertEquals("Wilmington   ", d.merchantCity());
        assertEquals("DE", d.merchantState());
        assertTrue(d.isMatchPending());
        assertFalse(d.isFraudConfirmed());
    }

    private static List<byte[]> readAll(Path p, int lrecl) throws Exception {
        List<byte[]> out = new ArrayList<>();
        try (FixedWidthFileRecordReader r = new FixedWidthFileRecordReader(p, lrecl)) {
            for (Optional<byte[]> rec = r.read(); rec.isPresent(); rec = r.read()) {
                out.add(rec.get());
            }
        }
        return out;
    }
}
