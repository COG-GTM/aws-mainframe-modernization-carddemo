package com.carddemo.programs.copaus1c;

import com.carddemo.programs.copaus1c.copybook.AidKey;
import com.carddemo.programs.copaus1c.copybook.AuthorizationKey;
import com.carddemo.programs.copaus1c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus1c.copybook.PendingAuthSummary;
import com.carddemo.programs.copaus1c.inmemory.ImsUnloadReader;
import com.carddemo.programs.copaus1c.inmemory.InMemoryPendingAuthDb;
import com.carddemo.programs.copaus1c.inmemory.RecordingFraudLink;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Drives COPAUS1C against the sample IMS unload AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat (EBCDIC). */
class FixtureDataTest {

    private static final AuthorizationKey FIRST = AuthorizationKey.of(76699, 998747444L);
    private static final AuthorizationKey SECOND = AuthorizationKey.of(76699, 998748388L);

    private InMemoryPendingAuthDb db;
    private byte[] raw;

    @BeforeEach
    void load() throws IOException {
        try (InputStream in = getClass().getResourceAsStream("/fixtures/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat")) {
            assertNotNull(in);
            raw = in.readAllBytes();
        }
        db = ImsUnloadReader.load(raw);
    }

    @Test
    void unloadDecodesRootsAndChildren() {
        List<PendingAuthSummary> roots = List.copyOf(db.summaries());
        int children = roots.stream().mapToInt(r -> db.details(r.getAcctId()).size()).sum();
        assertEquals(21, roots.size());
        assertTrue(roots.stream().allMatch(r -> r.getAcctId() < 100));
        assertEquals(202, children);
        PendingAuthSummary first = roots.get(0);
        assertEquals(1L, first.getAcctId());
        assertEquals(1L, first.getCustId());
        List<PendingAuthDetails> d = db.details(1L);
        assertEquals(6, d.size());
        assertEquals(FIRST, d.get(0).getAuthorizationKey());
        assertEquals("231027", d.get(0).getAuthOrigDate());
        assertEquals("041252", d.get(0).getAuthOrigTime());
        assertEquals("9680294154603697", d.get(0).getCardNum());
        assertEquals("00", d.get(0).getAuthRespCode());
        assertEquals(new BigDecimal("1.24"), d.get(0).getApprovedAmt());
        assertEquals('P', d.get(0).getMatchStatus());
        assertEquals("000000", d.get(0).getProcessingCodeDisplay());
    }

    @Test
    void everyValidSegmentRoundTripsByteForByteAndInvalidRootKeyIsDetected() {
        int roundTripped = 0;
        int invalidRoots = 0;
        for (String nameText : List.of("PAUTSUM0", "PAUTDTL1")) {
            byte[] name = nameText.getBytes(com.carddemo.programs.copaus1c.copybook.Cobol.EBCDIC);
            int len = nameText.equals("PAUTSUM0") ? PendingAuthSummary.LENGTH : PendingAuthDetails.LENGTH;
            for (int at = indexOf(raw, name, 2); at >= 0; at = indexOf(raw, name, at + 8)) {
                int declared = ((raw[at - 2] & 0xFF) << 8) | (raw[at - 1] & 0xFF);
                if (declared != len) {
                    continue;
                }
                byte[] seg = java.util.Arrays.copyOfRange(raw, at + 29, at + 29 + len);
                if (len == PendingAuthSummary.LENGTH) {
                    if (!com.carddemo.programs.copaus1c.copybook.Cobol.isValidPacked(seg, 0, 6)) {
                        invalidRoots++;
                        continue;
                    }
                    assertArrayEquals(seg, PendingAuthSummary.fromEbcdic(seg).toEbcdic());
                } else {
                    assertArrayEquals(seg, PendingAuthDetails.fromEbcdic(seg).toEbcdic());
                }
                roundTripped++;
            }
        }
        assertEquals(223, roundTripped);
        assertEquals(1, invalidRoots);
    }

    @Test
    void truncatedUnloadIsRejected() {
        byte[] cut = java.util.Arrays.copyOf(raw, raw.length - 150);
        IllegalArgumentException e = org.junit.jupiter.api.Assertions.assertThrows(IllegalArgumentException.class,
                () -> ImsUnloadReader.load(cut));
        assertTrue(e.getMessage().startsWith("Truncated IMS unload"));

        // cut through the last real PAUTDTL1 (account 48), not just the trailing dummy root
        byte[] name = "PAUTDTL1".getBytes(com.carddemo.programs.copaus1c.copybook.Cobol.EBCDIC);
        int lastDetail = -1;
        for (int at = indexOf(raw, name, 2); at >= 0; at = indexOf(raw, name, at + 8)) {
            if ((((raw[at - 2] & 0xFF) << 8) | (raw[at - 1] & 0xFF)) == PendingAuthDetails.LENGTH) {
                lastDetail = at;
            }
        }
        byte[] midDetail = java.util.Arrays.copyOf(raw, lastDetail + 29 + 100);
        IllegalArgumentException e2 = org.junit.jupiter.api.Assertions.assertThrows(IllegalArgumentException.class,
                () -> ImsUnloadReader.load(midDetail));
        assertTrue(e2.getMessage().startsWith("Truncated IMS unload: segment at offset " + lastDetail));
    }

    @Test
    void segmentImagesRoundTripByteForByte() {
        byte[] name = "PAUTDTL1".getBytes(com.carddemo.programs.copaus1c.copybook.Cobol.EBCDIC);
        int at = indexOf(raw, name, 200);
        byte[] seg = java.util.Arrays.copyOfRange(raw, at + 29, at + 29 + PendingAuthDetails.LENGTH);
        assertArrayEquals(seg, PendingAuthDetails.fromEbcdic(seg).toEbcdic());
        byte[] sname = "PAUTSUM0".getBytes(com.carddemo.programs.copaus1c.copybook.Cobol.EBCDIC);
        int sat = indexOf(raw, sname, 50);
        byte[] sseg = java.util.Arrays.copyOfRange(raw, sat + 29, sat + 29 + PendingAuthSummary.LENGTH);
        assertArrayEquals(sseg, PendingAuthSummary.fromEbcdic(sseg).toEbcdic());
    }

    @Test
    void viewAndPageThroughSampleAccount() {
        Copaus1cService service = new Copaus1cService(db, db, RecordingFraudLink.succeeding("11/26/26"),
                Fixtures.CLOCK);
        Copaus1cCommarea c = Fixtures.commarea(FIRST, false);
        AuthViewResult r = service.execute(new AuthViewRequest(c, AidKey.DFHENTER));
        AuthViewScreen s = r.lastScreen();
        assertEquals("9680294154603697", s.cardnum());
        assertEquals("10/27/23  ", s.authdt());
        assertEquals("04:12:52  ", s.authtm());
        assertEquals("        1.24", s.authamt());
        assertEquals("A", s.authrsp());
        assertEquals("0000-APPROVED       ", s.authrsn());
        assertEquals("000000", s.authcd());

        r = service.execute(new AuthViewRequest(r.commarea(), AidKey.DFHPF8));
        assertEquals(SECOND, r.commarea().cpvd().getPauSelected());
        assertEquals("04:12:51  ", r.lastScreen().authtm());

        r = service.execute(new AuthViewRequest(r.commarea(), AidKey.DFHPF5));
        assertEquals("F-11/26/26", r.lastScreen().authfrd());
        assertEquals('F', db.findDetail(1L, SECOND).getAuthFraud());
    }

    private static int indexOf(byte[] data, byte[] needle, int from) {
        outer:
        for (int i = from; i <= data.length - needle.length; i++) {
            for (int k = 0; k < needle.length; k++) {
                if (data[i + k] != needle[k]) {
                    continue outer;
                }
            }
            return i;
        }
        return -1;
    }
}
