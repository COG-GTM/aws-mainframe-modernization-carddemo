package com.carddemo.programs.copaus2c;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.copaus2c.copybook.Ebcdic;
import com.carddemo.programs.copaus2c.copybook.FraudCommarea;
import com.carddemo.programs.copaus2c.copybook.FraudCommarea.FraudStatusRecord;
import com.carddemo.programs.copaus2c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus2c.gateway.InMemoryAuthFrdsGateway;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * Runs COPAUS2C over every PAUTDTL1 (CIPAUDTY) segment in the sample IMS unload
 * app/app-authorization-ims-db2-mq/data/EBCDIC/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat.
 * Records are variable length (2-byte big-endian length prefix); detail records carry the segment
 * name at offset 10 and the 200-byte segment at offset 39.
 */
class PendingAuthFixtureTest {

    private static final String FIXTURE = "/fixtures/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat";
    private static final List<byte[]> SEGMENTS = new ArrayList<>();

    @BeforeAll
    static void load() throws IOException {
        byte[] data;
        try (InputStream in = PendingAuthFixtureTest.class.getResourceAsStream(FIXTURE)) {
            data = in.readAllBytes();
        }
        int i = 0;
        while (i < data.length) {
            int len = ((data[i] & 0xFF) << 8) | (data[i + 1] & 0xFF);
            if ("PAUTDTL1".equals(Ebcdic.text(data, i + 10, 8))) {
                SEGMENTS.add(Arrays.copyOfRange(data, i + 39, i + 39 + PendingAuthDetails.LENGTH));
            }
            i += len;
        }
    }

    @Test
    void decodesAllDetailSegments() {
        assertEquals(202, SEGMENTS.size());
        PendingAuthDetails first = PendingAuthDetails.fromEbcdic(SEGMENTS.get(0), 0);
        assertEquals(76699, first.authDate9C());
        assertEquals(998_747_444L, first.authTime9C());
        assertEquals("231027", first.authOrigDate());
        assertEquals("9680294154603697", first.cardNum());
        assertEquals(new BigDecimal("1.24"), first.transactionAmt());
        assertEquals("Amazon.com            ", first.merchantName());
        assertTrue(first.matchStatusCode().isPresent());
    }

    @Test
    void segmentEbcdicRoundTripIsByteExact() {
        for (byte[] seg : SEGMENTS) {
            byte[] out = new byte[PendingAuthDetails.LENGTH];
            PendingAuthDetails.fromEbcdic(seg, 0).toEbcdic(out, 0);
            assertArrayEquals(seg, out);
        }
    }

    @Test
    void reportThenRemoveFraudForEverySampleAuthorization() {
        InMemoryAuthFrdsGateway db2 = new InMemoryAuthFrdsGateway(TestData.CLOCK);
        Copaus2cService service = new Copaus2cService(db2, TestData.CLOCK);

        for (byte[] seg : SEGMENTS) {
            FraudCommarea out = service.execute(new FraudCommarea(1, 2, PendingAuthDetails.fromEbcdic(seg, 0),
                    new FraudStatusRecord("F", " ", "")));
            assertEquals("S", out.fraudStatusRecord().frdUpdateStatus());
            assertEquals("ADD SUCCESS", out.fraudStatusRecord().frdActMsg().strip());
            assertEquals("11/26/26", out.fraudAuthRecord().fraudRptDate());
        }
        assertEquals(SEGMENTS.size(), db2.rows().size());
        assertTrue(db2.find("9680294154603697", LocalDateTime.of(2023, 10, 27, 0, 12, 52, 555_000_000)).isPresent());

        for (byte[] seg : SEGMENTS) {
            FraudCommarea out = service.execute(new FraudCommarea(1, 2, PendingAuthDetails.fromEbcdic(seg, 0),
                    new FraudStatusRecord("R", " ", "")));
            assertEquals("UPDT SUCCESS", out.fraudStatusRecord().frdActMsg().strip());
        }
        assertTrue(db2.rows().stream().allMatch(r -> "R".equals(r.authFraud())));
    }
}
