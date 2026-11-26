package com.carddemo.programs.cbcus01c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class KsdsCustomerFileTest {

    @TempDir
    Path dir;

    @Test
    void returnsRecordsInKeyOrderThenEndOfFile() throws IOException {
        Path f = dir.resolve("cust.txt");
        Files.writeString(f, Fixtures.customer("000000002", "B") + "\n" + Fixtures.customer("000000001", "A") + "\n");
        KsdsCustomerFile file = new KsdsCustomerFile(f);

        assertEquals("00", file.open());
        assertEquals("000000001", file.readNext().record().fdCustId());
        assertEquals("000000002", file.readNext().record().fdCustId());
        CustomerFile.ReadResult eof = file.readNext();
        assertEquals("10", eof.status());
        assertNull(eof.record());
        assertEquals("00", file.close());
    }

    @Test
    void statusCodesForMisuse() {
        KsdsCustomerFile file = new KsdsCustomerFile(dir.resolve("missing.txt"));
        assertEquals("47", file.readNext().status());
        assertEquals("42", file.close());
        assertEquals("35", file.open());
    }

    @Test
    void inMemoryReopenRestartsFromFirstRecordLikeFileBackend() {
        InMemoryCustomerFile file = new InMemoryCustomerFile(java.util.List.of(Fixtures.customer("000000001", "A")));
        assertEquals("00", file.open());
        assertEquals("000000001", file.readNext().record().fdCustId());
        assertEquals("41", file.open());
        assertEquals("10", file.readNext().status());
        assertEquals("00", file.close());
        assertEquals("00", file.open());
        assertEquals("000000001", file.readNext().record().fdCustId());
    }

    @Test
    void fixedBlockFileWithPartialRecordFailsOpenWith39() throws IOException {
        Path f = dir.resolve("cust.ps");
        Files.write(f, new byte[501]);
        assertEquals("39", KsdsCustomerFile.fixedBlock(f, Cbcus01c.EBCDIC).open());
    }

    @Test
    void undecodableBytesFailOpenInsteadOfBeingReplaced() throws IOException {
        Path f = dir.resolve("cust.ps");
        byte[] bytes = new byte[500];
        java.util.Arrays.fill(bytes, (byte) 0xF1);
        Files.write(f, bytes);
        assertEquals("30", KsdsCustomerFile.fixedBlock(f, java.nio.charset.StandardCharsets.US_ASCII).open());
        assertEquals("00", KsdsCustomerFile.fixedBlock(f, Cbcus01c.EBCDIC).open());
    }

    @Test
    void multiByteCharsetIsRejected() {
        org.junit.jupiter.api.Assertions.assertThrows(IllegalArgumentException.class,
                () -> new KsdsCustomerFile(dir.resolve("x"), java.nio.charset.StandardCharsets.UTF_8));
    }

    @Test
    void fdRecordSplitsKeyAndData() {
        FdCustfileRec rec = FdCustfileRec.fromFixedWidth("000000009REST");
        assertEquals("000000009", rec.fdCustId());
        assertEquals(491, rec.fdCustData().length());
        assertEquals(500, rec.toFixedWidth().length());
    }

    @Test
    void inMemoryMisuseStatusesMatchFileBackend() {
        InMemoryCustomerFile file = new InMemoryCustomerFile(java.util.List.of(Fixtures.customer("000000001", "A")));
        assertEquals("47", file.readNext().status());
        assertEquals("42", file.close());
        assertEquals("00", file.open());
        assertEquals("00", file.close());
        assertEquals("47", file.readNext().status());
        assertEquals("42", file.close());
    }
}
