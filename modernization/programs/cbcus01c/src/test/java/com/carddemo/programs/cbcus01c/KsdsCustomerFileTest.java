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
        assertEquals("10", file.readNext().status());
        assertEquals("00", file.close());
        assertEquals("00", file.open());
        assertEquals("000000001", file.readNext().record().fdCustId());
    }

    @Test
    void fdRecordSplitsKeyAndData() {
        FdCustfileRec rec = FdCustfileRec.fromFixedWidth("000000009REST");
        assertEquals("000000009", rec.fdCustId());
        assertEquals(491, rec.fdCustData().length());
        assertEquals(500, rec.toFixedWidth().length());
    }
}
