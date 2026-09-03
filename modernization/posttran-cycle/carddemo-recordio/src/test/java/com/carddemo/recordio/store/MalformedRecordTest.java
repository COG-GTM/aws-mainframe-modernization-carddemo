package com.carddemo.recordio.store;

import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.codec.RecordFormatException;
import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.AccountLayout;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionLayout;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * Malformed input. On the mainframe a short block is a file-status 04/39 open or read failure and
 * non-numeric bytes in a PIC 9 field are silently mis-read; here both are surfaced as exceptions.
 */
class MalformedRecordTest {

    @Test
    void fileLengthNotAMultipleOfLrecl() throws IOException {
        Path p = Files.createTempFile("short", ".dat");
        Files.write(p, new byte[Transaction.LENGTH + 7]);
        assertThatThrownBy(() -> FixedWidthFile.readAll(p, TransactionLayout.INSTANCE, RecordEncoding.EBCDIC))
                .isInstanceOf(RecordFormatException.class);
    }

    @Test
    void wrongRecordLengthIsRejected() {
        assertThatThrownBy(() -> FixedWidthRecord.of(new byte[Account.LENGTH - 1], Account.LENGTH, RecordEncoding.EBCDIC))
                .isInstanceOf(RecordFormatException.class);
    }

    @Test
    void nonNumericBytesInAZonedFieldAreRejected() {
        byte[] image = new byte[Transaction.LENGTH];
        Arrays.fill(image, (byte) 0x40);                     // EBCDIC spaces everywhere, incl. TRAN-AMT
        FixedWidthRecord r = new FixedWidthRecord(image, RecordEncoding.EBCDIC);
        r.setText(0, 16, "0000000000000001");
        assertThatThrownBy(() -> TransactionLayout.INSTANCE.decode(r)).isInstanceOf(RecordFormatException.class);
    }

    @Test
    void duplicatePrimaryKeyIsRejectedLikeVsamStatus22() {
        Account a = new Account("00000000001", "Y", java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO,
                "2020-01-01", "2030-01-01", "2025-01-01", java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO, "1", "");
        KeyedRecordStore<Account> store = KeyedRecordStore.of("ACCTFILE", List.of(a), AccountLayout.INSTANCE, RecordEncoding.EBCDIC, Account::accountId);
        assertThatThrownBy(() -> store.write(a)).isInstanceOf(DuplicateKeyException.class);
    }
}
