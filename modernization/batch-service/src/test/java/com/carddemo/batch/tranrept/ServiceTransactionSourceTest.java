package com.carddemo.batch.tranrept;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.layout.TranRecordLayout;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;

/** The REPROC unload writes the timestamps the TRANSACT KSDS holds after POSTTRAN and INTCALC. */
class ServiceTransactionSourceTest {

    private static final LocalDateTime ORIG = LocalDateTime.of(2022, 6, 10, 19, 27, 53, 123_456_000);
    private static final LocalDateTime PROC = LocalDateTime.of(2022, 7, 18, 0, 0, 0);

    @Test
    void postedTransactionKeepsTheDalytranOrigTimestampAndADb2ProcTimestamp() {
        CobolRecord record = ServiceTransactionSource.toRecord(view("01", 1, "POS TERM", "Purchase at Store"),
                CodePage.ASCII);

        assertThat(record.get(TranRecordLayout.TRAN_ORIG_TS)).isEqualTo("2022-06-10 19:27:53.123456");
        assertThat(record.get(TranRecordLayout.TRAN_PROC_TS)).isEqualTo("2022-07-18-00.00.00.000000");
    }

    @Test
    void interestTransactionCarriesDb2TimestampsInBoth() {
        CobolRecord record = ServiceTransactionSource.toRecord(view("01", 5, "System", "Int. for a/c 00000000001"),
                CodePage.ASCII);

        assertThat(record.get(TranRecordLayout.TRAN_ORIG_TS)).isEqualTo("2022-06-10-19.27.53.123456");
        assertThat(record.get(TranRecordLayout.TRAN_PROC_TS)).isEqualTo("2022-07-18-00.00.00.000000");
    }

    private static ServiceTransactionSource.TransactionView view(String type, int category, String source,
                                                                 String description) {
        return new ServiceTransactionSource.TransactionView("0000000000000001", "4859452612877065", type, category,
                source, description, new BigDecimal("10.00"), "1", "Merchant", "City", "12345", ORIG, PROC);
    }
}
