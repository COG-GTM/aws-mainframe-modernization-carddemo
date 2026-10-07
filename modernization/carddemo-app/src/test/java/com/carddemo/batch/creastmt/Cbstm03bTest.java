package com.carddemo.batch.creastmt;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.batch.creastmt.Cbstm03b.Operation;
import com.carddemo.batch.creastmt.Cbstm03b.Response;
import com.carddemo.common.codec.FixedWidthRecord;
import java.util.List;
import org.junit.jupiter.api.Test;

/** CBSTM03B's operation codes per DD, with the file status returned in LK-M03B-RC. */
class Cbstm03bTest {

    static List<FixedWidthRecord> trxfl() {
        return CreastmtJobConfiguration.trxfl(CreastmtBaselineTest.transact(), CreastmtTestSupport.ASCII);
    }

    @Test
    void sequentialFilesOpenReadToEndOfFileAndClose() {
        Cbstm03b files = CreastmtTestSupport.files(Cbstm03bTest::trxfl, CreastmtTestSupport.XREFDATA);
        assertThat(files.call(Cbstm03b.TRNXFILE, Operation.OPEN).returnCode()).isEqualTo("00");
        int read = 0;
        Response response;
        while ((response = files.call(Cbstm03b.TRNXFILE, Operation.READ)).is("00")) {
            assertThat(response.record()).isPresent();
            read++;
        }
        assertThat(read).isEqualTo(312);
        assertThat(response.returnCode()).isEqualTo("10");
        assertThat(response.record()).isEmpty();
        assertThat(files.call(Cbstm03b.TRNXFILE, Operation.CLOSE).returnCode()).isEqualTo("00");

        assertThat(files.call(Cbstm03b.XREFFILE, Operation.OPEN).returnCode()).isEqualTo("00");
        assertThat(files.call(Cbstm03b.XREFFILE, Operation.READ).record().orElseThrow().text())
                .startsWith("0500024453765740");
    }

    @Test
    void keyedFilesReadByTheLeadingKeyLengthBytes() {
        Cbstm03b files = CreastmtTestSupport.files(Cbstm03bTest::trxfl, CreastmtTestSupport.XREFDATA);
        assertThat(files.call(Cbstm03b.CUSTFILE, Operation.OPEN).returnCode()).isEqualTo("00");
        Response customer = files.call(Cbstm03b.CUSTFILE, Operation.READ_KEY, "000000050", 9);
        assertThat(customer.returnCode()).isEqualTo("00");
        assertThat(customer.record().orElseThrow().text()).startsWith("000000050");
        assertThat(files.call(Cbstm03b.CUSTFILE, Operation.READ_KEY, "000000999", 9).returnCode()).isEqualTo("23");

        assertThat(files.call(Cbstm03b.ACCTFILE, Operation.OPEN).returnCode()).isEqualTo("00");
        assertThat(files.call(Cbstm03b.ACCTFILE, Operation.READ_KEY, "00000000050", 11).record().orElseThrow()
                .text()).startsWith("00000000050");
        assertThat(files.call(Cbstm03b.ACCTFILE, Operation.CLOSE).returnCode()).isEqualTo("00");
    }

    @Test
    void operationsTheDdDoesNotHandleLeaveTheLastStatus() {
        Cbstm03b files = CreastmtTestSupport.files(Cbstm03bTest::trxfl, CreastmtTestSupport.XREFDATA);
        assertThat(files.call(Cbstm03b.TRNXFILE, Operation.WRITE).returnCode()).isEqualTo("  ");
        files.call(Cbstm03b.TRNXFILE, Operation.OPEN);
        assertThat(files.call(Cbstm03b.TRNXFILE, Operation.READ_KEY, "x", 1).returnCode()).isEqualTo("00");
        assertThat(files.call(Cbstm03b.CUSTFILE, Operation.READ).returnCode()).isEqualTo("  ");
        assertThat(Operation.of('Z')).isEqualTo(Operation.REWRITE);
        assertThatThrownBy(() -> Operation.of('X')).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> files.call("STMTFILE", Operation.OPEN)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void readingAFileThatIsNotOpenReturnsTheHarnessStatus() {
        // CBSTM03A always opens first; the harness reports 47 (sequential) / 42 (keyed) for a read before OPEN
        Cbstm03b files = CreastmtTestSupport.files(Cbstm03bTest::trxfl, CreastmtTestSupport.XREFDATA);
        assertThat(files.call(Cbstm03b.XREFFILE, Operation.READ).returnCode()).isEqualTo("47");
        assertThat(files.call(Cbstm03b.ACCTFILE, Operation.READ_KEY, "00000000050", 11).returnCode())
                .isEqualTo("42");
    }
}
