package com.carddemo.batch.tranrept;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordWriter;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.TranCatRecordLayout;
import com.carddemo.common.record.layout.TranTypeRecordLayout;
import java.io.IOException;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * CBTRN03C alone, on the TRANFILE of each case as given (the 'program' cases feed records the SORT
 * step would have dropped or reordered), compared byte for byte with the GnuCOBOL run of the same inputs.
 */
class TransactionReportProgramTest {

    private static final Map<String, String> ABEND_MESSAGES = Map.of(
            "abend-xref", "INVALID CARD NUMBER : 9999999999999999",
            "abend-trantype", "INVALID TRANSACTION TYPE : 99",
            "abend-trancatg", "INVALID TRAN CATG KEY : 010042");

    static Stream<TranreptCases> programCases() throws IOException {
        return TranreptCases.all().filter(TranreptCases::programMode);
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("programCases")
    void matchesTheCobolReport(TranreptCases c) {
        FileReferenceData reference = new FileReferenceData(
                DdFiles.read(c.reference("CARDXREF"), CardXrefRecordLayout.LAYOUT, CodePage.ASCII),
                DdFiles.read(c.reference("TRANTYPE"), TranTypeRecordLayout.LAYOUT, CodePage.ASCII),
                DdFiles.read(c.reference("TRANCATG"), TranCatRecordLayout.LAYOUT, CodePage.ASCII));
        if (c.expectedReturnCode() != 0) {
            assertThat(c.expectedReturnCode()).isEqualTo(16);
            assertThatThrownBy(() -> TransactionReportProgram.run(c.dateParmRecord(), c.transactions(), reference,
                    CodePage.ASCII))
                    .isInstanceOf(TransactionReportAbend.class)
                    .hasMessage(ABEND_MESSAGES.get(c.name()));
            return;
        }
        TransactionReportProgram.Result result = TransactionReportProgram.run(c.dateParmRecord(), c.transactions(),
                reference, CodePage.ASCII);
        byte[] actual = RecordWriter.toBytes(TransactionReportProgram.records(result.lines(), CodePage.ASCII),
                RecordFormat.FIXED);
        assertThat(new String(actual, java.nio.charset.StandardCharsets.ISO_8859_1))
                .isEqualTo(new String(c.expectedReport().orElseThrow(), java.nio.charset.StandardCharsets.ISO_8859_1));
    }
}
