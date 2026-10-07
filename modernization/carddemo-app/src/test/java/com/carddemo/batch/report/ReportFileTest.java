package com.carddemo.batch.report;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.BatchOutputFile;
import com.carddemo.common.codec.RecordEncoding;
import java.time.LocalDate;
import org.junit.jupiter.api.Test;

class ReportFileTest {

    private static ReportFile file(RecordEncoding encoding, String... records) {
        StringBuilder text = new StringBuilder();
        for (String record : records) {
            text.append(String.format("%-133s", record));
        }
        return new ReportFile(new BatchOutputFile("TRANREPT", LocalDate.of(2022, 7, 6), 1L, "/out/TRANREPT.1",
                records.length, "sha"), encoding.encode(text.toString()), encoding);
    }

    @Test
    void fixedRecordGenerationsSplitByRecordLengthInEitherEncoding() {
        for (RecordEncoding encoding : new RecordEncoding[] {RecordEncoding.EBCDIC, RecordEncoding.ASCII}) {
            assertThat(file(encoding, "HEADER", "", "DETAIL 1").lines()).containsExactly("HEADER", "", "DETAIL 1");
        }
    }

    @Test
    void anEmptyGenerationHasNoLines() {
        assertThat(file(RecordEncoding.ASCII).lines()).isEmpty();
    }
}
