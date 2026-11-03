package com.carddemo.common.record;

import com.carddemo.common.record.layout.DalyrejsRecordLayout;
import com.carddemo.common.record.layout.DalytranRecordLayout;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class DalyrejsRecordLayoutTest {

    @Test
    void rejectIsTheDalytranRecordPlusTheEightyByteTrailer() throws Exception {
        List<CobolRecord> daily = RecordReader.readAll(Path.of("../../app/data/ASCII/dailytran.txt"),
                DalytranRecordLayout.LAYOUT, CodePage.ASCII, RecordFormat.LINE_SEQUENTIAL);

        CobolRecord reject = DalyrejsRecordLayout.of(daily.get(0), 102, "OVERLIMIT TRANSACTION");

        assertThat(DalyrejsRecordLayout.LAYOUT.length()).isEqualTo(430);
        byte[] bytes = reject.toBytes();
        assertThat(new String(bytes, 350, 80, StandardCharsets.ISO_8859_1))
                .isEqualTo("0102" + String.format("%-76s", "OVERLIMIT TRANSACTION"));
        assertThat(reject.getInt(DalyrejsRecordLayout.WS_VALIDATION_FAIL_REASON)).isEqualTo(102);
        assertThat(DalyrejsRecordLayout.dalytran(reject)).isEqualTo(daily.get(0));
    }

    @Test
    void ebcdicRejectRoundTrips() {
        CobolRecord daily = CobolRecord.initialized(DalytranRecordLayout.LAYOUT, CodePage.EBCDIC)
                .set(DalytranRecordLayout.DALYTRAN_ID, "0000000000000001");

        CobolRecord reject = DalyrejsRecordLayout.of(daily, 100, "INVALID CARD NUMBER FOUND");
        CobolRecord read = CobolRecord.of(DalyrejsRecordLayout.LAYOUT, CodePage.EBCDIC, reject.toBytes());

        assertThat(read.getTrimmed(DalyrejsRecordLayout.WS_VALIDATION_FAIL_REASON_DESC))
                .isEqualTo("INVALID CARD NUMBER FOUND");
        assertThat(DalyrejsRecordLayout.dalytran(read).get(DalytranRecordLayout.DALYTRAN_ID))
                .isEqualTo("0000000000000001");
    }
}
