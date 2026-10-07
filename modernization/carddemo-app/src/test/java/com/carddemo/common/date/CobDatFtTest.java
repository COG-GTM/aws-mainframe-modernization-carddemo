package com.carddemo.common.date;

import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class CobDatFtTest {

    private static final RecordLayout CODATECN = CobDatFt.layout();

    @Test
    void convertsBothWays() {
        assertThat(CODATECN.length()).isEqualTo(80);
        assertThat(CobDatFt.toIso("20220706")).isEqualTo("2022-07-06");
        assertThat(CobDatFt.toCompact("2022-07-06")).isEqualTo("20220706");
        assertThat(CobDatFt.toIso("99999999")).isEqualTo("9999-99-99");
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {"1|2022-07-06|1", "1|20220706|2", "2|2022-07-06|1", "3|20220706|1",
            "' '|20220706|' '"})
    void rejectsInvalidRequests(String type, String in, String outType) {
        FixedWidthRecord rec = request(type.isBlank() ? " " : type, in, outType.isBlank() ? " " : outType);
        CobDatFt.call(rec);
        assertThat(rec.getTrimmed(CODATECN.field("CODATECN-ERROR-MSG"))).isEqualTo(CobDatFt.INVALID_INPUT);
        assertThat(rec.isSpaces(CODATECN.field("CODATECN-0UT-DATE"))).isTrue();
    }

    @Test
    void writesOnlyTheConvertedPositions() {
        FixedWidthRecord rec = request("2", "2022-07-06", "2");
        rec.setString(CODATECN.field("CODATECN-0UT-DATE"), "XXXXXXXXXXXXXXXXXXXX");
        CobDatFt.call(rec);
        assertThat(rec.getString(CODATECN.field("CODATECN-0UT-DATE"))).isEqualTo("20220706XXXXXXXXXXXX");
        assertThat(rec.getString(CODATECN.field("CODATECN-2O-MM"))).isEqualTo("07");
    }

    @Test
    void convenienceMethodsThrowOnInvalidInput() {
        assertThatThrownBy(() -> CobDatFt.toIso("2022-07-06")).isInstanceOf(IllegalArgumentException.class);
    }

    private static FixedWidthRecord request(String type, String in, String outType) {
        FixedWidthRecord rec = FixedWidthRecord.spaces(CODATECN, RecordEncoding.EBCDIC);
        rec.setString(CODATECN.field("CODATECN-TYPE"), type);
        rec.setString(CODATECN.field("CODATECN-INP-DATE"), in);
        rec.setString(CODATECN.field("CODATECN-OUTTYPE"), outType);
        return rec;
    }
}
