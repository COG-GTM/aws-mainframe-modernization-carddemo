package com.carddemo.common.copybook;

import com.carddemo.common.cobol.CobolDataException;
import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.cobol.Ebcdic;
import com.carddemo.common.testing.MainframeSources;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class RecordViewTest {

    private static final MainframeSources SOURCES = MainframeSources.locate();

    @Test
    void readsAndWritesEbcdicAccount() {
        RecordLayout layout = SOURCES.layout("CVACT01Y");
        RecordView view = RecordView.initialized(layout, DataEncoding.EBCDIC);
        assertThat(view.getDecimal("ACCT-CURR-BAL")).isEqualByComparingTo("0");
        assertThat(view.getString("ACCT-GROUP-ID")).isBlank();

        view.setLong("ACCT-ID", 1);
        view.setString("ACCT-ACTIVE-STATUS", "Y");
        view.setDecimal("ACCT-CURR-BAL", new BigDecimal("-194.00"));
        view.setString("ACCT-GROUP-ID", "A000000000-TRUNCATED");

        assertThat(view.getLong("ACCT-ID")).isEqualTo(1);
        assertThat(view.getDecimal("ACCT-CURR-BAL")).isEqualByComparingTo("-194.00");
        assertThat(view.getString("ACCT-GROUP-ID")).isEqualTo("A000000000");
        assertThat(Ebcdic.toUnicode(view.bytes()).substring(0, 24)).isEqualTo("00000000001Y00000001940}");
    }

    @Test
    void packedAndOccursFields() {
        RecordLayout layout = SOURCES.layout("CIPAUSMY");
        RecordView view = RecordView.initialized(layout, DataEncoding.EBCDIC);
        view.setDecimal("PA-ACCT-ID", new BigDecimal("41000000001"));
        view.setDecimal("PA-CREDIT-LIMIT", new BigDecimal("5000.25"));
        view.setString("PA-ACCOUNT-STATUS", "A1", 0);
        view.setString("PA-ACCOUNT-STATUS", "Z5", 4);
        view.setLong("PA-APPROVED-AUTH-CNT", 12);

        assertThat(view.getLong("PA-ACCT-ID")).isEqualTo(41000000001L);
        assertThat(view.getDecimal("PA-CREDIT-LIMIT")).isEqualByComparingTo("5000.25");
        assertThat(view.getString("PA-ACCOUNT-STATUS", 0)).isEqualTo("A1");
        assertThat(view.getString("PA-ACCOUNT-STATUS", 1)).isEqualTo("  ");
        assertThat(view.getString("PA-ACCOUNT-STATUS", 4)).isEqualTo("Z5");
        assertThat(view.getInt("PA-APPROVED-AUTH-CNT")).isEqualTo(12);
        assertThatThrownBy(() -> view.getString("PA-ACCOUNT-STATUS", 5)).isInstanceOf(IndexOutOfBoundsException.class);
        assertThatThrownBy(() -> view.getString("PA-ACCOUNT-STATUS")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void redefinesGivesAlternateNumericView() {
        RecordLayout layout = SOURCES.layout("CVCRD01Y");
        RecordView view = RecordView.initialized(layout, DataEncoding.ASCII);
        CopybookField numeric = layout.field("CC-ACCT-ID-N");
        assertThat(view.isNumeric(numeric)).isFalse(); // spaces
        view.setString("CC-ACCT-ID", "00000000042");
        assertThat(view.isNumeric(numeric)).isTrue();
        assertThat(view.getLong("CC-ACCT-ID-N")).isEqualTo(42);
    }

    @Test
    void transcoderLeavesPackedBytesAlone() {
        RecordLayout layout = SOURCES.layout("CIPAUSMY");
        RecordView ebcdic = RecordView.initialized(layout, DataEncoding.EBCDIC);
        ebcdic.setDecimal("PA-ACCT-ID", new BigDecimal("40404040404"));
        ebcdic.setDecimal("PA-CUST-ID", new BigDecimal("123456789"));
        byte[] ascii = RecordTranscoder.transcode(ebcdic.bytes(), layout, DataEncoding.EBCDIC, DataEncoding.ASCII);
        RecordView view = RecordView.wrap(layout, DataEncoding.ASCII, ascii);
        assertThat(view.getLong("PA-ACCT-ID")).isEqualTo(40404040404L);
        assertThat(view.getLong("PA-CUST-ID")).isEqualTo(123456789L);
        assertThat(RecordTranscoder.transcode(ascii, layout, DataEncoding.ASCII, DataEncoding.EBCDIC)).isEqualTo(ebcdic.bytes());
    }

    @Test
    void rejectsShortRecordsAndWrongTypes() {
        RecordLayout layout = SOURCES.layout("CVACT03Y");
        assertThatThrownBy(() -> RecordView.wrap(layout, DataEncoding.EBCDIC, new byte[10])).isInstanceOf(CobolDataException.class);
        RecordView view = RecordView.initialized(layout, DataEncoding.EBCDIC);
        assertThatThrownBy(() -> view.setString("XREF-ACCT-ID", "1")).isInstanceOf(CobolDataException.class);
        assertThatThrownBy(() -> view.getDecimal("XREF-CARD-NUM")).isInstanceOf(CobolDataException.class);
    }
}
