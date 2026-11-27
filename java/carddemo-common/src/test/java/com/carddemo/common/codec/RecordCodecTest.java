package com.carddemo.common.codec;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.copybook.CopybookParser;
import com.carddemo.common.copybook.RecordLayout;
import com.carddemo.common.testing.MainframeSources;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class RecordCodecTest {

    record CategoryKey(@CobolField("TRANCAT-ACCT-ID") long accountId,
                       @CobolField("TRANCAT-TYPE-CD") String typeCode,
                       @CobolField("TRANCAT-CD") int categoryCode) {
    }

    record CategoryBalance(@CobolField("TRAN-CAT-KEY") CategoryKey key,
                           @CobolField("TRAN-CAT-BAL") BigDecimal balance) {
    }

    record Summary(@CobolField("PA-ACCT-ID") Long accountId,
                   @CobolField("PA-ACCOUNT-STATUS") List<String> statuses,
                   @CobolField("PA-CREDIT-LIMIT") BigDecimal creditLimit,
                   @CobolField("PA-APPROVED-AUTH-CNT") Short approvedCount) {
    }

    record Line(@CobolField("WS-ITEM") List<Item> items) {
    }

    record Item(@CobolField("WS-CODE") String code, @CobolField("WS-QTY") int quantity) {
    }

    @Test
    void nestedGroupRecord() {
        RecordLayout layout = MainframeSources.locate().layout("CVTRA01Y");
        RecordCodec<CategoryBalance> codec = RecordCodec.of(CategoryBalance.class, layout, DataEncoding.EBCDIC);
        CategoryBalance value = new CategoryBalance(new CategoryKey(1, "01", 1), new BigDecimal("-12.34"));
        byte[] bytes = codec.encode(value);
        assertThat(bytes).hasSize(50);
        assertThat(codec.decode(bytes)).isEqualTo(value);
    }

    @Test
    void packedOccursAndBinary() {
        RecordLayout layout = MainframeSources.locate().layout("CIPAUSMY");
        RecordCodec<Summary> codec = RecordCodec.of(Summary.class, layout, DataEncoding.EBCDIC);
        Summary value = new Summary(41000000001L, List.of("A1", "B2", "", "", "Z9"), new BigDecimal("5000.25"), (short) 3);
        assertThat(codec.decode(codec.encode(value))).isEqualTo(value);
    }

    @Test
    void occursOfGroups() {
        RecordLayout layout = CopybookParser.parse("T", """
                       01  WS-LINE.
                           05  WS-ITEM OCCURS 2 TIMES.
                               10  WS-CODE  PIC X(3).
                               10  WS-QTY   PIC 9(2).
                """).first();
        RecordCodec<Line> codec = RecordCodec.of(Line.class, layout, DataEncoding.ASCII);
        byte[] bytes = codec.encode(new Line(List.of(new Item("AB", 7), new Item("XYZ", 42))));
        assertThat(new String(bytes, DataEncoding.ASCII.charset())).isEqualTo("AB 07XYZ42");
        assertThat(codec.decode(bytes).items()).containsExactly(new Item("AB", 7), new Item("XYZ", 42));
    }

    record Wrong(@CobolField("PA-ACCOUNT-STATUS") String status) {
    }

    record Missing(@CobolField("NO-SUCH-FIELD") String value) {
    }

    @Test
    void rejectsMismatchedBindings() {
        RecordLayout layout = MainframeSources.locate().layout("CIPAUSMY");
        assertThatThrownBy(() -> RecordCodec.of(Wrong.class, layout, DataEncoding.EBCDIC))
                .hasMessageContaining("OCCURS 5");
        assertThatThrownBy(() -> RecordCodec.of(Missing.class, layout, DataEncoding.EBCDIC))
                .hasMessageContaining("NO-SUCH-FIELD");
    }
}
