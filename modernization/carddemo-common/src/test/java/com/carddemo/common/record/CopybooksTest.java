package com.carddemo.common.record;

import com.carddemo.common.cobol.CobolCharsets;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class CopybooksTest {

    static final Path SAMPLE = Path.of("../../app/data/ASCII");

    @Test
    void recordLengthsMatchTheCopybooksAndJcl() {
        assertThat(Copybooks.CVTRA06Y.length()).isEqualTo(350);
        assertThat(Copybooks.CVTRA05Y.length()).isEqualTo(350);
        assertThat(Copybooks.CVACT01Y.length()).isEqualTo(300);
        assertThat(Copybooks.CVACT03Y.length()).isEqualTo(50);
        assertThat(Copybooks.CVTRA01Y.length()).isEqualTo(50);
        assertThat(Copybooks.DALYREJS.length()).isEqualTo(430);
        assertThat(Copybooks.CVTRA06Y.field("DALYTRAN-CARD-NUM").offset()).isEqualTo(262);
        assertThat(Copybooks.CVTRA05Y.field("TRAN-PROC-TS").offset()).isEqualTo(304);
    }

    @Test
    void decodesTheFirstSampleAccount() {
        FixedRecord r = Copybooks.CVACT01Y.parse(
                RecordFiles.readLines(SAMPLE.resolve("acctdata.txt"), 300, CobolCharsets.ASCII).get(0));
        assertThat(r.number("ACCT-ID")).isEqualTo(1L);
        assertThat(r.text("ACCT-ACTIVE-STATUS")).isEqualTo("Y");
        assertThat(r.decimal("ACCT-CURR-BAL")).isEqualByComparingTo("194.00");
        assertThat(r.text("ACCT-EXPIRAION-DATE")).isEqualTo("2025-05-20");
        assertThat(r.text("ACCT-ADDR-ZIP")).isEqualTo("A000000000");
        assertThat(r.text("ACCT-GROUP-ID")).isEmpty();
    }

    @Test
    void parseThenFormatIsIdentityForEverySampleRecord() {
        check(Copybooks.CVACT01Y, "acctdata.txt");
        check(Copybooks.CVACT03Y, "cardxref.txt");
        check(Copybooks.CVTRA01Y, "tcatbal.txt");
        check(Copybooks.CVTRA06Y, "dailytran.txt");
    }

    @Test
    void blankRecordUsesFillerFillAndRefusesOversizedValues() {
        assertThat(Copybooks.CVTRA01Y.blank().toString()).isEqualTo("00000000000  0000" + "0000000000{" + "0".repeat(22));
        assertThatThrownBy(() -> Copybooks.CVACT03Y.blank().set("XREF-CARD-NUM", "12345678901234567"))
                .hasMessageContaining("16 characters");
        assertThatThrownBy(() -> Copybooks.CVACT03Y.parse("short")).hasMessageContaining("must be 50");
        assertThat(Copybooks.CVTRA01Y.blank().set("TRAN-CAT-BAL", new BigDecimal("-12.34")).decimal("TRAN-CAT-BAL"))
                .isEqualByComparingTo("-12.34");
    }

    private static void check(RecordLayout layout, String file) {
        List<String> records = RecordFiles.readLines(SAMPLE.resolve(file), layout.length(), CobolCharsets.ASCII);
        assertThat(records).isNotEmpty();
        for (String text : records) {
            FixedRecord parsed = layout.parse(text);
            FixedRecord rebuilt = layout.parse(text);
            for (RecordLayout.Field f : layout.fields()) {
                switch (f.kind()) {
                    case ALPHA -> rebuilt.set(f.name(), parsed.raw(f.name()));
                    case UNSIGNED, SIGNED -> rebuilt.set(f.name(), parsed.decimal(f.name()));
                    case FILLER -> { }
                }
            }
            assertThat(rebuilt.toString()).as(file).isEqualTo(text);
        }
    }
}
