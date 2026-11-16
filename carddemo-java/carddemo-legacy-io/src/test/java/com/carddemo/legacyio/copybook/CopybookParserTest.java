package com.carddemo.legacyio.copybook;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.util.NoSuchElementException;
import org.junit.jupiter.api.Test;

class CopybookParserTest {

    @Test
    void parsesAccountRecordOffsets() {
        CopybookItem record = Copybook.load("CVACT01Y").record();

        assertThat(record.name()).isEqualTo("ACCOUNT-RECORD");
        assertThat(record.length()).isEqualTo(300);
        CopybookItem balance = record.find("ACCT-CURR-BAL").orElseThrow();
        assertThat(balance.offset()).isEqualTo(12);
        assertThat(balance.length()).isEqualTo(12);
        assertThat(balance.picture().scale()).isEqualTo(2);
        assertThat(record.find("ACCT-GROUP-ID").orElseThrow().offset()).isEqualTo(112);
    }

    @Test
    void parsesExportRedefinesComputationalAndOccurs() {
        CopybookItem record = Copybook.load("CVEXPORT").record();

        assertThat(record.length()).isEqualTo(500);
        CopybookItem data = record.find("EXPORT-RECORD-DATA").orElseThrow();
        CopybookItem account = record.find("EXPORT-ACCOUNT-DATA").orElseThrow();
        assertThat(account.redefines()).isEqualTo("EXPORT-RECORD-DATA");
        assertThat(account.offset()).isEqualTo(data.offset());
        assertThat(record.find("EXP-ACCT-CURR-BAL").orElseThrow().usage()).isEqualTo(Usage.PACKED_DECIMAL);
        assertThat(record.find("EXPORT-SEQUENCE-NUM").orElseThrow().usage()).isEqualTo(Usage.BINARY);
        CopybookItem phones = record.find("EXP-CUST-PHONE-NUMS").orElseThrow();
        assertThat(phones.occurs()).isEqualTo(2);
        assertThat(phones.totalLength()).isEqualTo(30);
    }

    @Test
    void handlesSequenceAreaCommentsLevel88AndContinuation() {
        String source = String.join("\n",
                "000100 01  SAMPLE-REC.                                                  SEQ00001",
                "000200* A COMMENT LINE WITH PIC X(99). IN IT",
                "000300     05  STATUS-CODE    PIC X(01).                                SEQ00003",
                "000400         88  ACTIVE     VALUE 'Y'.",
                "000500     05  AMOUNT         PIC S9(07)V99",
                "000600                        USAGE COMP-3.",
                "000700     05  COUNTER        PIC 9(4) COMP.",
                "000800     05  NAMES          OCCURS 3 TIMES.",
                "000900         10  NAME       PIC X(05).");

        CopybookItem record = Copybook.parse("SAMPLE", source).record();

        assertThat(record.length()).isEqualTo(1 + 5 + 2 + 15);
        assertThat(record.find("AMOUNT").orElseThrow().offset()).isEqualTo(1);
        assertThat(record.find("COUNTER").orElseThrow().offset()).isEqualTo(6);
        assertThat(record.find("NAMES").orElseThrow().offset()).isEqualTo(8);
    }

    @Test
    void expandsTabIndentation() {
        assertThat(Copybook.load("CUSTREC").record().length()).isEqualTo(500);
    }

    @Test
    void rejectsUnsupportedClauses() {
        String source = "       01  REC.\n           05  ITEMS OCCURS 1 TO 5 DEPENDING ON N PIC X.";

        assertThatThrownBy(() -> Copybook.parse("BAD", source)).isInstanceOf(CopybookParseException.class);
    }

    @Test
    void reportsMissingCopybook() {
        assertThatThrownBy(() -> Copybook.load("NOSUCHMEMBER")).isInstanceOf(NoSuchElementException.class);
    }
    @Test
    void parsesCommaEditedReportPictures() {
        Copybook copybook = Copybook.load("CVTRA07Y");

        CopybookItem amount = copybook.record("TRANSACTION-DETAIL-REPORT").find("TRAN-REPORT-AMT").orElseThrow();
        assertThat(amount.picture().source()).isEqualTo("-ZZZ,ZZZ,ZZZ.ZZ");
        assertThat(amount.length()).isEqualTo(15);
        assertThat(copybook.record("TRANSACTION-HEADER-2").length()).isEqualTo(133);
    }

    @Test
    void placesItemsAfterTheLargestRedefinition() {
        Copybook copybook = Copybook.parse("R", String.join("\n",
                "       01  R-REC.",
                "           05  A  PIC X(2).",
                "           05  B  REDEFINES A PIC X(4).",
                "           05  C  PIC X."));

        CopybookItem record = copybook.record();
        assertThat(record.find("C").orElseThrow().offset()).isEqualTo(4);
        assertThat(record.length()).isEqualTo(5);
    }
}
