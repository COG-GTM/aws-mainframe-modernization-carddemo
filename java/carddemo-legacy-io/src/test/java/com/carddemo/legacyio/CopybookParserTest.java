package com.carddemo.legacyio;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class CopybookParserTest {

    @ParameterizedTest
    @CsvSource({
        "CVACT01Y, 300", "CVACT02Y, 150", "CVACT03Y, 50", "CVCUS01Y, 500", "CVTRA01Y, 50", "CVTRA02Y, 50",
        "CVTRA03Y, 60", "CVTRA04Y, 60", "CVTRA05Y, 350", "CVTRA06Y, 350", "CSUSR01Y, 80", "CVEXPORT, 500"
    })
    void dataFileCopybooksHaveTheirDocumentedRecordLength(String copybook, int length) {
        assertThat(Copybook.load(copybook).layout().length()).isEqualTo(length);
    }

    @Test
    void placesFieldsAtCumulativeOffsets() {
        RecordLayout account = CardDemoLayouts.ACCOUNT;
        assertThat(account.field("ACCT-ID")).isEqualTo(
                new FieldDefinition("ACCT-ID", 0, 11, Picture.parse("9(11)"), Usage.DISPLAY));
        assertThat(account.field("ACCT-CURR-BAL").offset()).isEqualTo(12);
        assertThat(account.field("ACCT-GROUP-ID").offset()).isEqualTo(112);
        assertThat(account.fields().getLast().isFiller()).isTrue();
    }

    @Test
    void exposesGroupSpansForKeys() {
        assertThat(CardDemoLayouts.TRAN_CAT_BALANCE.span("TRAN-CAT-KEY")).isEqualTo(new ItemSpan("TRAN-CAT-KEY", 0, 17));
    }

    @Test
    void expandsOccursWithSubscripts() {
        RecordLayout customer = CardDemoLayouts.export("C");
        assertThat(customer.field("EXP-CUST-ADDR-LINE(1)").offset()).isEqualTo(40 + 79);
        assertThat(customer.field("EXP-CUST-ADDR-LINE(3)").offset()).isEqualTo(40 + 79 + 100);
        assertThat(customer.field("EXP-CUST-PHONE-NUM(2)").length()).isEqualTo(15);
    }

    @Test
    void redefinesAreSwappedInOnlyByView() {
        RecordLayout raw = CardDemoLayouts.EXPORT;
        assertThat(raw.hasField("EXPORT-RECORD-DATA")).isTrue();
        assertThat(raw.hasField("EXP-ACCT-ID")).isFalse();

        RecordLayout account = CardDemoLayouts.export("A");
        assertThat(account.hasField("EXPORT-RECORD-DATA")).isFalse();
        assertThat(account.field("EXPORT-DATE").offset()).isEqualTo(1);
        assertThat(account.field("EXP-ACCT-CURR-CYC-DEBIT").usage()).isEqualTo(Usage.BINARY);
        assertThat(account.field("EXP-ACCT-CURR-CYC-DEBIT").length()).isEqualTo(8);
        assertThat(account.length()).isEqualTo(500);
    }

    @Test
    void parsesFreeStandingItemsAndSkipsConditionNames() {
        String source = """
                      * comment line
                       05  WS-A                PIC X(3) VALUE 'A. B'.
                           88  WS-A-YES        VALUE 'YES'.
                       05  WS-B                PIC S9(5) COMP-3 VALUE -1.
                       05  WS-C.
                           10  WS-C1           PIC 9(4) USAGE IS BINARY.
                           10  FILLER          PIC X(2).
                """;
        RecordLayout layout = Copybook.parse("WSTEST", source).layout();
        assertThat(layout.name()).isEqualTo("WSTEST");
        assertThat(layout.length()).isEqualTo(3 + 3 + 2 + 2);
        assertThat(layout.field("WS-C1").usage()).isEqualTo(Usage.BINARY);
    }

    @Test
    void groupUsageAppliesToChildren() {
        String source = """
                       01  REC.
                           05  NUMS COMP-3.
                               10  N1          PIC S9(3).
                               10  N2          PIC 9(5).
                """;
        RecordLayout layout = Copybook.parse("REC", source).layout();
        assertThat(layout.field("N1").usage()).isEqualTo(Usage.PACKED_DECIMAL);
        assertThat(layout.length()).isEqualTo(2 + 3);
    }

    @Test
    void fillerInsideOccursIsStillFiller() {
        String source = """
                       01  REC.
                           05  ITEMS OCCURS 2 TIMES.
                               10  ITEM        PIC X(2).
                               10  FILLER      PIC X(1).
                """;
        RecordLayout layout = Copybook.parse("REC", source).layout();
        assertThat(layout.fields().get(1).name()).isEqualTo("FILLER(1)");
        assertThat(layout.fields().get(1).isFiller()).isTrue();
        assertThat(Record.initialized(layout).toMap()).containsOnlyKeys("ITEM(1)", "ITEM(2)");
    }

    @Test
    void rejectsOccursDependingOn() {
        String source = """
                       01  REC.
                           05  N               PIC 9(2).
                           05  ITEMS OCCURS 1 TO 5 DEPENDING ON N PIC X.
                """;
        assertThatThrownBy(() -> Copybook.parse("REC", source)).isInstanceOf(CopybookParseException.class);
    }

    @Test
    void parsesEveryCopybookWithDataDescriptions() {
        for (String name : new String[] {"CUSTREC", "CVCRD01Y", "CVTRA07Y", "COCOM01Y", "CSDAT01Y", "CODATECN"}) {
            assertThat(Copybook.load(name).records()).as(name).isNotEmpty();
        }
    }
}
