package com.carddemo.common.codec;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class CopybookTest {

    /** Copybooks of app/cpy that are not data descriptions (procedure code, screen/message literal tables...). */
    private static final List<String> NOT_DATA = List.of("CSSETATY", "CSSTRPFY", "CSUTLDPY");

    @Test
    void everyDataCopybookOfTheEstateParses() throws IOException {
        Map<String, String> failures = new TreeMap<>();
        try (Stream<Path> files = Files.list(TestData.resolve("app/cpy"))) {
            for (Path file : files.sorted().toList()) {
                String name = file.getFileName().toString().replaceFirst("\\.(cpy|CPY)$", "");
                try {
                    Copybook cb = Copybook.parse(name, Files.readString(file, StandardCharsets.ISO_8859_1));
                    assertThat(cb.records()).isNotEmpty();
                    cb.records().forEach(r -> assertThat(r.length()).isPositive());
                } catch (RecordFormatException e) {
                    failures.put(name, e.getMessage());
                }
            }
        }
        assertThat(failures.keySet()).as(failures.toString()).containsExactlyInAnyOrderElementsOf(NOT_DATA);
    }

    @ParameterizedTest
    @CsvSource({"CVACT01Y, 300", "CVACT02Y, 150", "CVACT03Y, 50", "CVCUS01Y, 500", "CUSTREC, 500", "CVTRA01Y, 50",
            "CVTRA02Y, 50", "CVTRA03Y, 60", "CVTRA04Y, 60", "CVTRA05Y, 350", "CVTRA06Y, 350", "CSUSR01Y, 80",
            "CVEXPORT, 500", "CODATECN, 80"})
    void recordLengthsMatchTheVsamLrecl(String copybook, int length) {
        assertThat(Copybook.layout(copybook).length()).isEqualTo(length);
    }

    @Test
    void accountRecordOffsets() {
        RecordLayout acct = Copybook.layout("CVACT01Y");
        assertThat(acct.name()).isEqualTo("ACCOUNT-RECORD");
        Field bal = acct.field("ACCT-CURR-BAL");
        assertThat(bal.offset()).isEqualTo(12);
        assertThat(bal.size()).isEqualTo(12);
        assertThat(bal.digits()).isEqualTo(12);
        assertThat(bal.scale()).isEqualTo(2);
        assertThat(bal.signed()).isTrue();
        assertThat(bal.usage()).isEqualTo(Usage.DISPLAY);
        assertThat(acct.field("ACCT-GROUP-ID").offset()).isEqualTo(112);
        assertThat(acct.fields().get(0)).isSameAs(acct.root());
    }

    @Test
    void exportRecordRedefinesOccursCompAndComp3() {
        RecordLayout exp = Copybook.layout("CVEXPORT");
        Field seq = exp.field("EXPORT-SEQUENCE-NUM");
        assertThat(seq.offset()).isEqualTo(27);
        assertThat(seq.size()).isEqualTo(4);
        assertThat(seq.usage()).isEqualTo(Usage.BINARY);
        assertThat(exp.field("EXPORT-TIMESTAMP-R").offset()).isEqualTo(1);
        assertThat(exp.field("EXPORT-TIMESTAMP-R").redefines()).isEqualTo("EXPORT-TIMESTAMP");
        assertThat(exp.field("EXPORT-TIME").offset()).isEqualTo(12);
        assertThat(exp.field("EXPORT-RECORD-DATA").offset()).isEqualTo(40);
        for (String variant : List.of("EXPORT-CUSTOMER-DATA", "EXPORT-ACCOUNT-DATA", "EXPORT-TRANSACTION-DATA",
                "EXPORT-CARD-XREF-DATA", "EXPORT-CARD-DATA")) {
            assertThat(exp.field(variant).offset()).as(variant).isEqualTo(40);
            assertThat(exp.field(variant).totalSize()).as(variant).isLessThanOrEqualTo(460);
        }
        Field lines = exp.field("EXP-CUST-ADDR-LINES");
        assertThat(lines.occurs()).isEqualTo(3);
        assertThat(lines.offset()).isEqualTo(119);
        assertThat(lines.totalSize()).isEqualTo(150);
        Field line = exp.field("EXP-CUST-ADDR-LINE");
        assertThat(line.dimensions()).isEqualTo(1);
        assertThat(line.subscript(1).offset()).isEqualTo(119);
        assertThat(line.subscript(3).offset()).isEqualTo(219);
        assertThat(line.subscript(3).dimensions()).isZero();
        assertThat(exp.field("EXP-CUST-ADDR-STATE-CD").offset()).isEqualTo(269);
        assertThat(exp.field("EXP-CUST-PHONE-NUM").subscript(2).offset()).isEqualTo(299);
        Field fico = exp.field("EXP-CUST-FICO-CREDIT-SCORE");
        assertThat(fico.usage()).isEqualTo(Usage.PACKED);
        assertThat(fico.size()).isEqualTo(2);
        assertThat(exp.field("EXP-ACCT-CURR-BAL").size()).isEqualTo(7);
        assertThat(exp.field("EXP-ACCT-CURR-CYC-DEBIT").size()).isEqualTo(8);
        assertThatThrownBy(() -> line.subscript(4)).isInstanceOf(IndexOutOfBoundsException.class);
        assertThatThrownBy(() -> line.subscript(0)).isInstanceOf(IndexOutOfBoundsException.class);
        assertThatThrownBy(() -> line.subscript()).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void leavesSelectTheActiveRedefinition() {
        RecordLayout exp = Copybook.layout("CVEXPORT");
        List<String> plain = exp.leaves().stream().map(RecordLayout.Leaf::key).toList();
        assertThat(plain).contains("EXPORT-TIMESTAMP", "EXPORT-RECORD-DATA").doesNotContain("EXP-CUST-ID");
        List<String> customer = exp.leaves("EXPORT-CUSTOMER-DATA", "EXPORT-TIMESTAMP-R").stream()
                .map(RecordLayout.Leaf::key).toList();
        assertThat(customer).doesNotContain("EXPORT-RECORD-DATA", "EXPORT-TIMESTAMP", "EXP-ACCT-ID")
                .contains("EXPORT-DATE", "EXP-CUST-ID", "EXP-CUST-ADDR-LINE(1)", "EXP-CUST-ADDR-LINE(3)",
                        "EXP-CUST-PHONE-NUM(2)")
                .containsSubsequence("EXP-CUST-ADDR-LINE(1)", "EXP-CUST-ADDR-LINE(2)", "EXP-CUST-ADDR-LINE(3)",
                        "EXP-CUST-ADDR-STATE-CD");
    }

    @Test
    void duplicateNamesAreQualifiedInLeafKeys() {
        RecordLayout codatecn = Copybook.layout("CODATECN");
        List<String> keys = codatecn.leaves("CODATECN-1INP", "CODATECN-2INP").stream()
                .map(RecordLayout.Leaf::key).toList();
        assertThat(keys).contains("CODATECN-1MM OF CODATECN-1INP", "CODATECN-1MM OF CODATECN-2INP");
        assertThat(codatecn.field("CODATECN-1MM", "CODATECN-2INP").offset()).isEqualTo(6);
        assertThat(codatecn.field("CODATECN-1MM", "CODATECN-1INP", "CODATECN-REC").offset()).isEqualTo(5);
        assertThatThrownBy(() -> codatecn.field("CODATECN-1MM")).isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("ambiguous");
        assertThatThrownBy(() -> codatecn.field("NOPE")).isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("no item");
    }

    private static final String SYNTHETIC = String.join("\n",
            "000100* a comment line",
            String.format("%-72s%s", "000200 01  SAMPLE-REC.", "SAMPLE01"),
            "000300     05  REC-TYPE        PIC X(01) VALUE 'A. B'.",
            "000400         88  TYPE-A          VALUE 'A'.",
            "000500     05  AMOUNTS COMP-3.",
            "000600         10  AMT-1        PIC S9(7)V99.",
            "000700         10  AMT-2        PIC S9(5) USAGE IS DISPLAY.",
            "000800     05  CNT              PIC 9(02) COMP SYNC.",
            "000900     05  TABLE-A OCCURS 2 TIMES INDEXED BY IX1.",
            "001000         10  ROW-B OCCURS 3 ASCENDING KEY IS CELL-C.",
            "001100             15  CELL-C   PIC X(02).",
            "001200         10  ROW-TAG      PIC X JUSTIFIED RIGHT.",
            "001300     05  ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON CNT OF SAMPLE-REC.",
            "001400         10  ITEM-AMT     PIC 9(3)V9 BLANK WHEN ZERO.",
            "001500     05  ITEMS-X REDEFINES ITEMS PIC X(20).",
            "001600     05  EDITED           PICTURE IS -ZZ9.99.",
            "001700\t    05  FILLER           PIC X(03) VALUES ARE SPACES.",
            "001800 77  STANDALONE           PIC 9(04) COMP-3 GLOBAL.",
            "001900 01  OTHER-REC.",
            "002000     05  OTHER-FIELD      PIC X(05).");

    @Test
    void parsesClausesUsageInheritanceAndNestedTables() {
        Copybook cb = Copybook.parse("SAMPLE", SYNTHETIC);
        assertThat(cb.name()).isEqualTo("SAMPLE");
        assertThat(cb.records()).extracting(RecordLayout::name)
                .containsExactly("SAMPLE-REC", "STANDALONE", "OTHER-REC");
        assertThat(cb.record("OTHER-REC").length()).isEqualTo(5);
        assertThat(cb.record("standalone").length()).isEqualTo(3);
        assertThatThrownBy(() -> cb.record("MISSING")).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(cb::single).isInstanceOf(IllegalStateException.class);

        RecordLayout rec = cb.record("SAMPLE-REC");
        assertThat(rec.field("AMT-1").usage()).isEqualTo(Usage.PACKED);
        assertThat(rec.field("AMT-1").size()).isEqualTo(5);
        assertThat(rec.field("AMT-2").usage()).isEqualTo(Usage.DISPLAY);
        assertThat(rec.field("AMT-2").offset()).isEqualTo(6);
        assertThat(rec.field("CNT").offset()).isEqualTo(11);
        assertThat(rec.field("CNT").size()).isEqualTo(2);
        Field table = rec.field("TABLE-A");
        assertThat(table.offset()).isEqualTo(13);
        assertThat(table.size()).isEqualTo(7);
        assertThat(table.totalSize()).isEqualTo(14);
        Field cell = rec.field("CELL-C");
        assertThat(cell.dimensions()).isEqualTo(2);
        assertThat(cell.subscript(1, 1).offset()).isEqualTo(13);
        assertThat(cell.subscript(1, 3).offset()).isEqualTo(17);
        assertThat(cell.subscript(2, 2).offset()).isEqualTo(22);
        assertThat(table.subscript(2).child("CELL-C").subscript(3).offset()).isEqualTo(24);
        assertThat(table.subscript(2).child("ROW-TAG").offset()).isEqualTo(26);
        assertThatThrownBy(() -> table.child("NOPE")).isInstanceOf(IllegalArgumentException.class);
        Field items = rec.field("ITEMS");
        assertThat(items.occurs()).isEqualTo(5);
        assertThat(items.dependingOn()).isEqualTo("CNT");
        assertThat(items.offset()).isEqualTo(27);
        assertThat(rec.field("ITEMS-X").offset()).isEqualTo(27);
        assertThat(rec.field("ITEMS-X").size()).isEqualTo(20);
        Field edited = rec.field("EDITED");
        assertThat(edited.offset()).isEqualTo(47);
        assertThat(edited.isNumericEdited()).isTrue();
        assertThat(edited.isNumeric()).isFalse();
        assertThat(rec.length()).isEqualTo(57);
        assertThat(rec.leaves()).extracting(RecordLayout.Leaf::key)
                .contains("CELL-C(2,3)", "ITEM-AMT(5)", "FILLER").doesNotContain("ITEMS-X");
        assertThat(rec.toString()).isEqualTo("RecordLayout[SAMPLE-REC, 57 bytes]");
        assertThat(rec.field("CELL-C").toString()).isEqualTo("15 CELL-C @13+2 PIC XX");
        assertThat(rec.field("AMT-1").toString()).isEqualTo("10 AMT-1 @1+5 PIC S9999999V99 PACKED");
        assertThat(table.toString()).endsWith("OCCURS 2");
        assertThat(rec.field("AMT-1").ancestors()).containsExactly("AMOUNTS", "SAMPLE-REC");
        assertThat(rec.field("AMT-1")).isEqualTo(Copybook.parse("SAMPLE", SYNTHETIC).record("SAMPLE-REC")
                .field("AMT-1")).hasSameHashCodeAs(rec.field("AMT-1")).isNotEqualTo(rec.field("AMT-2"));
        assertThat(rec.field("REC-TYPE").level()).isEqualTo(5);
        assertThat(rec.field("REC-TYPE").picture().text()).isEqualTo("X");
    }

    @Test
    void fragmentCopybookBecomesOneRecordNamedAfterIt() {
        RecordLayout wy = Copybook.layout("CSUTLDWY");
        assertThat(wy.name()).isEqualTo("CSUTLDWY");
        assertThat(wy.field("WS-EDIT-DATE-CCYYMMDD").offset()).isZero();
    }

    @Test
    void continuationLinesJoinLiterals() {
        String src = String.join("\n",
                "       01  R.",
                "           05  F  PIC X(10) VALUE 'ABCDE",
                "      -    'FGHIJ'.",
                "           05  G  PIC X.");
        assertThat(Copybook.parse("CONT", src).single().length()).isEqualTo(11);
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "       01  R.\n           05  F PIC X SIGN LEADING SEPARATE.",
            "       01  R.\n           05  F PIC X FROBNICATE.",
            "       01  R PIC X.\n           05  F PIC X.",
            "       01  R.\n           05  F PIC X.\n           05  G REDEFINES NOPE PIC X.",
            "       01  R.\n           05  F.",
            "       MOVE A TO B.",
            "       50  R PIC X.",
            "       01  R.\n           05  F PIC X VALUE 'OPEN.",
            "       01  R.\n           05  F COMP-1.",
            "       01  R.\n           05  F PIC X(4) COMP.",
            "       01  R.\n           05  F PIC X USAGE IS FOO.",
            "       01  R.\n           05  F PIC X OCCURS MANY.",
            "       01  R.\n           05  F PIC",
            "      * nothing but a comment"
    })
    void rejectsUnsupportedOrMalformedEntries(String source) {
        assertThatThrownBy(() -> Copybook.parse("BAD", source).records().get(0).leaves())
                .isInstanceOf(RecordFormatException.class);
    }

    @Test
    void loadFailsForUnknownCopybook() {
        assertThatThrownBy(() -> Copybook.load("NOSUCHCPY")).isInstanceOf(IllegalArgumentException.class);
    }
}
