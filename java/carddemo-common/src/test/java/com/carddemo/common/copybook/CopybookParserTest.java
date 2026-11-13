package com.carddemo.common.copybook;

import com.carddemo.common.cobol.SignMode;
import com.carddemo.common.cobol.Usage;
import com.carddemo.common.testing.MainframeSources;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

class CopybookParserTest {

    private static final MainframeSources SOURCES = MainframeSources.locate();

    /** Copybooks that hold PROCEDURE DIVISION code rather than data descriptions. */
    private static final Set<String> PROCEDURE_COPYBOOKS = Set.of("CSUTLDPY", "CSSTRPFY", "CSSETATY", "CSDB2RPY");

    @ParameterizedTest(name = "{0} -> {1} bytes")
    @CsvSource({
            "CVACT01Y, 300", "CVACT02Y, 150", "CVACT03Y, 50", "CVCUS01Y, 500", "CUSTREC, 500",
            "CVTRA01Y, 50", "CVTRA02Y, 50", "CVTRA03Y, 60", "CVTRA04Y, 60", "CVTRA05Y, 350", "CVTRA06Y, 350",
            "CSUSR01Y, 80", "COSTM01, 350", "CIPAUSMY, 100", "CIPAUDTY, 200"
    })
    void recordLengthsMatchDocumentedLrecl(String copybook, int length) {
        assertThat(SOURCES.layout(copybook).length()).isEqualTo(length);
    }

    static Stream<Path> allCopybooks() throws IOException {
        List<String> dirs = List.of("app/cpy", "app/cpy-bms", "app/app-authorization-ims-db2-mq/cpy",
                "app/app-authorization-ims-db2-mq/cpy-bms", "app/app-transaction-type-db2/cpy",
                "app/app-transaction-type-db2/cpy-bms");
        Stream<Path> all = Stream.empty();
        for (String d : dirs) {
            try (Stream<Path> files = Files.list(SOURCES.resolve(d))) {
                all = Stream.concat(all, files.filter(f -> !f.getFileName().toString().startsWith(".")).toList().stream());
            }
        }
        return all.sorted();
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("allCopybooks")
    void parsesEveryCopybookInTheRepository(Path path) {
        Copybook copybook = CopybookParser.parse(path);
        if (PROCEDURE_COPYBOOKS.contains(copybook.name())) {
            assertThat(copybook.records()).isEmpty();
        } else {
            assertThat(copybook.records()).isNotEmpty();
            copybook.records().forEach(r -> assertThat(r.length()).isPositive());
        }
    }

    @Test
    void accountLayoutOffsets() {
        RecordLayout account = SOURCES.layout("CVACT01Y");
        assertThat(account.name()).isEqualTo("ACCOUNT-RECORD");
        CopybookField bal = account.field("ACCT-CURR-BAL");
        assertThat(bal.offset()).isEqualTo(12);
        assertThat(bal.length()).isEqualTo(12);
        assertThat(bal.pic().scale()).isEqualTo(2);
        assertThat(bal.signMode()).isEqualTo(SignMode.TRAILING_OVERPUNCH);
        assertThat(account.field("ACCT-OPEN-DATE").offset()).isEqualTo(48);
        assertThat(account.field("ACCT-GROUP-ID").offset()).isEqualTo(112);
    }

    @Test
    void groupKeysAreNested() {
        RecordLayout tcat = SOURCES.layout("CVTRA01Y");
        CopybookField key = tcat.field("TRAN-CAT-KEY");
        assertThat(key.isGroup()).isTrue();
        assertThat(key.length()).isEqualTo(17);
        assertThat(tcat.field("TRANCAT-CD").path()).isEqualTo("TRAN-CAT-BAL-RECORD.TRAN-CAT-KEY.TRANCAT-CD");
        assertThat(tcat.field("TRAN-CAT-BAL").offset()).isEqualTo(17);
    }

    @Test
    void packedDecimalAndOccursInPendingAuthorizationSummary() {
        RecordLayout summary = SOURCES.layout("CIPAUSMY");
        CopybookField acct = summary.field("PA-ACCT-ID");
        assertThat(acct.usage()).isEqualTo(Usage.PACKED_DECIMAL);
        assertThat(acct.length()).isEqualTo(6);
        CopybookField statuses = summary.field("PA-ACCOUNT-STATUS");
        assertThat(statuses.occurs()).isEqualTo(5);
        assertThat(statuses.offset()).isEqualTo(16);
        assertThat(statuses.totalLength()).isEqualTo(10);
        assertThat(summary.field("PA-CREDIT-LIMIT").offset()).isEqualTo(26);
        assertThat(summary.field("PA-APPROVED-AUTH-CNT").usage()).isEqualTo(Usage.BINARY);
        assertThat(summary.field("PA-APPROVED-AUTH-CNT").length()).isEqualTo(2);
    }

    @Test
    void redefinesShareStorage() {
        RecordLayout work = SOURCES.layout("CVCRD01Y");
        CopybookField text = work.field("CC-ACCT-ID");
        CopybookField numeric = work.field("CC-ACCT-ID-N");
        assertThat(numeric.redefines()).isEqualTo("CC-ACCT-ID");
        assertThat(numeric.offset()).isEqualTo(text.offset());
        assertThat(numeric.isAlternateView()).isTrue();
        assertThat(work.field("CC-CARD-NUM").offset()).isEqualTo(text.offset() + 11);
        assertThat(work.field("CCARD-AID").conditions()).extracting(ConditionName::name).contains("CCARD-AID-ENTER", "CCARD-AID-PFK12");
    }

    @Test
    void occursOverRedefinedTable() {
        RecordLayout menu = SOURCES.layout("COMEN02Y");
        CopybookField options = menu.field("CDEMO-MENU-OPTIONS");
        CopybookField opt = menu.field("CDEMO-MENU-OPT");
        assertThat(opt.occurs()).isEqualTo(12);
        assertThat(opt.length()).isEqualTo(2 + 35 + 8 + 1);
        assertThat(options.offset()).isEqualTo(menu.field("CDEMO-MENU-OPTIONS-DATA").offset());
        assertThat(menu.field("CDEMO-MENU-OPT-PGMNAME").occursAncestors()).containsExactly(opt);
    }

    @Test
    void inlineSourceWithUsageInheritanceAndConditions() {
        String src = """
                       01  WS-REC.
                           05  WS-AMOUNTS COMP-3.
                               10  WS-AMT  PIC S9(7)V99 OCCURS 3 TIMES.
                           05  WS-COUNT    PIC S9(4) COMP VALUE +0.
                           05  WS-FLAG     PIC X VALUE 'N'.
                               88  WS-FLAG-ON  VALUE 'Y'.
                               88  WS-RANGE    VALUES 'A' THRU 'C' 'X'.
                           05  WS-SIGNED   PIC S9(3) SIGN LEADING SEPARATE.
                           05  WS-TEXT     PIC X(10) VALUE 'A. B ''C'''.
                           05  FILLER      PIC X(5).
                """;
        RecordLayout r = CopybookParser.parse("INLINE", src).first();
        assertThat(r.field("WS-AMT").usage()).isEqualTo(Usage.PACKED_DECIMAL);
        assertThat(r.field("WS-AMOUNTS").length()).isEqualTo(15);
        assertThat(r.field("WS-COUNT").offset()).isEqualTo(15);
        assertThat(r.field("WS-FLAG").conditions()).containsExactly(
                new ConditionName("WS-FLAG-ON", List.of("'Y'")),
                new ConditionName("WS-RANGE", List.of("'A' THRU 'C'", "'X'")));
        assertThat(r.field("WS-SIGNED").signMode()).isEqualTo(SignMode.LEADING_SEPARATE);
        assertThat(r.field("WS-SIGNED").length()).isEqualTo(4);
        assertThat(r.length()).isEqualTo(15 + 2 + 1 + 4 + 10 + 5);
    }
}
