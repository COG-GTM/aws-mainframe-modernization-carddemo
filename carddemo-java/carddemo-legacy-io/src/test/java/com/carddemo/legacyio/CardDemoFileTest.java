package com.carddemo.legacyio;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.legacyio.copybook.Copybook;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

class CardDemoFileTest {

    @ParameterizedTest
    @EnumSource(CardDemoFile.class)
    void copybookLayoutMatchesRecordLength(CardDemoFile file) {
        assertThat(file.layout().length()).isEqualTo(file.recordLength());
    }

    @ParameterizedTest
    @EnumSource(CardDemoFile.class)
    void packagedCopybookIsTheOneInApp(CardDemoFile file) throws IOException {
        byte[] source = Files.readAllBytes(SampleData.appDir().resolve("cpy").resolve(file.copybook() + ".cpy"));
        try (InputStream packaged = Copybook.class.getClassLoader()
                .getResourceAsStream(Copybook.RESOURCE_FOLDER + file.copybook() + ".cpy")) {
            assertThat(packaged).isNotNull();
            assertThat(packaged.readAllBytes()).isEqualTo(source);
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"EXPORT-CUSTOMER-DATA", "EXPORT-ACCOUNT-DATA", "EXPORT-TRANSACTION-DATA",
        "EXPORT-CARD-XREF-DATA", "EXPORT-CARD-DATA"})
    void exportVariantsCoverTheWholeRecord(String variant) {
        RecordLayout layout = CardDemoFile.EXPORT.layout(variant);

        assertThat(layout.length()).isEqualTo(500);
        assertThat(layout.hasField("EXPORT-RECORD-DATA")).isFalse();
        assertThat(layout.span(variant).offset()).isEqualTo(40);
        assertThat(layout.span(variant).length()).isEqualTo(460);
    }

    @Test
    void accountLayoutFields() {
        RecordLayout layout = CardDemoFile.ACCOUNT.layout();

        assertThat(layout.field("ACCT-ID").offset()).isZero();
        assertThat(layout.field("ACCT-CURR-BAL").offset()).isEqualTo(12);
        assertThat(layout.field("ACCT-GROUP-ID").end()).isEqualTo(122);
    }

    @Test
    void customerPhoneOccursIsExpanded() {
        RecordLayout layout = CardDemoFile.EXPORT.layout("EXPORT-CUSTOMER-DATA");

        assertThat(layout.hasField("EXP-CUST-PHONE-NUM(1)")).isTrue();
        assertThat(layout.field("EXP-CUST-PHONE-NUM(2)").offset())
                .isEqualTo(layout.field("EXP-CUST-PHONE-NUM(1)").offset() + 15);
    }
}
