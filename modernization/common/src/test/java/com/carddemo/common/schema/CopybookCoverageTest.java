package com.carddemo.common.schema;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import org.junit.jupiter.api.Test;

/** Every elementary item of every persisted copybook is documented in schema-mapping.md, in order, with its picture. */
class CopybookCoverageTest {

    private static final List<String> PERSISTED_COPYBOOKS = List.of(
            "CSUSR01Y", "CVCUS01Y", "CVACT01Y", "CVACT02Y", "CVACT03Y",
            "CVTRA05Y", "CVTRA06Y", "CVTRA03Y", "CVTRA04Y", "CVTRA01Y", "CVTRA02Y");

    private final SchemaMapping mapping = SchemaMapping.load();

    @Test
    void everyPersistedCopybookHasASection() {
        assertThat(mapping.sections().stream().map(SchemaMapping.Section::copybook).filter(c -> c != null))
                .containsExactlyInAnyOrderElementsOf(PERSISTED_COPYBOOKS);
    }

    @Test
    void everyElementaryItemIsDocumentedWithItsPicture() {
        for (SchemaMapping.Section section : mapping.sections()) {
            if (section.copybook() == null) {
                continue;
            }
            List<String> expected = Copybook.elementaryItems(section.copybook()).stream()
                    .map(item -> item.name() + " " + Copybook.normalize(item.picture()))
                    .toList();
            List<String> documented = section.copybookFields().stream()
                    .map(row -> row.field() + " " + Copybook.normalize(row.picture()))
                    .toList();
            assertThat(documented).as(section.heading()).containsExactlyElementsOf(expected);
        }
    }

    @Test
    void onlyFillerIsDropped() {
        for (SchemaMapping.Section section : mapping.sections()) {
            section.rows().stream().filter(SchemaMapping.Row::dropped).forEach(row ->
                    assertThat(row.field()).as(section.heading()).matches("FILLER|.*-FILLER"));
        }
    }

    @Test
    void rejectRecordCarriesTheWholeDailyTransactionRecord() {
        SchemaMapping.Section rejects = mapping.sections().stream()
                .filter(s -> s.table().equals("transaction_rejects")).findFirst().orElseThrow();
        List<String> dailyRecord = Copybook.elementaryItems("CVTRA06Y").stream()
                .map(item -> item.name() + " " + Copybook.normalize(item.picture())).toList();
        assertThat(rejects.copybookFields().stream()
                .map(row -> row.field() + " " + Copybook.normalize(row.picture())))
                .startsWith(dailyRecord.toArray(String[]::new));
    }
}
