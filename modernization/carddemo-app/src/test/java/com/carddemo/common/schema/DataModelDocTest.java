package com.carddemo.common.schema;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.codec.Field;
import com.carddemo.common.codec.RecordLayout;
import com.carddemo.common.codec.TestData;
import com.carddemo.common.schema.CopybookColumnMap.Entry;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

/**
 * The field-to-column tables in docs/modernization/03-data-model.md are generated from the parsed copybooks and
 * copybook-column-map.csv. Regenerate with {@code mvn test -Dtest=DataModelDocTest -Dcarddemo.docs.write=true}.
 */
class DataModelDocTest {

    static final String DOC = "docs/modernization/03-data-model.md";
    static final String BEGIN = "<!-- BEGIN GENERATED: copybook-column-map (DataModelDocTest) -->";
    static final String END = "<!-- END GENERATED: copybook-column-map -->";

    static String render() {
        StringBuilder md = new StringBuilder();
        CopybookColumnMap.layouts().forEach((copybook, layout) -> {
            List<Entry> entries = CopybookColumnMap.forCopybook(copybook);
            Entry first = entries.get(0);
            String table = entries.stream().filter(Entry::stored).findFirst().orElseThrow().table();
            md.append("\n#### ").append(first.dataset()).append(" (`").append(copybook).append("`, ")
                    .append(layout.length()).append(" bytes) → `").append(table).append("`\n\n")
                    .append("| Offset | Len | COBOL field | PIC | Usage | Column | SQL type | Null |\n")
                    .append("|---:|---:|---|---|---|---|---|---|\n");
            Iterator<Entry> mapped = entries.iterator();
            for (RecordLayout.Leaf leaf : layout.leaves()) {
                Entry e = mapped.next();
                Field f = leaf.field();
                md.append("| ").append(f.offset()).append(" | ").append(f.size()).append(" | `").append(leaf.key())
                        .append("` | `").append(CopybookColumnMap.pic(f)).append("` | ")
                        .append(CopybookColumnMap.usage(f)).append(" | ");
                if (e.stored()) {
                    md.append('`').append(e.column()).append("` | `")
                            .append(CopybookColumnMap.sqlType(f).replace("character varying", "varchar"))
                            .append("` | NOT NULL |\n");
                } else {
                    md.append("— | not stored (FILLER) | — |\n");
                }
            }
        });
        return md.toString();
    }

    @Test
    void generatedMappingTablesAreCurrent() throws IOException {
        Path doc = TestData.resolve(DOC);
        String text = Files.readString(doc, StandardCharsets.UTF_8);
        int begin = text.indexOf(BEGIN);
        int end = text.indexOf(END);
        assertThat(begin).as(DOC + " generated-section markers").isNotNegative().isLessThan(end);
        String updated = text.substring(0, begin + BEGIN.length()) + "\n" + render() + "\n"
                + text.substring(end);
        if (Boolean.getBoolean("carddemo.docs.write")) {
            Files.writeString(doc, updated, StandardCharsets.UTF_8);
        }
        assertThat(text).as("run with -Dcarddemo.docs.write=true to regenerate " + DOC).isEqualTo(updated);
    }

    @Test
    void everyStoredFieldAppearsInTheDocument() throws IOException {
        String text = Files.readString(TestData.resolve(DOC), StandardCharsets.UTF_8);
        for (Map.Entry<String, RecordLayout> layout : CopybookColumnMap.layouts().entrySet()) {
            for (RecordLayout.Leaf leaf : layout.getValue().leaves()) {
                assertThat(text).as(layout.getKey()).contains("`" + leaf.key() + "`");
            }
        }
    }
}
