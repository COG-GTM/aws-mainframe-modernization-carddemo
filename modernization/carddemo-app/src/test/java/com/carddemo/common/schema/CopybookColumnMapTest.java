package com.carddemo.common.schema;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.codec.RecordLayout;
import com.carddemo.common.schema.CopybookColumnMap.Entry;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

/** s3.1 acceptance: every leaf field of the 11 VSAM/sequential layouts maps to a column (or is FILLER). */
class CopybookColumnMapTest {

    static final Map<String, Integer> LAYOUT_LENGTHS = Map.ofEntries(
            Map.entry("CSUSR01Y", 80), Map.entry("CVCUS01Y", 500), Map.entry("CVACT01Y", 300),
            Map.entry("CVACT02Y", 150), Map.entry("CVACT03Y", 50), Map.entry("CVTRA05Y", 350),
            Map.entry("CVTRA06Y", 350), Map.entry("CVTRA01Y", 50), Map.entry("CVTRA02Y", 50),
            Map.entry("CVTRA03Y", 60), Map.entry("CVTRA04Y", 60));

    @Test
    void coversExactlyTheElevenLayoutsWithTheirRecordLengths() {
        Map<String, RecordLayout> layouts = CopybookColumnMap.layouts();
        assertThat(layouts.keySet()).containsExactlyInAnyOrderElementsOf(LAYOUT_LENGTHS.keySet());
        layouts.forEach((copybook, layout) ->
                assertThat(layout.length()).as(copybook).isEqualTo(LAYOUT_LENGTHS.get(copybook)));
    }

    @Test
    void everyLeafFieldIsMappedOnceInStorageOrder() {
        CopybookColumnMap.layouts().forEach((copybook, layout) -> {
            List<String> leaves = layout.leaves().stream().map(RecordLayout.Leaf::key).toList();
            List<String> mapped = CopybookColumnMap.forCopybook(copybook).stream().map(Entry::field).toList();
            assertThat(mapped).as(copybook + " fields in copybook-column-map.csv").containsExactlyElementsOf(leaves);
        });
    }

    @Test
    void onlyFillerIsLeftUnstored() {
        for (Entry e : CopybookColumnMap.entries()) {
            boolean filler = e.field().equals("FILLER") || e.field().endsWith("-FILLER");
            assertThat(e.stored()).as(e.copybook() + " " + e.field()).isEqualTo(!filler);
            if (e.stored()) {
                assertThat(e.column()).as(e.field()).matches("[a-z][a-z0-9_]*");
            } else {
                assertThat(e.column()).as(e.field()).isEmpty();
            }
        }
    }

    @Test
    void eachLayoutHasOneTableAndUniqueColumns() {
        Map<String, Set<String>> tablesByCopybook = CopybookColumnMap.entries().stream().filter(Entry::stored)
                .collect(Collectors.groupingBy(Entry::copybook, Collectors.mapping(Entry::table, Collectors.toSet())));
        tablesByCopybook.forEach((copybook, tables) -> assertThat(tables).as(copybook).hasSize(1));
        assertThat(new HashSet<>(tablesByCopybook.values().stream().map(s -> s.iterator().next()).toList()))
                .as("one table per layout").hasSize(tablesByCopybook.size());

        Set<String> columns = new HashSet<>();
        for (Entry e : CopybookColumnMap.entries()) {
            if (e.stored()) {
                assertThat(columns.add(e.qualifiedColumn())).as("duplicate " + e.qualifiedColumn()).isTrue();
            }
        }
    }
}
