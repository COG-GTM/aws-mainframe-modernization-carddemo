package com.carddemo.transactiontype.domain;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.transactiontype.domain.LegacyRuleException.Kind;
import java.util.stream.IntStream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class TransactionTypeServiceTest {

    private InMemoryTransactionTypeStore store;
    private TransactionTypeService service;

    @BeforeEach
    void setUp() {
        store = InMemoryTransactionTypeStore.seeded();
        IntStream.rangeClosed(10, 19).forEach(i -> store.types.put(String.valueOf(i), "EXTRA TYPE " + i));
        service = new TransactionTypeService(store);
    }

    private static void assertRule(Runnable r, String ruleId, Kind kind) {
        assertThatThrownBy(r::run).isInstanceOfSatisfying(LegacyRuleException.class, e -> {
            assertThat(e.ruleId()).isEqualTo(ruleId);
            assertThat(e.kind()).isEqualTo(kind);
        });
    }

    @Test
    @DisplayName("BR-TT-010 first page: 7 rows ordered by TR_TYPE")
    void br_tt_010_firstPage() {
        var page = service.list(null, null, null, PageDirection.FIRST);
        assertThat(page.items()).extracting(TransactionType::code)
                .containsExactly("01", "02", "03", "04", "05", "06", "07");
        assertThat(page.totalMatching()).isEqualTo(17);
        assertThat(page.hasPreviousPage()).isFalse();
    }

    @Test
    @DisplayName("BR-TT-013 PF8 starts at the peeked 8th row; last page has no next")
    void br_tt_013_forwardPaging() {
        var p1 = service.list(null, null, null, PageDirection.FIRST);
        assertThat(p1.hasNextPage()).isTrue();
        assertThat(p1.nextStartKey()).isEqualTo("10");
        var p2 = service.list(null, null, p1.nextStartKey(), PageDirection.NEXT);
        assertThat(p2.items()).extracting(TransactionType::code)
                .containsExactly("10", "11", "12", "13", "14", "15", "16");
        var p3 = service.list(null, null, p2.nextStartKey(), PageDirection.NEXT);
        assertThat(p3.items()).extracting(TransactionType::code).containsExactly("17", "18", "19");
        assertThat(p3.hasNextPage()).isFalse();
        assertThat(p3.hasPreviousPage()).isTrue();
    }

    @Test
    @DisplayName("BR-TT-014 PF7 reads TR_TYPE < first key descending and shows rows ascending")
    void br_tt_014_backwardPaging() {
        var p3 = service.list(null, null, "17", PageDirection.PREVIOUS);
        assertThat(p3.items()).extracting(TransactionType::code)
                .containsExactly("10", "11", "12", "13", "14", "15", "16");
    }

    @Test
    @DisplayName("BR-TT-015 PF7 on the first page / PF8 on the last page return legacy messages")
    void br_tt_015_pagingBoundaries() {
        assertThat(service.list(null, null, "01", PageDirection.PREVIOUS).message())
                .isEqualTo("No previous pages to display");
        assertThat(service.list(null, null, "17", PageDirection.NEXT).message())
                .isEqualTo("No more pages to display");
    }

    @Test
    @DisplayName("BR-TT-016 no matching rows message")
    void br_tt_016_noRecords() {
        var page = service.list(null, "NOPE", null, PageDirection.FIRST);
        assertThat(page.items()).isEmpty();
        assertThat(page.message()).isEqualTo("No records found for this search condition.");
    }

    @Test
    @DisplayName("BR-TT-017 filters combine (AND) and FIRST resets paging regardless of startKey")
    void br_tt_017_filtersAndReset() {
        assertThat(service.list("05", null, null, PageDirection.FIRST).items())
                .extracting(TransactionType::code).containsExactly("05");
        assertThat(service.list(null, "EXTRA", "15", PageDirection.FIRST).items())
                .extracting(TransactionType::code).startsWith("10");
        assertThat(service.list("02", "PURCHASE", null, PageDirection.FIRST).items()).isEmpty();
    }

    @Test
    @DisplayName("BR-TT-020 get unknown key -> not found")
    void br_tt_020_getNotFound() {
        assertThat(service.get("1").description()).isEqualTo("PURCHASE");
        assertRule(() -> service.get("99"), "BR-TT-020", Kind.NOT_FOUND);
    }

    @Test
    @DisplayName("BR-TT-030 create inserts; duplicate key (-803) is a conflict")
    void br_tt_030_create() {
        var r = service.create("8", "  CASHBACK ");
        assertThat(r.item()).isEqualTo(new TransactionType("08", "CASHBACK"));
        assertThat(store.types).containsEntry("08", "CASHBACK");
        assertRule(() -> service.create("01", "DUP"), "BR-TT-030", Kind.CONFLICT);
    }

    @Test
    @DisplayName("BR-TT-008 update with no change is rejected")
    void br_tt_008_updateNoChange() {
        assertRule(() -> service.update("01", "purchase", false), "BR-TT-008", Kind.NO_CHANGE);
    }

    @Test
    @DisplayName("BR-TT-040 update of a row deleted meanwhile (+100) -> not found")
    void br_tt_040_updateLostRow() {
        TransactionTypeStore racing = new InMemoryTransactionTypeStore() {
            { types.put("02", "PAYMENT"); }

            @Override
            public int updateDescription(String code, String description) {
                return 0;
            }
        };
        assertRule(() -> new TransactionTypeService(racing).update("02", "PAYMENTS", false),
                "BR-TT-040", Kind.NOT_FOUND);
        assertThat(service.update("02", "PAYMENTS", false).message()).isEqualTo("Changes committed to database");
    }

    @Test
    @DisplayName("BR-TT-041 SQLCODE -911 on update -> lock conflict")
    void br_tt_041_deadlock() {
        store.failNextMutationWith(SqlCodes.DEADLOCK_OR_TIMEOUT);
        assertRule(() -> service.update("02", "PAYMENTS", false), "BR-TT-041", Kind.LOCKED);
    }

    @Test
    @DisplayName("BR-TT-042 createIfMissing: UPDATE then INSERT on +100 (COTRTUPC 9600)")
    void br_tt_042_upsert() {
        var r = service.update("09", "NEW NINE", true);
        assertThat(r.created()).isTrue();
        assertThat(store.types).containsEntry("09", "NEW NINE");
    }

    @Test
    @DisplayName("BR-TT-050 delete requires explicit confirmation")
    void br_tt_050_deleteNeedsConfirm() {
        assertRule(() -> service.delete("02", false), "BR-TT-050", Kind.CONFIRMATION_REQUIRED);
        assertThat(store.types).containsKey("02");
        assertThat(service.delete("02", true).message()).isEqualTo("Delete successful.");
        assertThat(store.types).doesNotContainKey("02");
    }

    @Test
    @DisplayName("BR-TT-051 delete with child categories (-532) -> conflict")
    void br_tt_051_deleteRestricted() {
        assertRule(() -> service.delete("01", true), "BR-TT-051", Kind.CONFLICT);
    }

    @Test
    @DisplayName("BR-TT-052 delete of a row removed meanwhile (+100) -> not found")
    void br_tt_052_deleteLostRow() {
        TransactionTypeStore racing = new InMemoryTransactionTypeStore() {
            { types.put("03", "CREDIT"); }

            @Override
            public int delete(String code) {
                return 0;
            }
        };
        assertRule(() -> new TransactionTypeService(racing).delete("03", true), "BR-TT-052", Kind.NOT_FOUND);
    }

    @Test
    @DisplayName("BR-TT-060 any other negative SQLCODE -> DB error carrying the SQLCODE")
    void br_tt_060_otherSqlCode() {
        store.failNextMutationWith(-204);
        assertThatThrownBy(() -> service.update("02", "PAYMENTS", false))
                .isInstanceOfSatisfying(LegacyRuleException.class, e -> {
                    assertThat(e.ruleId()).isEqualTo("BR-TT-060");
                    assertThat(e.sqlCode()).isEqualTo(-204);
                });
        store.failNextMutationWith(-204);
        assertRule(() -> service.delete("03", true), "BR-TT-060", Kind.DB_ERROR);
        store.failNextMutationWith(-204);
        assertRule(() -> service.create("08", "X"), "BR-TT-060", Kind.DB_ERROR);
    }

    @Test
    @DisplayName("BR-TT-080/081 extract produces 60-byte TRANEXTR records")
    void br_tt_080_081_extract() {
        assertThat(service.extractTransactionTypes().get(0))
                .hasSize(60).isEqualTo("01" + String.format("%-50s", "PURCHASE") + "00000000");
        assertThat(service.extractCategories().get(0)).hasSize(60).startsWith("010001REGULAR SALES DRAFT");
    }
}
