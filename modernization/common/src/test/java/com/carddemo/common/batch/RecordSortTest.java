package com.carddemo.common.batch;

import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.layout.TranRecordLayout;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class RecordSortTest {

    private static CobolRecord tran(CodePage codePage, String id, String card) {
        return CobolRecord.initialized(TranRecordLayout.LAYOUT, codePage)
                .set(TranRecordLayout.TRAN_ID, id)
                .set(TranRecordLayout.TRAN_CARD_NUM, card);
    }

    @Test
    void combtranConcatenatesSortinsAndSortsByTranId() {
        // COMBTRAN STEP05R: SORTIN=TRANSACT.BKUP(0)+SYSTRAN(0), SORT FIELDS=(TRAN-ID,A)
        List<CobolRecord> backup = List.of(tran(CodePage.ASCII, "0000000000000003", "C1"),
                tran(CodePage.ASCII, "0000000000000001", "C1"));
        List<CobolRecord> systran = List.of(tran(CodePage.ASCII, "0000000000000002", "C2"));

        List<CobolRecord> sorted = RecordSort.fields(RecordSort.Key.ascending(TranRecordLayout.TRAN_ID))
                .sort(List.of(backup, systran), TranRecordLayout.LAYOUT);

        assertThat(sorted).extracting(r -> r.get(TranRecordLayout.TRAN_ID))
                .containsExactly("0000000000000001", "0000000000000002", "0000000000000003");
    }

    @Test
    void creastmtStep010SortsByCardThenIdAndMovesTheCardToTheFront() {
        // CREASTMT STEP010: SORT FIELDS=(263,16,CH,A,1,16,CH,A)
        //                   OUTREC FIELDS=(1:263,16,17:1,262,279:279,50) -> TRXFL, LRECL 350
        List<CobolRecord> transact = List.of(
                tran(CodePage.ASCII, "0000000000000002", "4444333322221111"),
                tran(CodePage.ASCII, "0000000000000003", "1111222233334444"),
                tran(CodePage.ASCII, "0000000000000001", "4444333322221111"));
        RecordSort sort = RecordSort.fields(RecordSort.Key.ascending(263, 16), RecordSort.Key.ascending(1, 16))
                .outrec(new RecordSort.OutrecField(1, 263, 16), new RecordSort.OutrecField(17, 1, 262),
                        new RecordSort.OutrecField(279, 279, 50));

        List<byte[]> out = sort.sort(transact.stream().map(CobolRecord::toBytes).toList(), 350, (byte) ' ');

        assertThat(out).extracting(b -> new String(b, 0, 32, StandardCharsets.ISO_8859_1)).containsExactly(
                "11112222333344440000000000000003",
                "44443333222211110000000000000001",
                "44443333222211110000000000000002");
        assertThat(out).allSatisfy(b -> assertThat(b).hasSize(350));
    }

    @Test
    void includeFiltersBeforeSortingAndEbcdicKeysCompareAsEbcdic() {
        // In EBCDIC digits (F0-F9) sort after letters (C1-E9); in ASCII before.
        List<CobolRecord> ebcdic = List.of(tran(CodePage.EBCDIC, "1", "X"), tran(CodePage.EBCDIC, "A", "X"),
                tran(CodePage.EBCDIC, "B", "SKIP"));

        List<CobolRecord> sorted = RecordSort.fields(RecordSort.Key.ascending(TranRecordLayout.TRAN_ID))
                .include(bytes -> !CobolRecord.of(TranRecordLayout.LAYOUT, CodePage.EBCDIC, bytes)
                        .getTrimmed(TranRecordLayout.TRAN_CARD_NUM).equals("SKIP"))
                .sort(List.of(ebcdic), TranRecordLayout.LAYOUT);

        assertThat(sorted).extracting(r -> r.getTrimmed(TranRecordLayout.TRAN_ID)).containsExactly("A", "1");
    }
}
