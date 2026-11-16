package com.carddemo.batch.jobs.creastmt;

import com.carddemo.batch.model.StatementTransactionRecord;
import com.carddemo.batch.model.StatementTransactionRecord.TrnxKey;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class CardTransactionTableTest {

    @Test
    void groupsTransactionsByCardInKeyOrder() {
        CardTransactionTable table = new CardTransactionTable(List.of(
                trnx("1111", "A"), trnx("1111", "B"), trnx("2222", "C")));

        assertThat(table.forCard("1111")).extracting(t -> t.trnxKey().trnxId()).containsExactly("A", "B");
        assertThat(table.forCard("2222")).extracting(t -> t.trnxKey().trnxId()).containsExactly("C");
        assertThat(table.forCard("0000")).isEmpty();
    }

    @Test
    void rejectsMoreThanTenTransactionsPerCard() {
        List<StatementTransactionRecord> trans = new ArrayList<>();
        for (int i = 0; i < 11; i++) {
            trans.add(trnx("1111", String.format("%02d", i)));
        }
        assertThatThrownBy(() -> new CardTransactionTable(trans)).hasMessageContaining("WS-TRAN-TBL");
    }

    @Test
    void rejectsMoreThanFiftyOneCards() {
        List<StatementTransactionRecord> trans = new ArrayList<>();
        for (int i = 0; i < 52; i++) {
            trans.add(trnx(String.format("%04d", i), "A"));
        }
        assertThatThrownBy(() -> new CardTransactionTable(trans)).hasMessageContaining("WS-CARD-TBL");
    }

    @Test
    void editsAmountsLikeCobolPictures() {
        assertThat(Cobol.edited(new BigDecimal("492.00"), 9, false)).isEqualTo("000000492.00 ");
        assertThat(Cobol.edited(new BigDecimal("-47.88"), 9, true)).isEqualTo("       47.88-");
        assertThat(Cobol.edited(new BigDecimal("0.05"), 9, true)).isEqualTo("         .05 ");
        assertThat(Cobol.edited(new BigDecimal("12345678901.23"), 9, false)).isEqualTo("345678901.23 ");
    }

    private static StatementTransactionRecord trnx(String card, String id) {
        return new StatementTransactionRecord(new TrnxKey(card, id), "01", 1, "POS", "desc",
                BigDecimal.ONE, 0, "", "", "", "", "", "");
    }
}
