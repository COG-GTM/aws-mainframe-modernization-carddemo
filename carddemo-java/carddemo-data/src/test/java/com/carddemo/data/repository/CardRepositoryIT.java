package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.entity.Card;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;

class CardRepositoryIT extends RepositoryIT {

    @Autowired
    CardRepository cards;

    @Test
    void readsByCardNumAndByAccountThroughCardaix() {
        cards.save(Fixtures.card("4000000000000101", 27L));
        cards.save(Fixtures.card("4000000000000202", 50L));
        cards.save(Fixtures.card("4000000000000201", 50L));
        flushAndClear();

        assertThat(cards.findById("4000000000000101")).get().extracting(Card::getAcctId).isEqualTo(27L);
        assertThat(cards.findByAcctIdOrderByCardNum(50L))
                .extracting(Card::getCardNum)
                .containsExactly("4000000000000201", "4000000000000202");
        assertThat(cards.findByAcctIdOrderByCardNum(99L)).isEmpty();
    }

    @Test
    void browsesForwardFromAKey() {
        for (int i = 1; i <= 5; i++) {
            cards.save(Fixtures.card("400000000000000" + i, i));
        }
        flushAndClear();

        assertThat(cards.findByCardNumGreaterThanEqualOrderByCardNum("4000000000000002", PageRequest.ofSize(2)))
                .extracting(Card::getCardNum)
                .containsExactly("4000000000000002", "4000000000000003");
    }
}
