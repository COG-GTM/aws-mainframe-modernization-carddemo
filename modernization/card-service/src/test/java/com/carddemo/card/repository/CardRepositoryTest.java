package com.carddemo.card.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.card.domain.Card;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.transaction.annotation.Transactional;

@SpringBootTest
@Transactional
class CardRepositoryTest {

    @Autowired
    private CardRepository cards;

    @Autowired
    private CardXrefRepository xrefs;

    @BeforeEach
    void setUp() {
        xrefs.deleteAll();
        cards.deleteAll();
        for (String num : List.of("0500024453765740", "0683586198171516", "0923877193247330", "1014086565224350")) {
            cards.save(new Card(num, 1L));
        }
    }

    @Test
    void browsesForwardFromStartKeyInKeyOrder() {
        assertThat(cards.findByCardNumGreaterThanEqualOrderByCardNumAsc("0683586198171516", PageRequest.ofSize(2)))
                .extracting(Card::getCardNum).containsExactly("0683586198171516", "0923877193247330");
    }

    @Test
    void browsesBackwardFromKeyNearestFirst() {
        assertThat(cards.findByCardNumLessThanOrderByCardNumDesc("0923877193247330", PageRequest.ofSize(10)))
                .extracting(Card::getCardNum).containsExactly("0683586198171516", "0500024453765740");
    }
}
