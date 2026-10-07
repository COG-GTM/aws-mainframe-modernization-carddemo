package com.carddemo.card;

import static com.carddemo.support.BrowseAssertions.assertFullBrowse;
import static com.carddemo.support.BrowseAssertions.sorted;
import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.data.KeysetPage;
import com.carddemo.support.PostgresRepositoryTest;
import com.carddemo.support.Samples;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

/** CARDDATA access paths, including the COCRDLIC seven-row browse with and without the account filter. */
class CardRepositoryIT extends PostgresRepositoryTest {

    @Autowired
    CardRepository cards;

    List<CardRecord> sample;
    long busyAccount;
    List<String> allCardNums;

    @BeforeEach
    void load() {
        loadSamples(Dataset.CARDDATA);
        sample = CardRecord.MAPPER.fromRecords(Samples.read(Dataset.CARDDATA, RecordEncoding.EBCDIC));
        busyAccount = sample.get(17).acctId();
        List<String> nums = new ArrayList<>(sample.stream().map(CardRecord::cardNum).toList());
        for (int i = 1; i <= 15; i++) {
            String num = String.format("%d%015d", i % 9 + 1, i * 7_919L);
            cards.save(Card.from(new CardRecord(num, busyAccount, 123, "EXTRA CARD " + i, "2030-01-31",
                    i % 2 == 0 ? CardStatus.ACTIVE : CardStatus.INACTIVE)));
            nums.add(num);
        }
        flushAndClear();
        allCardNums = sorted(nums);
    }

    @Test
    void readsByKeyWithGeneratedDateAndStatusEnum() {
        CardRecord first = sample.get(0);
        Card card = cards.findById(first.cardNum()).orElseThrow();
        assertThat(card.toRecord()).isEqualTo(first);
        assertThat(card.getExpirationDateDt()).isEqualTo(LocalDate.parse(first.expirationDate()));
        assertThat(card.getVersion()).isZero();
        assertThat(cards.findAllByOrderByCardNumAsc()).extracting(Card::getCardNum)
                .containsExactlyElementsOf(allCardNums);
    }

    @Test
    void browsesSevenCardsPerScreenInCardNumberOrder() {
        assertThat(allCardNums).hasSize(65);
        assertFullBrowse(allCardNums, CardRepository.COCRDLIC_SCREEN_ROWS, Card::getCardNum, cards::browseFrom,
                cards::nextPage, cards::previousPage);
    }

    @Test
    void accountFilteredBrowseMatchesFilteringTheKeyOrderBrowse() {
        List<String> accountCards = cards.findAllByOrderByCardNumAsc().stream()
                .filter(c -> c.getAcctId() == busyAccount).map(Card::getCardNum).toList();
        assertThat(accountCards).hasSize(16);
        assertFullBrowse(accountCards, CardRepository.COCRDLIC_SCREEN_ROWS, Card::getCardNum,
                start -> cards.browseFrom(busyAccount, start), last -> cards.nextPage(busyAccount, last),
                first -> cards.previousPage(busyAccount, first));
        assertThat(cards.browseFrom(99_999_999_999L, "").rows()).isEmpty();
    }

    @Test
    void startbrIsGreaterOrEqual() {
        String existing = allCardNums.get(20);
        assertThat(cards.browseFrom(existing).first().getCardNum()).isEqualTo(existing);
        String missing = existing.substring(0, 15);
        KeysetPage<Card> page = cards.browseFrom(missing);
        assertThat(page.rows()).extracting(Card::getCardNum).containsExactlyElementsOf(
                allCardNums.stream().filter(n -> n.compareTo(missing) >= 0).limit(7).toList());
    }

    @Test
    void cardaixReadReturnsTheLowestCardOfTheAccount() {
        String lowest = allCardNums.stream().filter(n -> cards.findById(n).orElseThrow().getAcctId() == busyAccount)
                .findFirst().orElseThrow();
        assertThat(cards.findFirstByAcctIdOrderByCardNumAsc(busyAccount).orElseThrow().getCardNum())
                .isEqualTo(lowest);
        assertThat(cards.findFirstByAcctIdOrderByCardNumAsc(99_999_999_999L)).isEmpty();
    }

    @Test
    void rewriteRefreshesTheGeneratedDateAndVersion() {
        CardRecord first = sample.get(0);
        Card card = cards.findById(first.cardNum()).orElseThrow();
        card.update(new CardRecord(first.cardNum(), first.acctId(), first.cvvCd(), "NEW NAME", "2031-12-31",
                CardStatus.INACTIVE));
        cards.saveAndFlush(card);
        assertThat(card.getExpirationDateDt()).isEqualTo(LocalDate.of(2031, 12, 31));
        assertThat(card.getVersion()).isEqualTo(1);
        assertThat(jdbc.queryForObject("select active_status from card where card_num = ?", String.class,
                first.cardNum())).isEqualTo("N");
    }
}
