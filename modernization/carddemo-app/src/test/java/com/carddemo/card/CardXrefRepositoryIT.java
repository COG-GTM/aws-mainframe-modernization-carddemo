package com.carddemo.card;

import static com.carddemo.support.BrowseAssertions.sorted;
import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.support.PostgresRepositoryTest;
import com.carddemo.support.Samples;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

/** CARDXREF access paths: by card number, CXACAIX by account, sequential. */
class CardXrefRepositoryIT extends PostgresRepositoryTest {

    @Autowired
    CardXrefRepository xrefs;

    @Autowired
    CardRepository cards;

    List<CardXrefRecord> sample;

    @BeforeEach
    void load() {
        loadSamples(Dataset.CUSTDATA, Dataset.ACCTDATA, Dataset.CARDDATA, Dataset.CARDXREF);
        jdbc.execute("set constraints all immediate");
        sample = CardXrefRecord.MAPPER.fromRecords(Samples.read(Dataset.CARDXREF, RecordEncoding.EBCDIC));
    }

    @Test
    void readsByCardNumberAndSequentially() {
        CardXrefRecord any = sample.get(9);
        assertThat(xrefs.findById(any.cardNum()).orElseThrow().toRecord()).isEqualTo(any);
        assertThat(xrefs.findAllByOrderByCardNumAsc()).extracting(CardXref::getCardNum)
                .containsExactlyElementsOf(sorted(sample.stream().map(CardXrefRecord::cardNum).toList()));
    }

    @Test
    void cxacaixReadsTheAccountsCardsInCardNumberOrder() {
        CardXrefRecord existing = sample.get(3);
        assertThat(xrefs.findFirstByAcctIdOrderByCardNumAsc(existing.acctId()).orElseThrow().toRecord())
                .isEqualTo(existing);

        String lower = "0000000000000001";
        cards.save(Card.from(new CardRecord(lower, existing.acctId(), 1, "SECOND CARD", "2030-01-31",
                CardStatus.ACTIVE)));
        xrefs.save(CardXref.from(new CardXrefRecord(lower, existing.custId(), existing.acctId())));
        flushAndClear();
        jdbc.execute("set constraints all immediate");

        assertThat(xrefs.findFirstByAcctIdOrderByCardNumAsc(existing.acctId()).orElseThrow().getCardNum())
                .isEqualTo(lower);
        assertThat(xrefs.findByAcctIdOrderByCardNumAsc(existing.acctId())).extracting(CardXref::getCardNum)
                .containsExactly(lower, existing.cardNum());
        assertThat(xrefs.findFirstByAcctIdOrderByCardNumAsc(99_999_999_999L)).isEmpty();
    }
}
