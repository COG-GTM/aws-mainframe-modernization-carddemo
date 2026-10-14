package com.carddemo.common.copybook;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.util.List;
import org.junit.jupiter.api.Test;

/** CVACT02Y layout and the CARDDAT seed (app/data/ASCII/carddata.txt). */
class CardRecordTest {

    @Test
    void layoutMatchesCvact02y() {
        // 16 + 11 + 3 + 50 + 10 + 1 + 59 = 150 (RECLN 150)
        assertThat(CardRecord.LENGTH).isEqualTo(150);
        assertThat(CardRecord.NUM.offset()).isZero();
        assertThat(CardRecord.ACCT_ID.offset()).isEqualTo(16);
        assertThat(CardRecord.CVV_CD.offset()).isEqualTo(27);
        assertThat(CardRecord.EMBOSSED_NAME.offset()).isEqualTo(30);
        assertThat(CardRecord.EXPIRAION_DATE.offset()).isEqualTo(80);
        assertThat(CardRecord.ACTIVE_STATUS.offset()).isEqualTo(90);
        assertThat(CardRecord.KEY_OFFSET).isZero();
        assertThat(CardRecord.KEY_LENGTH).isEqualTo(16);
    }

    @Test
    void seedDecodesFirstRecordOfCarddataTxt() {
        List<String> seed = CardDataSeed.records();
        assertThat(seed).hasSize(50).allMatch(r -> r.length() == 150);
        // carddata.txt line 1
        CardRecord r = CardRecord.of(seed.getFirst());
        assertThat(r.cardNum()).isEqualTo("0500024453765740");
        assertThat(r.acctIdRaw()).isEqualTo("00000000050");
        assertThat(CardRecord.ACCT_ID.get(r)).isEqualByComparingTo(BigDecimal.valueOf(50));
        assertThat(CardRecord.CVV_CD.get(r)).isEqualByComparingTo(BigDecimal.valueOf(747));
        assertThat(r.embossedName().trim()).isEqualTo("Aniya Von");
        assertThat(r.expirationDate()).isEqualTo("2023-03-09");
        assertThat(r.activeStatus()).isEqualTo("Y");
        assertThat(seed.stream().map(s -> s.substring(0, 16)).distinct()).hasSize(50);
    }
}
