package com.carddemo.common.copybook;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.util.List;
import org.junit.jupiter.api.Test;

class CardRecordTest {

    @Test
    void layoutMatchesCvact02y() {
        // CVACT02Y: X(16) 9(11) 9(03) X(50) X(10) X(01) FILLER X(59) = 150
        assertThat(CardRecord.LENGTH).isEqualTo(150);
        assertThat(CardRecord.ACCT_ID.offset()).isEqualTo(16);
        assertThat(CardRecord.CVV_CD.offset()).isEqualTo(27);
        assertThat(CardRecord.EMBOSSED_NAME.offset()).isEqualTo(30);
        assertThat(CardRecord.EXPIRAION_DATE.offset()).isEqualTo(80);
        assertThat(CardRecord.ACTIVE_STATUS.offset()).isEqualTo(90);
    }

    @Test
    void seedDecodesCarddataTxt() {
        // app/data/ASCII/carddata.txt line 1
        List<String> seed = CardDataSeed.records();
        assertThat(seed).hasSize(50).allMatch(r -> r.length() == 150);
        CardRecord r = CardRecord.of(seed.getFirst());
        assertThat(r.cardNum()).isEqualTo("0500024453765740");
        assertThat(CardRecord.ACCT_ID.get(r)).isEqualTo(new BigDecimal("50"));
        assertThat(r.acctIdRaw()).isEqualTo("00000000050");
        assertThat(CardRecord.CVV_CD.get(r)).isEqualTo(new BigDecimal("747"));
        assertThat(r.embossedName()).startsWith("Aniya Von");
        assertThat(r.expirationDate()).isEqualTo("2023-03-09");
        assertThat(r.activeStatus()).isEqualTo("Y");
    }
}
