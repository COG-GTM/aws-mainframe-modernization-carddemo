package com.carddemo.common.copybook;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.runtime.vsam.KsdsFile;
import java.math.BigDecimal;
import java.util.List;
import org.junit.jupiter.api.Test;

class CardRecordTest {

    @Test
    void cardRecordMatchesCvact02y() {
        // CVACT02Y: 16 + 11 + 3 + 50 + 10 + 1 + FILLER 59 = 150
        assertThat(CardRecord.LENGTH).isEqualTo(150);
        assertThat(CardRecord.ACCT_ID.offset()).isEqualTo(16);
        assertThat(CardRecord.CVV_CD.offset()).isEqualTo(27);
        assertThat(CardRecord.EMBOSSED_NAME.offset()).isEqualTo(30);
        assertThat(CardRecord.EXPIRAION_DATE.offset()).isEqualTo(80);
        assertThat(CardRecord.ACTIVE_STATUS.offset()).isEqualTo(90);
    }

    @Test
    void seedIsCarddataTxt() {
        // app/data/ASCII/carddata.txt: 50 records; line 1 = card 0500024453765740, acct 50, CVV 747, Aniya Von
        List<String> records = CardDataSeed.records();
        assertThat(records).hasSize(50).allMatch(r -> r.length() == 150);
        CardRecord first = CardRecord.of(records.getFirst());
        assertThat(first.cardNum()).isEqualTo("0500024453765740");
        assertThat(CardRecord.ACCT_ID.get(first)).isEqualTo(new BigDecimal(50));
        assertThat(CardRecord.CVV_CD.get(first)).isEqualTo(new BigDecimal(747));
        assertThat(first.embossedName().trim()).isEqualTo("Aniya Von");
        assertThat(first.expirationDate()).isEqualTo("2023-03-09");
        assertThat(first.activeStatus()).isEqualTo("Y");

        KsdsFile carddat = KsdsFile.inMemory(CardDataSeed.FILE_NAME, CardRecord.LENGTH, CardRecord.KEY_OFFSET,
                CardRecord.KEY_LENGTH);
        assertThat(carddat.load(records)).isEqualTo(50);
        assertThat(carddat.path(CardDataSeed.AIX_PATH_NAME, CardRecord.ACCT_ID_OFFSET, CardRecord.ACCT_ID_LENGTH, false)
                .read("00000000050").isNormal()).isTrue();
    }

    @Test
    void abendDataMatchesCsmsg02y() {
        // CSMSG02Y: 4 + 8 + 50 + 72 = 134, all VALUE SPACES
        AbendData a = new AbendData();
        assertThat(AbendData.LENGTH).isEqualTo(134);
        assertThat(a.asString()).isBlank().hasSize(134);
        assertThat(AbendData.MSG.offset()).isEqualTo(62);
    }
}
