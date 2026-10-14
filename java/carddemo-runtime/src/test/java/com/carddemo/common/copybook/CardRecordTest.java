package com.carddemo.common.copybook;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.util.List;
import org.junit.jupiter.api.Test;

/** CVACT02Y CARD-RECORD, CSMSG02Y ABEND-DATA and the CARDDAT seed. */
class CardRecordTest {

    @Test
    void cardRecordLayoutMatchesCopybook() {
        // CVACT02Y: 16 + 11 + 3 + 50 + 10 + 1 + FILLER 59 = 150
        assertThat(CardRecord.LENGTH).isEqualTo(150);
        assertThat(CardRecord.ACCT_ID.offset()).isEqualTo(16);
        assertThat(CardRecord.CVV_CD.offset()).isEqualTo(27);
        assertThat(CardRecord.EMBOSSED_NAME.offset()).isEqualTo(30);
        assertThat(CardRecord.EXPIRAION_DATE.offset()).isEqualTo(80);
        assertThat(CardRecord.ACTIVE_STATUS.offset()).isEqualTo(90);
        assertThat(CardRecord.LAYOUT.fields()).extracting("name").contains("CARD-NUM", "CARD-ACCT-ID",
                "CARD-CVV-CD", "CARD-EMBOSSED-NAME", "CARD-EXPIRAION-DATE", "CARD-ACTIVE-STATUS");
    }

    @Test
    void seedDecodesCarddataTxt() {
        List<String> records = CardDataSeed.records();
        assertThat(records).hasSize(50).allMatch(r -> r.length() == 150);
        CardRecord first = CardRecord.of(records.getFirst());
        assertThat(first.cardNum()).isEqualTo("0500024453765740");
        assertThat(CardRecord.ACCT_ID.get(first)).isEqualByComparingTo(BigDecimal.valueOf(50));
        assertThat(CardRecord.CVV_CD.get(first)).isEqualByComparingTo(BigDecimal.valueOf(747));
        assertThat(first.embossedName().stripTrailing()).isEqualTo("Aniya Von");
        assertThat(first.expiraionDate()).isEqualTo("2023-03-09");
        assertThat(first.activeStatus()).isEqualTo("Y");
    }

    @Test
    void abendDataStartsAsSpaces() {
        AbendData a = new AbendData();
        assertThat(AbendData.LENGTH).isEqualTo(134);
        assertThat(a.asString()).isBlank().hasSize(134);
        a.setCode("9999");
        a.setCulprit("COCRDSLC");
        assertThat(a.asString()).startsWith("9999COCRDSLC");
        assertThat(a.msg()).hasSize(72);
    }
}
