package com.carddemo.common.copybook;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.util.List;
import org.junit.jupiter.api.Test;

/** CVACT01Y / CVACT02Y / CVACT03Y / CVCUS01Y / CSMSG02Y layouts and the app/data/ASCII seeds. */
class CardDemoRecordsTest {

    @Test
    void recordLengthsMatchCopybooks() {
        assertThat(AccountRecord.LENGTH).isEqualTo(300);
        assertThat(CardRecord.LENGTH).isEqualTo(150);
        assertThat(CardXrefRecord.LENGTH).isEqualTo(50);
        assertThat(CustomerRecord.LENGTH).isEqualTo(500);
        assertThat(AbendData.LENGTH).isEqualTo(134);
        assertThat(AccountRecord.GROUP_ID.offset()).isEqualTo(112);
        assertThat(CardXrefRecord.ACCT_ID.offset()).isEqualTo(CardXrefRecord.ACCT_AIX_OFFSET);
        assertThat(CustomerRecord.FICO_CREDIT_SCORE.offset()).isEqualTo(329);
        assertThat(CardRecord.ACTIVE_STATUS.offset()).isEqualTo(90);
    }

    @Test
    void accountSeedDecodesSignedZonedFields() {
        List<String> seed = AsciiSeedData.records(AsciiSeedData.ACCTDATA, AccountRecord.LENGTH);
        assertThat(seed).hasSize(50);
        AccountRecord a = AccountRecord.of(seed.getFirst());
        assertThat(a.acctId()).isEqualByComparingTo("1");
        assertThat(a.activeStatus()).isEqualTo("Y");
        assertThat(a.currBal()).isEqualTo(new BigDecimal("194.00"));
        assertThat(a.creditLimit()).isEqualTo(new BigDecimal("2020.00"));
        assertThat(a.openDate()).isEqualTo("2014-11-20");
    }

    @Test
    void xrefAndCustomerSeeds() {
        List<String> xref = AsciiSeedData.records(AsciiSeedData.CARDXREF, CardXrefRecord.LENGTH);
        assertThat(xref).hasSize(50).allSatisfy(r -> assertThat(r).hasSize(50));
        CardXrefRecord x = CardXrefRecord.of(xref.get(48));
        assertThat(x.cardNum()).isEqualTo("9680294154603697");
        assertThat(x.custId()).isEqualByComparingTo("1");
        assertThat(x.acctId()).isEqualByComparingTo("1");
        CustomerRecord c = CustomerRecord.of(
                AsciiSeedData.records(AsciiSeedData.CUSTDATA, CustomerRecord.LENGTH).getFirst());
        assertThat(c.firstName().trim()).isEqualTo("Immanuel");
        assertThat(c.ssn()).isEqualByComparingTo("20973888");
        assertThat(c.ficoCreditScore()).isEqualByComparingTo("274");
    }

    @Test
    void abendDataStartsAsSpaces() {
        AbendData d = AbendData.initial();
        assertThat(d.asString()).isBlank().hasSize(134);
    }
}
