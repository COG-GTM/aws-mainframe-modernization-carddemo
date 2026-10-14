package com.carddemo.common.copybook;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.FileResult;
import com.carddemo.runtime.vsam.KsdsFile;
import java.math.BigDecimal;
import java.util.List;
import org.junit.jupiter.api.Test;

class AccountCopybooksTest {

    @Test
    void layoutsMatchCopybooks() {
        // CVACT01Y: 11+1+12*3+10*3+12*2+10+10+178 = 300
        assertThat(AccountRecord.LENGTH).isEqualTo(300);
        assertThat(AccountRecord.CURR_BAL.offset()).isEqualTo(12);
        assertThat(AccountRecord.OPEN_DATE.offset()).isEqualTo(48);
        assertThat(AccountRecord.CURR_CYC_CREDIT.offset()).isEqualTo(78);
        assertThat(AccountRecord.GROUP_ID.offset()).isEqualTo(112);
        // CVCUS01Y: 500, CUST-SSN at 9+75+150+2+3+10+30 = 279
        assertThat(CustomerRecord.LENGTH).isEqualTo(500);
        assertThat(CustomerRecord.SSN.offset()).isEqualTo(279);
        assertThat(CustomerRecord.FICO_CREDIT_SCORE.offset()).isEqualTo(329);
        // CVACT03Y: 16+9+11+14 = 50; XREF-ACCT-ID at 25 = the AIX KEYS(11,25)
        assertThat(CardXrefRecord.LENGTH).isEqualTo(50);
        assertThat(CardXrefRecord.ACCT_ID.offset()).isEqualTo(CardXrefRecord.ACCT_AIX_OFFSET);
        // CSMSG02Y: 4+8+50+72 = 134, VALUE SPACES
        assertThat(AbendData.LENGTH).isEqualTo(134);
        assertThat(new AbendData().asString()).isBlank().hasSize(134);
    }

    @Test
    void seedDatasetsDecode() {
        List<String> accounts = CardDemoDatasets.accounts();
        assertThat(accounts).hasSize(50).allMatch(r -> r.length() == 300);
        AccountRecord a = AccountRecord.of(accounts.getFirst());
        assertThat(AccountRecord.ACCT_ID.getLong(a)).isEqualTo(1);
        assertThat(AccountRecord.CURR_BAL.get(a)).isEqualByComparingTo(new BigDecimal("194.00"));
        assertThat(AccountRecord.CREDIT_LIMIT.get(a)).isEqualByComparingTo(new BigDecimal("2020.00"));
        assertThat(AccountRecord.OPEN_DATE.get(a)).isEqualTo("2014-11-20");

        CustomerRecord c = CustomerRecord.of(CardDemoDatasets.customers().getFirst());
        assertThat(CustomerRecord.FIRST_NAME.get(c).trim()).isEqualTo("Immanuel");
        assertThat(CustomerRecord.SSN.raw(c)).isEqualTo("020973888");
        assertThat(CustomerRecord.FICO_CREDIT_SCORE.getLong(c)).isEqualTo(274);

        List<String> xrefs = CardDemoDatasets.cardXrefs();
        assertThat(xrefs).hasSize(50).allMatch(r -> r.length() == 50);
        CardXrefRecord x = CardXrefRecord.of(xrefs.get(48));
        assertThat(CardXrefRecord.CARD_NUM.get(x)).isEqualTo("9680294154603697");
        assertThat(CardXrefRecord.ACCT_ID.getLong(x)).isEqualTo(1);
    }

    @Test
    void fileControlReadsThroughRegisteredAlternateIndexPath() {
        KsdsFile xref = KsdsFile.inMemory("CCXREF", 50, 0, 16);
        xref.load(CardDemoDatasets.cardXrefs());
        FileControl fct = new FileControl().register(xref).register(xref.path("CXACAIX", 25, 11, false));
        FileResult r = fct.read("CXACAIX ", "00000000001", 50);
        assertThat(r.resp()).isEqualTo(CicsResp.NORMAL);
        assertThat(r.dataString()).startsWith("9680294154603697");
        assertThat(fct.read("CXACAIX", "99999999999", 50).resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(fct.read("CXACAIX", "1", 50).resp()).isEqualTo(CicsResp.INVREQ);
        assertThat(fct.read("CXACAIX", "00000000001", 10).resp()).isEqualTo(CicsResp.LENGERR);
        assertThat(fct.path("CXACAIX")).isPresent();
        // base cluster still reachable by primary key
        assertThat(fct.read("CCXREF", "9680294154603697", 50).isNormal()).isTrue();
        // browse through the registered path
        assertThat(fct.startbr("CXACAIX", "00000000049").isNormal()).isTrue();
        assertThat(fct.readnext("CXACAIX").dataString().substring(25, 36)).isEqualTo("00000000049");
        assertThat(fct.readnext("CXACAIX").dataString().substring(25, 36)).isEqualTo("00000000050");
        assertThat(fct.readnext("CXACAIX").resp()).isEqualTo(CicsResp.ENDFILE);
        assertThat(fct.endbr("CXACAIX").isNormal()).isTrue();
        xref.write("0000000000000001" + "000000001" + "00000000001" + " ".repeat(14));
        assertThat(fct.read("CXACAIX", "00000000001", 50).resp()).isEqualTo(CicsResp.DUPKEY);
    }
}
