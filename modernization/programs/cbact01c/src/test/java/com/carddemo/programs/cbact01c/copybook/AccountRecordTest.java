package com.carddemo.programs.cbact01c.copybook;

import com.carddemo.programs.cbact01c.support.FieldDef;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AccountRecordTest {

    @Test
    void layoutMatchesCvact01yRecln300() {
        int offset = 0;
        for (FieldDef f : AccountRecord.FIELDS) {
            assertEquals(offset, f.offset(), f.cobolName());
            offset = f.end();
        }
        assertEquals(AccountRecord.LENGTH, offset);
    }

    @Test
    void parsesFirstSampleAccount() throws Exception {
        List<String> lines = Files.readAllLines(Path.of("src/test/resources/fixtures/acctdata.txt"),
                StandardCharsets.ISO_8859_1);
        AccountRecord r = AccountRecord.fromText(lines.get(0));
        assertEquals(1L, r.acctId());
        assertEquals("Y", r.acctActiveStatus());
        assertEquals(new BigDecimal("194.00"), r.acctCurrBal());
        assertEquals(new BigDecimal("2020.00"), r.acctCreditLimit());
        assertEquals(new BigDecimal("1020.00"), r.acctCashCreditLimit());
        assertEquals("2014-11-20", r.acctOpenDate());
        assertEquals("2025-05-20", r.acctExpiraionDate());
        assertEquals("2025-05-20", r.acctReissueDate());
        assertEquals(new BigDecimal("0.00"), r.acctCurrCycCredit());
        assertEquals(new BigDecimal("0.00"), r.acctCurrCycDebit());
        assertEquals("A000000000", r.acctAddrZip());
        assertEquals("          ", r.acctGroupId());
        assertEquals(lines.get(0), r.asText());
    }

    @Test
    void shortInputIsSpacePadded() {
        AccountRecord r = AccountRecord.fromText("00000000009N");
        assertEquals(300, r.length());
        assertEquals(" ".repeat(288), r.asText().substring(12));
    }

    @Test
    void builderEncodesTypedValues() {
        AccountRecord r = AccountRecord.of(42, "Y", new BigDecimal("-12.34"), new BigDecimal("500"),
                BigDecimal.ZERO, "2020-01-01", "2030-01-01", "2028-02-29", BigDecimal.ONE,
                new BigDecimal("7.5"), "12345", "GRP1");
        assertEquals("00000000042", r.text(AccountRecord.ACCT_ID));
        assertEquals("00000000123M", r.text(AccountRecord.ACCT_CURR_BAL));
        assertEquals("00000000075{", r.text(AccountRecord.ACCT_CURR_CYC_DEBIT));
        assertEquals("GRP1      ", r.acctGroupId());
        assertEquals(new BigDecimal("-12.34"), r.acctCurrBal());
    }
}
