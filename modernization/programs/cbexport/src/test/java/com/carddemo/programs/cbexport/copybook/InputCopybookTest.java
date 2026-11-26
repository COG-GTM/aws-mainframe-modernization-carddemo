package com.carddemo.programs.cbexport.copybook;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.math.BigDecimal;
import java.util.Arrays;
import org.junit.jupiter.api.Test;

/** Field offsets of the five input copybooks, pinned against the first row of each app/data/ASCII file. */
class InputCopybookTest {

    static byte[] rec(String line, int length) {
        byte[] r = new byte[length];
        Arrays.fill(r, (byte) ' ');
        byte[] src = line.getBytes(CobolFieldCodec.CHARSET);
        System.arraycopy(src, 0, r, 0, src.length);
        return r;
    }

    static final String CUST_LINE = "000000001Immanuel                 Madeline                 Kessler                  "
            + "618 Deshaun Route                                 Apt. 802                                          "
            + "Altenwerthshire                                   NCUSA12546     (908)119-8310  (373)693-8684  "
            + "020973888000000000000493684371961-06-080053581756Y274";

    @Test
    void cvcus01yCustomerRecord() {
        byte[] r = rec(CUST_LINE, CustomerRecord.LENGTH);
        CustomerRecord c = CustomerRecord.parse(r);
        assertEquals(1, c.custId());
        assertEquals("Immanuel", c.firstName().strip());
        assertEquals("Madeline", c.middleName().strip());
        assertEquals("Kessler", c.lastName().strip());
        assertEquals("618 Deshaun Route", c.addrLine1().strip());
        assertEquals("Apt. 802", c.addrLine2().strip());
        assertEquals("Altenwerthshire", c.addrLine3().strip());
        assertEquals("NC", c.addrStateCd());
        assertEquals("USA", c.addrCountryCd());
        assertEquals("12546     ", c.addrZip());
        assertEquals("(908)119-8310  ", c.phoneNum1());
        assertEquals("(373)693-8684  ", c.phoneNum2());
        assertEquals(20973888L, c.ssn());
        assertEquals("00000000000049368437", c.govtIssuedId());
        assertEquals("1961-06-08", c.dobYyyyMmDd());
        assertEquals("0053581756", c.eftAccountId());
        assertEquals("Y", c.priCardHolderInd());
        assertEquals(274, c.ficoCreditScore());
        assertEquals(168, c.filler().length());
        assertArrayEquals(r, c.toBytes());
    }

    @Test
    void cvact01yAccountRecordWithOverpunchedAmounts() {
        String line = "00000000001Y00000001940{00000020200{00000010200{2014-11-202025-05-202025-05-20"
                + "00000000000{00000000000{A000000000";
        byte[] r = rec(line, AccountRecord.LENGTH);
        AccountRecord a = AccountRecord.parse(r);
        assertEquals(1, a.acctId());
        assertEquals("Y", a.activeStatus());
        assertEquals(new BigDecimal("194.00"), a.currBal());
        assertEquals(new BigDecimal("2020.00"), a.creditLimit());
        assertEquals(new BigDecimal("1020.00"), a.cashCreditLimit());
        assertEquals("2014-11-20", a.openDate());
        assertEquals("2025-05-20", a.expiraionDate());
        assertEquals("2025-05-20", a.reissueDate());
        assertEquals(new BigDecimal("0.00"), a.currCycCredit());
        assertEquals(new BigDecimal("0.00"), a.currCycDebit());
        assertEquals("A000000000", a.addrZip());
        assertEquals("          ", a.groupId());
        assertArrayEquals(r, a.toBytes());
    }

    @Test
    void cvact03yXrefShortLineIsSpacePadded() {
        byte[] r = rec("050002445376574000000005000000000050", CardXrefRecord.LENGTH);
        CardXrefRecord x = CardXrefRecord.parse(r);
        assertEquals("0500024453765740", x.cardNum());
        assertEquals(50, x.custId());
        assertEquals(50, x.acctId());
        assertEquals(" ".repeat(14), x.filler());
        assertArrayEquals(r, x.toBytes());
    }

    @Test
    void cvtra05yTransactionRecordNegativeAmount() {
        String line = "0000000001774260030001OPERATOR  Return item at Nitzsche, Nicolas and Lowe"
                + " ".repeat(59) + "0000009190}800000000Nitzsche, Nicolas and Lowe" + " ".repeat(24)
                + "Fidelshire" + " ".repeat(40) + "53378     0927987108636232" + "2022-06-10 19:27:53.000000";
        byte[] r = rec(line, TranRecord.LENGTH);
        TranRecord t = TranRecord.parse(r);
        assertEquals("0000000001774260", t.tranId());
        assertEquals("03", t.typeCd());
        assertEquals(1, t.catCd());
        assertEquals("OPERATOR  ", t.source());
        assertEquals("Return item at Nitzsche, Nicolas and Lowe", t.desc().strip());
        assertEquals(new BigDecimal("-919.00"), t.amt());
        assertEquals(800000000L, t.merchantId());
        assertEquals("Nitzsche, Nicolas and Lowe", t.merchantName().strip());
        assertEquals("Fidelshire", t.merchantCity().strip());
        assertEquals("53378     ", t.merchantZip());
        assertEquals("0927987108636232", t.cardNum());
        assertEquals("2022-06-10 19:27:53.000000", t.origTs());
        assertEquals(" ".repeat(26), t.procTs());
        assertArrayEquals(r, t.toBytes());
    }

    @Test
    void cvact02yCardRecord() {
        byte[] r = rec("050002445376574000000000050747Aniya Von" + " ".repeat(41) + "2023-03-09Y", CardRecord.LENGTH);
        CardRecord c = CardRecord.parse(r);
        assertEquals("0500024453765740", c.cardNum());
        assertEquals(50, c.acctId());
        assertEquals(747, c.cvvCd());
        assertEquals("Aniya Von", c.embossedName().strip());
        assertEquals("2023-03-09", c.expiraionDate());
        assertEquals("Y", c.activeStatus());
        assertArrayEquals(r, c.toBytes());
    }
}
