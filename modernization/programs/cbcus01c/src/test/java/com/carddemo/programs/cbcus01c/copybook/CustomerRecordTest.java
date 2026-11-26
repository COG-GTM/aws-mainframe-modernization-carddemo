package com.carddemo.programs.cbcus01c.copybook;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;

class CustomerRecordTest {

    private static final Path CUSTDATA = Path.of(System.getProperty("user.dir"))
            .resolve("../../../app/data/ASCII/custdata.txt").normalize();

    @Test
    void fieldLengthsSumToRecln500() {
        int sum = CustomerRecord.CUST_ID_LEN + CustomerRecord.CUST_FIRST_NAME_LEN
                + CustomerRecord.CUST_MIDDLE_NAME_LEN + CustomerRecord.CUST_LAST_NAME_LEN
                + 3 * CustomerRecord.CUST_ADDR_LINE_LEN + CustomerRecord.CUST_ADDR_STATE_CD_LEN
                + CustomerRecord.CUST_ADDR_COUNTRY_CD_LEN + CustomerRecord.CUST_ADDR_ZIP_LEN
                + 2 * CustomerRecord.CUST_PHONE_NUM_LEN + CustomerRecord.CUST_SSN_LEN
                + CustomerRecord.CUST_GOVT_ISSUED_ID_LEN + CustomerRecord.CUST_DOB_LEN
                + CustomerRecord.CUST_EFT_ACCOUNT_ID_LEN + CustomerRecord.CUST_PRI_CARD_HOLDER_IND_LEN
                + CustomerRecord.CUST_FICO_CREDIT_SCORE_LEN + CustomerRecord.FILLER_LEN;
        assertEquals(CustomerRecord.RECORD_LENGTH, sum);
    }

    @Test
    void parsesFirstSampleRecordAtCopybookOffsets() throws IOException {
        String image = Files.readAllLines(CUSTDATA, StandardCharsets.US_ASCII).get(0);
        CustomerRecord r = CustomerRecord.fromFixedWidth(image);
        assertEquals(1L, r.custId());
        assertEquals("Immanuel", r.custFirstName().strip());
        assertEquals(25, r.custFirstName().length());
        assertEquals("Kessler", r.custLastName().strip());
        assertEquals("NC", r.custAddrStateCd());
        assertEquals("USA", r.custAddrCountryCd());
        assertEquals("12546     ", r.custAddrZip());
        assertEquals("(908)119-8310  ", r.custPhoneNum1());
        assertEquals(20973888L, r.custSsn());
        assertEquals("1961-06-08", r.custDobYyyyMmDd());
        assertEquals("Y", r.custPriCardHolderInd());
        assertEquals(274, r.custFicoCreditScore());
        assertEquals(" ".repeat(168), r.filler());
    }

    @Test
    void everySampleRecordRoundTripsByteForByte() throws IOException {
        List<String> lines = Files.readAllLines(CUSTDATA, StandardCharsets.US_ASCII);
        assertEquals(50, lines.size());
        for (String line : lines) {
            assertEquals(line, CustomerRecord.fromFixedWidth(line).toFixedWidth());
        }
    }

    @Test
    void numericFieldsSerialiseZeroFilled() {
        CustomerRecord r = new CustomerRecord(42, "", "", "", "", "", "", "", "", "", "", "", 7, "", "", "", "", 5, "");
        String image = r.toFixedWidth();
        assertEquals(500, image.length());
        assertEquals("000000042", image.substring(0, 9));
        assertEquals("000000007", image.substring(279, 288));
        assertEquals("005", image.substring(329, 332));
    }

    @Test
    void shortImageIsSpacePaddedAndLongImageTruncated() throws IOException {
        assertEquals("000000001Ann" + " ".repeat(267) + "000000000" + " ".repeat(41) + "000" + " ".repeat(168),
                CustomerRecord.fromFixedWidth("000000001Ann" + " ".repeat(267) + "000000000" + " ".repeat(41) + "000")
                        .toFixedWidth());
        String first = Files.readAllLines(CUSTDATA, StandardCharsets.US_ASCII).get(0);
        assertEquals(first, CustomerRecord.fromFixedWidth(first + "OVERFLOW").toFixedWidth());
    }

    @Test
    void nonNumericPic9IsRejected() {
        IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
                () -> CustomerRecord.fromFixedWidth("00000000A"));
        assertEquals("CUST-ID is not NUMERIC", e.getMessage());
    }

    @Test
    void picXMoveTruncatesAndPads() {
        assertEquals("AB ", CustomerRecord.pic("AB", 3));
        assertEquals("ABC", CustomerRecord.pic("ABCD", 3));
        assertEquals("   ", CustomerRecord.pic(null, 3));
    }

    @Test
    void constructorEnforcesPicSizes() {
        assertThrows(IllegalArgumentException.class, () -> new CustomerRecord(1_000_000_000L, "", "", "", "", "", "",
                "", "", "", "", "", 0, "", "", "", "", 0, ""));
        assertThrows(IllegalArgumentException.class, () -> new CustomerRecord(1, "", "", "", "", "", "",
                "", "", "", "", "", 0, "", "", "", "", 1000, ""));
        CustomerRecord r = new CustomerRecord(1, "A".repeat(30), "", "", "", "", "",
                "", "", "", "", "", 0, "", "", "", "", 0, "");
        assertEquals("A".repeat(25), r.custFirstName());
    }
}
