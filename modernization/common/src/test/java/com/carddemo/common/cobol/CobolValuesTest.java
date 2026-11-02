package com.carddemo.common.cobol;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;

class CobolValuesTest {

    /** First record of app/data/ASCII/acctdata.txt (ACCOUNT-RECORD, CVACT01Y), FILLER cut. */
    private static final String ACCOUNT_RECORD =
            "00000000001Y00000001940{00000020200{00000010200{2014-11-202025-05-202025-05-20"
                    + "00000000000{00000000000{A000000000";

    @Test
    void truncatesInsteadOfRounding() {
        assertThat(CobolValues.truncate(new BigDecimal("12.3456"), 2)).isEqualByComparingTo("12.34");
        assertThat(CobolValues.truncate(new BigDecimal("-12.3456"), 2)).isEqualByComparingTo("-12.34");
        assertThat(CobolValues.divide(new BigDecimal("14814.7200"), new BigDecimal("1200"), 2))
                .isEqualByComparingTo("12.34");
        assertThat(CobolValues.divide(new BigDecimal("-14814.7200"), new BigDecimal("1200"), 2))
                .isEqualByComparingTo("-12.34");
    }

    @Test
    void trimsOnLoadAndPadsOnOutput() {
        assertThat(CobolValues.trim("POS TERM  ")).isEqualTo("POS TERM");
        assertThat(CobolValues.trim("          ")).isNull();
        assertThat(CobolValues.padRight("POS TERM", 10)).isEqualTo("POS TERM  ");
        assertThat(CobolValues.padRight(null, 3)).isEqualTo("   ");
        assertThat(CobolValues.padRight("ABCDEFGHIJK", 10)).isEqualTo("ABCDEFGHIJ");
    }

    @Test
    void keepsLeadingZerosOfNumericIds() {
        assertThat(CobolValues.zeroPad(1L, 11)).isEqualTo("00000000001");
        assertThat(CobolValues.zeroPad(1L, 9)).isEqualTo("000000001");
        assertThatThrownBy(() -> CobolValues.zeroPad(1_000_000_000L, 9)).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> CobolValues.zeroPad(-1L, 9)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void decodesSignOverpunch() {
        assertThat(CobolValues.decodeZoned("00000001940{", 2)).isEqualByComparingTo("194.00");
        assertThat(CobolValues.decodeZoned("0000005047G", 2)).isEqualByComparingTo("504.77");
        assertThat(CobolValues.decodeZoned("0000005047P", 2)).isEqualByComparingTo("-504.77");
        assertThat(CobolValues.decodeZoned("0000000000}", 2)).isEqualByComparingTo("0.00");
        assertThat(CobolValues.decodeZoned("100000001", 0)).isEqualByComparingTo("100000001");
        assertThat(CobolValues.encodeZoned(new BigDecimal("504.77"), 9, 2)).isEqualTo("0000005047G");
        assertThat(CobolValues.encodeZoned(new BigDecimal("-504.779"), 9, 2)).isEqualTo("0000005047P");
        assertThat(CobolValues.encodeZoned(BigDecimal.ZERO, 10, 2)).isEqualTo("00000000000{");
        assertThatThrownBy(() -> CobolValues.decodeZoned("00000001940X", 2))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void decodesPackedDecimal() {
        // PIC S9(09)V99 COMP-3 (6 bytes): +504.77 and -504.77
        assertThat(CobolValues.decodePacked(bytes(0x00, 0x00, 0x00, 0x50, 0x47, 0x7C), 2))
                .isEqualByComparingTo("504.77");
        assertThat(CobolValues.decodePacked(bytes(0x00, 0x00, 0x00, 0x50, 0x47, 0x7D), 2))
                .isEqualByComparingTo("-504.77");
        // PIC 9(03) COMP-3 unsigned (F sign nibble)
        assertThat(CobolValues.decodePacked(bytes(0x78, 0x1F), 0)).isEqualByComparingTo("781");
        assertThatThrownBy(() -> CobolValues.decodePacked(bytes(0x7A, 0x1C), 0))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void decodesBinary() {
        // PIC 9(09) COMP is a 4 byte big-endian fullword
        assertThat(CobolValues.decodeBinary(bytes(0x05, 0xF5, 0xE1, 0x01), false, 0))
                .isEqualByComparingTo("100000001");
        // PIC S9(10)V99 COMP is an 8 byte doubleword, two's complement when negative
        assertThat(CobolValues.decodeBinary(bytes(0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE, 0x0C), true, 2))
                .isEqualByComparingTo("-5.00");
    }

    @Test
    void workedExampleAccountRecord() {
        assertThat(CobolValues.zeroPad(Long.parseLong(ACCOUNT_RECORD.substring(0, 11)), 11)).isEqualTo("00000000001");
        assertThat(ACCOUNT_RECORD.substring(11, 12)).isEqualTo("Y");
        assertThat(CobolValues.decodeZoned(ACCOUNT_RECORD.substring(12, 24), 2)).isEqualByComparingTo("194.00");
        assertThat(CobolValues.decodeZoned(ACCOUNT_RECORD.substring(24, 36), 2)).isEqualByComparingTo("2020.00");
        assertThat(CobolValues.decodeZoned(ACCOUNT_RECORD.substring(36, 48), 2)).isEqualByComparingTo("1020.00");
        assertThat(CobolValues.date(ACCOUNT_RECORD.substring(48, 58))).isEqualTo(LocalDate.of(2014, 11, 20));
        assertThat(CobolValues.date(ACCOUNT_RECORD.substring(58, 68))).isEqualTo(LocalDate.of(2025, 5, 20));
        assertThat(CobolValues.decodeZoned(ACCOUNT_RECORD.substring(78, 90), 2)).isEqualByComparingTo("0.00");
    }

    @Test
    void workedExampleTranRecordRoundTrip() {
        String record = CobolValues.padRight("0000000000683580", 16)
                + "01" + "0001"
                + CobolValues.padRight("POS TERM", 10)
                + CobolValues.padRight("Purchase at Abshire-Lowe", 100)
                + CobolValues.encodeZoned(new BigDecimal("504.77"), 9, 2)
                + CobolValues.zeroPad(800000000L, 9)
                + CobolValues.padRight("Abshire-Lowe", 50)
                + CobolValues.padRight("North Enoshaven", 50)
                + CobolValues.padRight("72112", 10)
                + "4859452612877065"
                + CobolValues.padRight("2022-06-10 19:27:53.000000", 26)
                + CobolValues.padRight(null, 26);

        assertThat(record).hasSize(330);
        assertThat(record.substring(132, 143)).isEqualTo("0000005047G");
        assertThat(CobolValues.decodeZoned(record.substring(132, 143), 2)).isEqualByComparingTo("504.77");
        assertThat(CobolValues.trim(record.substring(22, 32))).isEqualTo("POS TERM");
        assertThat(CobolValues.timestamp(record.substring(278, 304)))
                .isEqualTo(LocalDateTime.of(2022, 6, 10, 19, 27, 53));
        assertThat(CobolValues.timestamp(record.substring(304, 330))).isNull();
    }

    private static byte[] bytes(int... values) {
        byte[] result = new byte[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = (byte) values[i];
        }
        return result;
    }
}
