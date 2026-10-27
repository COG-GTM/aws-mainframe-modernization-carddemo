package com.carddemo.transaction.cobol;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class CobolSemanticsTest {

    @Test
    void picXMovePadsWithSpacesAndTruncatesOnTheRight() {
        assertThat(Pic.x("AB", 5)).isEqualTo("AB   ");
        assertThat(Pic.x("ABCDEFG", 3)).isEqualTo("ABC");
        assertThat(Pic.x(null, 2)).isEqualTo("  ");
    }

    @Test
    void signedNumericMoveTruncatesHighOrderDigitsAndExtraDecimalsWithoutRounding() {
        assertThat(Pic.s9(new BigDecimal("12345678901.999"), 9, 2)).isEqualByComparingTo("345678901.99");
        assertThat(Pic.s9(new BigDecimal("-1.005"), 9, 2)).isEqualByComparingTo("-1.00");
    }

    @Test
    void zonedDecimalOverpunchDecodesPositiveAndNegativeTrailingSigns() {
        assertThat(ZonedDecimal.decode("0000005047G", 2)).isEqualByComparingTo("504.77");
        assertThat(ZonedDecimal.decode("00000001940{", 2)).isEqualByComparingTo("194.00");
        assertThat(ZonedDecimal.decode("0000000081}", 2)).isEqualByComparingTo("-8.10");
        assertThat(ZonedDecimal.decode("0000000081J", 2)).isEqualByComparingTo("-8.11");
        assertThat(ZonedDecimal.decode("00000000123", 2)).isEqualByComparingTo("1.23");
    }

    @Test
    void zonedDecimalEncodeRoundTrips() {
        for (String v : new String[]{"0", "504.77", "-8.11", "-999999999.99", "999999999.99"}) {
            String enc = ZonedDecimal.encode(new BigDecimal(v), 9, 2);
            assertThat(enc).hasSize(11);
            assertThat(ZonedDecimal.decode(enc, 2)).isEqualByComparingTo(v);
        }
    }

    @Test
    void nonNumericDataInANumericFieldIsAnS0c7DataException() {
        assertThatThrownBy(() -> ZonedDecimal.decode("00000A0000{", 2))
                .isInstanceOf(CobolDataException.class).hasMessageContaining("S0C7");
    }

    @Test
    void comp3PackedDecimalMapsToBigDecimal() {
        byte[] packed = PackedDecimal.encode(new BigDecimal("-1234.56"), 9, 2);
        assertThat(packed).hasSize(6);
        assertThat(packed[packed.length - 1] & 0x0F).isEqualTo(0x0D);
        assertThat(PackedDecimal.decode(packed, 2)).isEqualByComparingTo("-1234.56");
        assertThat(PackedDecimal.decode(new byte[]{0x12, 0x34, 0x5C}, 2)).isEqualByComparingTo("123.45");
    }

    @Test
    void fileStatusZeroZeroIsSuccessAndOneZeroIsEndOfFile() {
        assertThat(FileStatus.SUCCESS.code()).isEqualTo("00");
        assertThat(FileStatus.SUCCESS.isSuccess()).isTrue();
        assertThat(FileStatus.END_OF_FILE.code()).isEqualTo("10");
        assertThat(FileStatus.END_OF_FILE.isEndOfFile()).isTrue();
    }

    @Test
    void displayIoStatusRendersTheFourDigitIoStatusLikeZDisplayIoStatus() {
        assertThat(FileStatus.displayIoStatus("35")).isEqualTo("FILE STATUS IS: NNNN0035");
        assertThat(FileStatus.displayIoStatus("9" + (char) 34)).isEqualTo("FILE STATUS IS: NNNN9034");
    }

    @Test
    void cobolFileExceptionCarriesDdNameAndFileStatus() {
        CobolFileException e = new CobolFileException("ACCTFILE", FileStatus.PERMANENT_IO_ERROR, "ERROR READING ACCOUNT FILE", null);
        assertThat(e.getDdName()).isEqualTo("ACCTFILE");
        assertThat(e.getFileStatus()).isEqualTo("30");
        assertThat(e.getMessage()).contains("ERROR READING ACCOUNT FILE", "NNNN0030", "ABCODE 999");
    }
}
