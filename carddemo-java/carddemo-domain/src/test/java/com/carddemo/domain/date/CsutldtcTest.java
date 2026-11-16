package com.carddemo.domain.date;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * {@code CALL 'CSUTLDTC'} as its callers make it. Expected severity, message number and result text are
 * the {@code FC-*} tokens and {@code WS-RESULT} literals of {@code app/cbl/CSUTLDTC.cbl}; which date gives
 * which token is CEEDAYS behaviour (see {@link Ceedays}).
 */
class CsutldtcTest {

    @Test
    void validDateGivesTheFullCobolMessage() {
        Csutldtc.Result r = Csutldtc.call("2024-02-29", "YYYY-MM-DD");

        assertThat(r.message())
                .hasSize(Csutldtc.RESULT_LENGTH)
                .isEqualTo("0000Mesg Code: 0000 Date is valid   TstDate: \u0000\n2024-02- Mask used:YYYY-MM-DD    ");
        assertThat(r.valid()).isTrue();
        assertThat(r.returnCode()).isZero();
    }

    @ParameterizedTest(name = "[{0}] {1} -> {2} {3} {4}")
    @CsvSource(delimiter = ';', ignoreLeadingAndTrailingWhitespace = false, value = {
        // CORPT00C / COTRN02C: WS-DATE-FORMAT 'YYYY-MM-DD'
        "2024-02-29;YYYY-MM-DD;0000;0000;Date is valid  ",
        "1582-10-15;YYYY-MM-DD;0000;0000;Date is valid  ",
        "9999-12-31;YYYY-MM-DD;0000;0000;Date is valid  ",
        "2023-02-29;YYYY-MM-DD;0003;2508;Datevalue error",
        "2024-04-31;YYYY-MM-DD;0003;2508;Datevalue error",
        "2024-01-00;YYYY-MM-DD;0003;2508;Datevalue error",
        "1900-02-29;YYYY-MM-DD;0003;2508;Datevalue error",
        "2024-13-01;YYYY-MM-DD;0003;2517;Invalid month  ",
        "2024-00-10;YYYY-MM-DD;0003;2517;Invalid month  ",
        "2024-0A-01;YYYY-MM-DD;0003;2520;Nonnumeric data",
        "2024/01/05;YYYY-MM-DD;0003;2520;Nonnumeric data",
        "2024-01-  ;YYYY-MM-DD;0003;2507;Insufficient   ",
        "          ;YYYY-MM-DD;0003;2507;Insufficient   ",
        "1582-10-14;YYYY-MM-DD;0003;2513;Unsupp. Range  ",
        "0000-01-01;YYYY-MM-DD;0003;2513;Unsupp. Range  ",
        "2024-01-05;YYYY-MM-XX;0003;2518;Bad Pic String ",
        "2024-01-05;          ;0003;2518;Bad Pic String ",
        // CSUTLDPY EDIT-DATE-LE: WS-DATE-FORMAT 'YYYYMMDD'
        "20240229  ;YYYYMMDD  ;0000;0000;Date is valid  ",
        "20230229  ;YYYYMMDD  ;0003;2508;Datevalue error",
        "20241301  ;YYYYMMDD  ;0003;2517;Invalid month  ",
    })
    void feedbackMatchesCsutldtcLiterals(String date, String format, String severity, String messageNumber,
            String result) {
        Csutldtc.Result r = Csutldtc.call(date, format);

        assertThat(r.severityCode()).isEqualTo(severity);
        assertThat(r.messageNumber()).isEqualTo(messageNumber);
        assertThat(r.resultText()).isEqualTo(result);
        assertThat(r.returnCode()).isEqualTo(Integer.parseInt(severity));
        assertThat(r.valid()).isEqualTo("0000".equals(severity));
        assertThat(r.message()).hasSize(Csutldtc.RESULT_LENGTH);
    }

    @Test
    void outOfRangeDateIsTheMessage2513ThatCorpt00cAndCotrn02cLetThrough() {
        Csutldtc.Result r = Csutldtc.call("1500-01-01", "YYYY-MM-DD");

        assertThat(r.valid()).isFalse();
        assertThat(r.messageNumber()).isEqualTo("2513");
    }

    @Test
    void inputsAreReadAsPicX10() {
        assertThat(Csutldtc.call("2024-02-29 trailing", "YYYY-MM-DD").message())
                .isEqualTo(Csutldtc.call("2024-02-29", "YYYY-MM-DD").message());
        assertThat(Csutldtc.call("20240229", "YYYYMMDD").message())
                .endsWith("TstDate: \u0000\n20240229 Mask used:YYYYMMDD      ");
    }
}
