package com.carddemo.zerotrust.masking;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.LoggingEvent;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DataMaskerTest {

    private final DataMasker masker = new DataMasker();

    @Test
    void maskPanKeepsLastFour() {
        assertThat(masker.maskPan("4000000000000001")).isEqualTo("************0001");
        assertThat(masker.maskPan(" 4000000000000001 ")).isEqualTo("************0001");
        assertThat(masker.maskPan("1234")).isEqualTo("1234");
        assertThat(masker.maskPan(null)).isNull();
        assertThat(masker.maskPan("   ")).isEqualTo("   ");
    }

    @Test
    void maskSsnKeepsLastFour() {
        assertThat(masker.maskSsn("123-45-6789")).isEqualTo("***-**-6789");
        assertThat(masker.maskSsn("123456789")).isEqualTo("***-**-6789");
    }

    @Test
    void scrubMasksLongDigitRuns() {
        String out = masker.scrub("card 4000000000000001 ok");
        assertThat(out).isEqualTo("card ************0001 ok");
    }

    @Test
    void scrubMasksSsnPattern() {
        assertThat(masker.scrub("ssn 123-45-6789 here")).isEqualTo("ssn ***-**-6789 here");
    }

    @Test
    void scrubOverMasks16DigitTransactionIds() {
        // Documented behavior: no Luhn check, any 13-19 digit run is masked.
        assertThat(masker.scrub("txn 1234567890123456 posted"))
                .isEqualTo("txn ************3456 posted");
    }

    @Test
    void scrubLeavesShortNumbersAlone() {
        assertThat(masker.scrub("amount 12345.67 code 100")).isEqualTo("amount 12345.67 code 100");
    }

    @Test
    void scrubHandlesNullAndEmpty() {
        assertThat(masker.scrub(null)).isNull();
        assertThat(masker.scrub("")).isEmpty();
    }

    @Test
    void logbackConverterMasksMessage() {
        LoggerContext ctx = new LoggerContext();
        LoggingEvent event = new LoggingEvent("test",
                ctx.getLogger("test"), Level.INFO,
                "posting card 4000000000000002 amount 42.00", null, null);
        MaskingMessageConverter converter = new MaskingMessageConverter();
        assertThat(converter.convert(event))
                .isEqualTo("posting card ************0002 amount 42.00");
    }
}
