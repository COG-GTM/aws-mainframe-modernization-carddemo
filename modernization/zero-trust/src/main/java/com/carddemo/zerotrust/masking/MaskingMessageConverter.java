package com.carddemo.zerotrust.masking;

import ch.qos.logback.classic.pattern.MessageConverter;
import ch.qos.logback.classic.spi.ILoggingEvent;

/**
 * Logback conversion rule ({@code %maskedMsg}) that scrubs every rendered message.
 * Registered in logback-spring.xml so log lines from the wrapped posting code are
 * masked too, not just this module's own.
 */
public class MaskingMessageConverter extends MessageConverter {

    private static final DataMasker MASKER = new DataMasker();

    @Override
    public String convert(ILoggingEvent event) {
        return MASKER.scrub(event.getFormattedMessage());
    }
}
