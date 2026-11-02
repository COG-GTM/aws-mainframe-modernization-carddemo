package com.carddemo.batch.statement;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class StatementWriterEscapeTest {

    @Test
    void markupInCustomerOrTransactionTextIsEscaped() {
        assertThat(StatementWriter.escape("<script>alert('x')</script> & \"q\""))
                .isEqualTo("&lt;script&gt;alert('x')&lt;/script&gt; &amp; \"q\"");
        assertThat(StatementWriter.escape("Purchase at Zulauf-O'Keefe")).isEqualTo("Purchase at Zulauf-O'Keefe");
    }

    @Test
    void escapedLineStillFitsTheRecordWithItsClosingTag() {
        String line = StatementWriter.htmlLine("<p>", "&".repeat(50), "  </p>");
        assertThat(line).hasSizeLessThanOrEqualTo(StatementWriter.HTML_LRECL).endsWith("&amp;  </p>");
        assertThat(StatementWriter.htmlLine("<p>", "1588 Nienow Cape", "  </p>")).isEqualTo("<p>1588 Nienow Cape  </p>");
    }
}
