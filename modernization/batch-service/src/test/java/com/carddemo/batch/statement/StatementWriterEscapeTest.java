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
}
