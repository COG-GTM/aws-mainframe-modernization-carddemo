package com.carddemo.batch.creastmt;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class StatementWriterTest {

    @Test
    void escapesHtmlMarkupInDataFields() {
        assertThat(StatementWriter.esc("<script>alert('x')</script> & \"q\""))
                .isEqualTo("&lt;script&gt;alert(&#39;x&#39;)&lt;/script&gt; &amp; &quot;q&quot;");
        assertThat(StatementWriter.esc("JOHN DOE")).isEqualTo("JOHN DOE");
    }
}
