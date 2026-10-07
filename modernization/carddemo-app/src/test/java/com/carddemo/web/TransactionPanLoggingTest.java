package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import ch.qos.logback.classic.Level;
import com.carddemo.support.LogCapture;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.List;
import org.junit.jupiter.api.Test;

/**
 * ADR-0020 / s6.4: transaction and bill-payment endpoints never put a full PAN in a log line, an error body or the
 * COTRN00C list; COTRN01C (CTRN1A shows CARDNUM) and the COTRN02C form echo it as the COBOL screens do.
 */
class TransactionPanLoggingTest extends TransactionWebTest {

    private static final List<String> PANS = List.of(card(1), card(2), card(3), "4999999999999999");

    private static void assertNoPan(String what, String text) {
        for (String pan : PANS) {
            assertThat(text).as(what + " must not contain PAN " + pan).doesNotContain(pan);
        }
    }

    @Test
    void noFullPanInLogsListsOrErrors() throws Exception {
        try (LogCapture logs = LogCapture.start(Level.DEBUG, "com.carddemo")) {
            JsonNode first = page();
            assertNoPan("COTRN00C list", first.toString());
            assertNoPan("next page", page("after", first.get("nextPage").asText()).toString());
            assertNoPan("selection", body(select(List.of(id(1)), List.of("S"))).toString());
            assertThat(body(view(id(1)).andExpect(status().isOk())).toString())
                    .as("COTRN01C shows the card number").contains(card(1));
            assertNoPan("view not found", body(view(id(99)).andExpect(status().isNotFound())).toString());

            ObjectNode unknownCard = form();
            unknownCard.put("accountId", "").put("cardNumber", "4999999999999999");
            assertNoPan("card not found", body(add(unknownCard)).toString());
            ObjectNode badAmount = form();
            badAmount.put("amount", "12.34");
            assertNoPan("edit error", body(add(badAmount).andExpect(status().isBadRequest())).toString());

            ObjectNode ok = form();
            ok.put("confirm", "Y");
            add(ok).andExpect(status().isCreated());
            ObjectNode byCard = form();
            byCard.put("accountId", "").put("cardNumber", card(2)).put("confirm", "Y");
            add(byCard).andExpect(status().isCreated());

            JsonNode shown = body(pay("1", "", null));
            pay("1", "Y", shown.at("/account/version").asLong()).andExpect(status().isOk());
            assertNoPan("bill payment not found", body(pay("9", "Y", 0L)).toString());

            assertThat(logs.lines()).anyMatch(l -> l.contains("COTRN02C WRITE transaction"))
                    .anyMatch(l -> l.contains("COBIL00C bill payment"));
            for (String line : logs.lines()) {
                assertNoPan("log line", line);
            }
        }
    }
}
