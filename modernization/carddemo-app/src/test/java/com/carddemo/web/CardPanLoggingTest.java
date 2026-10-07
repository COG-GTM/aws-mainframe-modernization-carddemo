package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import ch.qos.logback.classic.Level;
import com.carddemo.support.LogCapture;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

/**
 * ADR-0020 / s6.4: card endpoints never put a full PAN in a log line, an error body or a list response; only the
 * COCRDSLC/COCRDUPC detail screens (CCRDSLA/CCRDUPA show CARDSID) return it. Logs captured at DEBUG.
 */
class CardPanLoggingTest extends CardWebTest {

    private List<String> pans() {
        return new ArrayList<>(store.keySet());
    }

    private void assertNoPan(String what, String text) {
        for (String pan : pans()) {
            assertThat(text).as(what + " must not contain PAN " + pan).doesNotContain(pan);
        }
    }

    @Test
    void noFullPanInLogsListsOrErrors() throws Exception {
        try (LogCapture logs = LogCapture.start(Level.DEBUG, "com.carddemo")) {
            JsonNode first = page(admin());
            assertNoPan("admin list", first.toString());
            JsonNode next = page(admin(), "after", first.get("nextPage").asText());
            assertNoPan("next page", next.toString());
            assertNoPan("user list", page(user(), "accountId", String.valueOf(account(1))).toString());
            List<String> refs = refsOf(first);
            String selection = body(select(admin(), refs.subList(0, 1), List.of("S"))).toString();
            assertNoPan("selection", selection);

            String detail = body(view(admin(), refs.get(0), String.valueOf(account(1))).andExpect(status().isOk())).toString();
            assertThat(detail).as("COCRDSLC shows the full card number").contains(pan(1));

            assertNoPan("not found", body(view(admin(), "4999999999999999", "1")
                    .andExpect(status().isNotFound())).toString());
            assertNoPan("other account (USER)", body(view(user(), pan(10), String.valueOf(account(1))).andExpect(status().isNotFound()))
                    .toString());
            assertNoPan("bad key", body(view(admin(), "41110000ABCD0001", "1")
                    .andExpect(status().isBadRequest())).toString());

            ObjectNode bad = form(1);
            bad.put("embossedName", "12345").put("confirm", false);
            String badBody = body(update(admin(), refs.get(0), bad).andExpect(status().isBadRequest())).toString();
            assertNoPan("edit error", badBody);
            ObjectNode stale = form(1);
            stale.put("embossedName", "NEW NAME").put("version", 99).put("confirm", true);
            assertNoPan("stale version", body(update(admin(), refs.get(0), stale)).toString());
            ObjectNode ok = form(1);
            ok.put("embossedName", "NEW NAME").put("confirm", true);
            String committed = body(update(admin(), refs.get(0), ok).andExpect(status().isOk())).toString();
            assertThat(committed).as("COCRDUPC shows the full card number").contains(pan(1));

            assertThat(logs.lines()).as("COCRDUPC REWRITE was logged (masked)")
                    .anyMatch(l -> l.contains("COCRDUPC REWRITE card"));
            for (String line : logs.lines()) {
                assertNoPan("log line", line);
            }
        }
    }
}
