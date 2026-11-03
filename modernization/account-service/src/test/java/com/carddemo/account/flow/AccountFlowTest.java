package com.carddemo.account.flow;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.testsupport.flow.CobolFlow;
import com.carddemo.testsupport.flow.ScreenKey;
import com.carddemo.testsupport.flow.ServiceStandIn;
import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.util.Map;
import org.junit.jupiter.api.Test;

/**
 * CAVW account view (COACTVWC) and CAUP account update (COACTUPC: ENTER fetches, PF5 edits and
 * rewrites ACCTDAT and CUSTDAT) on sample account 1 (card 9680294154603697, customer 1 Immanuel
 * Madeline Kessler). The edit-by-edit message cases are in {@link AccountUpdateMessagesFlowTest}.
 */
class AccountFlowTest extends AccountFlowTestSupport {

    @Test
    @CobolFlow(program = "COACTVWC", paragraph = "9000-READ-ACCT", key = ScreenKey.ENTER)
    void COACTVWC_9000_READ_ACCT_knownAccountShowsAccountAndCustomer() throws Exception {
        mockMvc.perform(asUser(get("/api/v1/accounts/00000000001/view")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.accountId").value("00000000001"))
                .andExpect(jsonPath("$.activeStatus").value("Y"))
                .andExpect(jsonPath("$.currentBalance").value(194.00))
                .andExpect(jsonPath("$.creditLimit").value(2020.00))
                .andExpect(jsonPath("$.cashCreditLimit").value(1020.00))
                .andExpect(jsonPath("$.openDate").value("2014-11-20"))
                .andExpect(jsonPath("$.expirationDate").value("2025-05-20"))
                .andExpect(jsonPath("$.customerId").value("000000001"))
                .andExpect(jsonPath("$.firstName").value("Immanuel"))
                .andExpect(jsonPath("$.middleName").value("Madeline"))
                .andExpect(jsonPath("$.lastName").value("Kessler"))
                .andExpect(jsonPath("$.city").value("Altenwerthshire"))
                .andExpect(jsonPath("$.stateCode").value("NC"))
                .andExpect(jsonPath("$.ssn").value("***-**-3888"))
                .andExpect(jsonPath("$.dateOfBirth").value("1961-06-08"))
                .andExpect(jsonPath("$.ficoScore").value(274));
        // 9200-GETCARDXREF-BYACCT then 9400-GETCUSTDATA-BYCUST, with the caller's token forwarded.
        assertThat(SERVICES.calls()).extracting(ServiceStandIn.Call::request)
                .contains("GET /api/v1/card-xrefs/by-account/1", "GET /api/v1/customers/1");
        assertThat(SERVICES.calls()).allSatisfy(call -> assertThat(call.authorization()).startsWith("Bearer "));
    }

    @Test
    @CobolFlow(program = "COACTVWC", paragraph = "2210-EDIT-ACCOUNT", key = ScreenKey.ENTER)
    void COACTVWC_2210_EDIT_ACCOUNT_zeroOrNonNumericAccountIsRejected() throws Exception {
        for (String id : new String[] {"00000000000", "1234X"}) {
            mockMvc.perform(asUser(get("/api/v1/accounts/" + id + "/view")))
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.fieldErrors[0].message").value("Account Filter must  be a non-zero 11 digit number"));
        }
    }

    @Test
    @CobolFlow(program = "COACTVWC", paragraph = "9200-GETCARDXREF-BYACCT", key = ScreenKey.ENTER)
    void COACTVWC_9200_GETCARDXREF_BYACCT_unknownAccountIsNotFound() throws Exception {
        mockMvc.perform(asUser(get("/api/v1/accounts/99999999999/view")))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Account:99999999999 not found in Cross ref file."));
    }

    @Test
    @CobolFlow(program = "COACTUPC", paragraph = "9000-READ-ACCT", key = ScreenKey.ENTER)
    void COACTUPC_9000_READ_ACCT_enterFetchesTheRecordsToUpdate() throws Exception {
        JsonNode view = view(2);
        assertThat(view.get("firstName").asText()).isEqualTo("Enrico");
        assertThat(view.get("lastName").asText()).isEqualTo("Rosenbaum");
        assertThat(view.get("accountVersion").asLong()).isZero();
        assertThat(view.get("customerVersion").asLong()).isZero();
    }

    @Test
    @CobolFlow(program = "COACTUPC", paragraph = "1210-EDIT-ACCOUNT", key = ScreenKey.PF5)
    void COACTUPC_1210_EDIT_ACCOUNT_zeroAccountNumberIsRejected() throws Exception {
        update("00000000000", validScreen(view(1)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message")
                        .value("Account Number if supplied must be a 11 digit Non-Zero Number"));
    }

    @Test
    @CobolFlow(program = "COACTUPC", paragraph = "1205-COMPARE-OLD-NEW", key = ScreenKey.PF5)
    void COACTUPC_1205_COMPARE_OLD_NEW_unchangedScreenIsNotWritten() throws Exception {
        // As stored the record would fail three edits; "no change" is decided before the edits run.
        update("00000000001", screen(view(1)))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("No change detected with respect to values fetched."));
    }

    @Test
    @CobolFlow(program = "COACTUPC", paragraph = "9600-WRITE-PROCESSING", key = ScreenKey.PF5)
    void COACTUPC_9600_WRITE_PROCESSING_validChangesRewriteAccountAndCustomer() throws Exception {
        Map<String, Object> body = validScreen(view(4));
        body.put("creditLimit", "5,000.00");
        body.put("stateCode", "NC");
        body.put("zip", "28202");
        body.put("phoneNumber1", "201-555-0100");
        update("00000000004", body)
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.creditLimit").value(5000.00))
                .andExpect(jsonPath("$.zip").value("28202"))
                .andExpect(jsonPath("$.accountVersion").value(1))
                .andExpect(jsonPath("$.customerVersion").value(1));
        assertThat(jdbc.queryForObject("SELECT credit_limit FROM account.accounts WHERE acct_id = 4", BigDecimal.class))
                .isEqualByComparingTo("5000.00");
        Map<String, Object> customer = jdbc.queryForMap(
                "SELECT c.* FROM customer.customers c JOIN card.card_xref x ON x.cust_id = c.cust_id WHERE x.acct_id = 4");
        assertThat(customer).containsEntry("addr_state_cd", "NC").containsEntry("addr_zip", "28202")
                .containsEntry("phone_num_1", "(201)555-0100").containsEntry("fico_credit_score", 700);
    }

    @Test
    @CobolFlow(program = "COACTUPC", paragraph = "9700-CHECK-CHANGE-IN-REC", key = ScreenKey.PF5)
    void COACTUPC_9700_CHECK_CHANGE_IN_REC_recordChangedSinceFetchIsRefused() throws Exception {
        JsonNode fetched = view(5);
        Map<String, Object> first = validScreen(fetched);
        first.put("stateCode", "NC");
        first.put("activeStatus", "N");
        update("00000000005", first).andExpect(status().isOk());
        Map<String, Object> stale = validScreen(fetched);
        stale.put("stateCode", "NC");
        stale.put("cashCreditLimit", "1.00");
        update("00000000005", stale)
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.message").value("Record changed by some one else. Please review"));
    }
}
