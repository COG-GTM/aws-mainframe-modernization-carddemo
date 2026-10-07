package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.http.HttpHeaders;

/** {@code docs/modernization/rules/COTRN01C.md} against {@code GET /api/v1/transactions/{id}}. */
class TransactionViewRulesTest extends TransactionWebTest {

    @Test
    void R1_noSessionIsRejected() throws Exception {
        mvc.perform(get(TRANSACTIONS + "/" + id(1))).andExpect(status().isUnauthorized());
    }

    @Test
    void R2_noIdSelectedPromptsForOne() throws Exception {
        view(" ").andExpect(status().isBadRequest()).andExpect(jsonPath("$.field").value("tranId"));
    }

    @Test
    void R3_aRowSelectedOnTheListIsShownImmediately() throws Exception {
        JsonNode selection = body(select(List.of(id(3)), List.of("S")));
        String next = selection.get("next").asText();
        mvc.perform(get(next).header(HttpHeaders.AUTHORIZATION, user())).andExpect(status().isOk())
                .andExpect(jsonPath("$.transaction.tranId").value(id(3)))
                .andExpect(jsonPath("$.exit.toProgram").value("COTRN00C"));
    }

    @Test
    void R4_enterReadsTheId() throws Exception {
        view(id(5)).andExpect(status().isOk()).andExpect(jsonPath("$.transaction.tranId").value(id(5)));
    }

    @Test
    void R5_pf3ReturnsToTheCallerOrTheMainMenu() throws Exception {
        view(id(5)).andExpect(jsonPath("$.exit.toTranId").value("CM00"))
                .andExpect(jsonPath("$.exit.toProgram").value("COMEN01C"));
        view(id(5), "fromProgram", "COTRN00C").andExpect(jsonPath("$.exit.toTranId").value("CT00"))
                .andExpect(jsonPath("$.exit.toProgram").value("COTRN00C"));
    }

    @Test
    void R6_pf4ClearIsAFreshScreen() throws Exception {
        view(id(5)).andExpect(jsonPath("$.message").value(""));
        view(id(6)).andExpect(jsonPath("$.transaction.tranId").value(id(6)))
                .andExpect(jsonPath("$.transaction.description").value("Purchase 6"));
    }

    @Test
    void R7_pf5ReturnsToTheList() throws Exception {
        view(id(5)).andExpect(jsonPath("$.list.fromTranId").value("CT01"))
                .andExpect(jsonPath("$.list.fromProgram").value("COTRN01C"))
                .andExpect(jsonPath("$.list.toTranId").value("CT00"))
                .andExpect(jsonPath("$.list.toProgram").value("COTRN00C"));
    }

    @Test
    void R8_otherKeysAreInvalid() throws Exception {
        mvc.perform(put(TRANSACTIONS + "/" + id(1)).header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isMethodNotAllowed()).andExpect(jsonPath("$.code").value("INVALID_KEY"));
    }

    @Test
    void R9_blankIdIsRejected() throws Exception {
        view("  ").andExpect(status().isBadRequest()).andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.message").value("Tran ID can NOT be empty..."));
    }

    @Test
    void R10_theIdIsReadAsTypedWithoutNumericCheckOrPadding() throws Exception {
        view("ABC").andExpect(status().isNotFound());
        view("1").andExpect(status().isNotFound());
        view(id(1)).andExpect(status().isOk());
    }

    @Test
    void R11_allFieldsAreShownWithTheAmountEdited() throws Exception {
        JsonNode t = body(view(id(1)).andExpect(status().isOk())).get("transaction");
        assertThat(t.get("tranId").asText()).isEqualTo(id(1));
        assertThat(t.get("cardNumber").asText()).isEqualTo(card(1));
        assertThat(t.get("typeCode").asText()).isEqualTo("01");
        assertThat(t.get("categoryCode").asText()).isEqualTo("0001");
        assertThat(t.get("source").asText()).isEqualTo("POS TERM");
        assertThat(t.get("amount").asText()).isEqualTo("+00000045.10");
        assertThat(t.get("description").asText()).isEqualTo(LONG_DESCRIPTION);
        assertThat(t.get("origTimestamp").asText()).isEqualTo("2022-06-01 10:11:12.000000");
        assertThat(t.get("procTimestamp").asText()).isEqualTo("2022-07-06 13:45:10.000000");
        assertThat(t.get("merchantId").asText()).isEqualTo("000000101");
        assertThat(t.get("merchantName").asText()).isEqualTo("Merchant 1");
        assertThat(t.get("merchantCity").asText()).isEqualTo("Seattle");
        assertThat(t.get("merchantZip").asText()).isEqualTo("98101");
        view(id(2)).andExpect(jsonPath("$.transaction.amount").value("-00000120.00"));
    }

    @Test
    void R12_nothingIsRewritten() throws Exception {
        view(id(1)).andExpect(status().isOk());
        verify(transactions, never()).saveAndFlush(any());
    }

    @Test
    void R13_unknownIdIsNotFound() throws Exception {
        view(id(99)).andExpect(status().isNotFound()).andExpect(jsonPath("$.code").value("NOTFND"))
                .andExpect(jsonPath("$.message").value("Transaction ID NOT found..."));
    }

    @Test
    void R14_otherReadErrorsAbend() throws Exception {
        given(transactions.findById(anyString())).willThrow(new DataAccessResourceFailureException("down"));
        view(id(1)).andExpect(status().isInternalServerError()).andExpect(jsonPath("$.code").value("ABEND"))
                .andExpect(jsonPath("$.message", endsWith("Unable to lookup Transaction...")));
    }

    @Test
    void R15_returnCarriesTheFromFields() throws Exception {
        view(id(1)).andExpect(jsonPath("$.exit.fromTranId").value("CT01"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COTRN01C"));
    }

    @Test
    void R16_standardHeader() throws Exception {
        view(id(1)).andExpect(jsonPath("$.header.tranId").value("CT01"))
                .andExpect(jsonPath("$.header.programName").value("COTRN01C"))
                .andExpect(jsonPath("$.header.currentTime").value("13:45:10"));
    }
}
