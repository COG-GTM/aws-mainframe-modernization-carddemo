package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.account.Account;
import com.carddemo.transaction.Transaction;
import java.math.BigDecimal;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;

/** {@code docs/modernization/rules/COBIL00C.md} against {@code POST /api/v1/accounts/{id}/bill-payment}. */
class BillPaymentRulesTest extends TransactionWebTest {

    @Test
    void R1_noSessionIsRejected() throws Exception {
        mvc.perform(post("/api/v1/accounts/1/bill-payment").contentType(MediaType.APPLICATION_JSON).content("{}"))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void R2_aPrefilledAccountIsShownAtOnce() throws Exception {
        pay("00000000001", "", null).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("SHOW"))
                .andExpect(jsonPath("$.accountId").value("00000000001"))
                .andExpect(jsonPath("$.currentBalance").value("194.00"));
    }

    @Test
    void R3_enterReadsTheAccount() throws Exception {
        pay("1", null, null).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("SHOW"));
    }

    @Test
    void R4_pf3ReturnsToTheCallerOrTheMainMenu() throws Exception {
        pay("1", "", null).andExpect(jsonPath("$.exit.toProgram").value("COMEN01C"));
        pay("1", "", null, "fromProgram", "COTRN00C").andExpect(jsonPath("$.exit.toProgram").value("COTRN00C"));
        pay("1", "", null, "fromProgram", "COACTVWC").andExpect(jsonPath("$.exit.toProgram").value("COACTVWC"))
                .andExpect(jsonPath("$.exit.toTranId").value("CAVW"));
    }

    @Test
    void R5_confirmNClearsTheScreen() throws Exception {
        pay("1", "N", null).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("CLEARED"))
                .andExpect(jsonPath("$.accountId").doesNotExist())
                .andExpect(jsonPath("$.currentBalance").doesNotExist())
                .andExpect(jsonPath("$.message").value(""));
    }

    @Test
    void R6_otherKeysAreInvalid() throws Exception {
        mvc.perform(get("/api/v1/accounts/1/bill-payment").header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isMethodNotAllowed()).andExpect(jsonPath("$.code").value("INVALID_KEY"));
    }

    @Test
    void R7_blankAccountIsRejected() throws Exception {
        pay(" ", "", null).andExpect(status().isBadRequest()).andExpect(jsonPath("$.field").value("accountId"))
                .andExpect(jsonPath("$.message").value("Acct ID can NOT be empty..."));
    }

    @Test
    void R8_accountAsTypedAndTheConfirmValues() throws Exception {
        pay("ABC", "", null).andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Account ID NOT found..."));
        pay("1", "X", null).andExpect(status().isBadRequest()).andExpect(jsonPath("$.field").value("confirm"))
                .andExpect(jsonPath("$.message").value("Invalid value. Valid values are (Y/N)..."));
        pay("1", "n", null).andExpect(jsonPath("$.state").value("CLEARED"));
        pay("1", "y", 0L).andExpect(jsonPath("$.state").value("PAID"));
    }

    @Test
    void R9_theCurrentBalanceIsShownWithTheVersion() throws Exception {
        pay("1", "", null).andExpect(jsonPath("$.currentBalance").value("194.00"))
                .andExpect(jsonPath("$.version").value(0));
    }

    @Test
    void R10_nothingToPayWhenTheBalanceIsZeroOrNegative() throws Exception {
        for (String account : new String[] {"2", "3"}) {
            for (String confirm : new String[] {"", "Y"}) {
                pay(account, confirm, 0L).andExpect(status().isBadRequest())
                        .andExpect(jsonPath("$.field").value("accountId"))
                        .andExpect(jsonPath("$.message").value("You have nothing to pay..."));
            }
        }
        verify(transactions, never()).saveAndFlush(any());
        verify(accounts, never()).saveAndFlush(any());
    }

    @Test
    void R11_blankConfirmAsksForConfirmation() throws Exception {
        pay("1", "", null).andExpect(jsonPath("$.message").value("Confirm to make a bill payment..."));
        verify(transactions, never()).saveAndFlush(any());
    }

    @Test
    void R12_confirmYWritesTheBillPaymentAndZeroesTheBalance() throws Exception {
        pay("1", "Y", 0L).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("PAID"))
                .andExpect(jsonPath("$.currentBalance").value("0.00"))
                .andExpect(jsonPath("$.version").value(1));
        Transaction t = store.get(id(24));
        assertThat(t.getTranTypeCd()).isEqualTo("02");
        assertThat(t.getTranCatCd()).isEqualTo(2);
        assertThat(t.getSource()).isEqualTo("POS TERM");
        assertThat(t.getDescription()).isEqualTo("BILL PAYMENT - ONLINE");
        assertThat(t.getAmount()).isEqualByComparingTo(new BigDecimal("194.00"));
        assertThat(t.getCardNum()).isEqualTo(card(1));
        assertThat(t.getMerchantId()).isEqualTo(999_999_999);
        assertThat(t.getMerchantName()).isEqualTo("BILL PAYMENT");
        assertThat(t.getMerchantCity()).isEqualTo("N/A");
        assertThat(t.getMerchantZip()).isEqualTo("N/A");
        Account account = accountStore.get(1L);
        assertThat(account.getCurrBal()).isEqualByComparingTo("0.00");
        assertThat(account.toRecord().currCycDebit()).isEqualByComparingTo("0.00");
    }

    @Test
    void R13_timestampsAreTheCurrentTimeWithZeroMicroseconds() throws Exception {
        pay("1", "Y", 0L).andExpect(jsonPath("$.transaction.origTimestamp").value("2022-07-06 13:45:10.000000"))
                .andExpect(jsonPath("$.transaction.procTimestamp").value("2022-07-06 13:45:10.000000"));
    }

    @Test
    void R16_accountNotFoundAndOtherReadErrors() throws Exception {
        pay("99", "", null).andExpect(status().isNotFound()).andExpect(jsonPath("$.code").value("NOTFND"))
                .andExpect(jsonPath("$.message").value("Account ID NOT found..."));
        pay("99", "Y", 0L).andExpect(status().isNotFound());
        given(accounts.findById(anyLong())).willThrow(new DataAccessResourceFailureException("down"));
        pay("1", "", null).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to lookup Account...")));
    }

    @Test
    void R17_updateFailuresAbendAndAStaleVersionIs409() throws Exception {
        pay("1", "Y", 7L).andExpect(status().isConflict()).andExpect(jsonPath("$.code").value("CHANGED"));
        pay("1", "Y", null).andExpect(status().isBadRequest()).andExpect(jsonPath("$.field").value("version"));
        verify(transactions, never()).saveAndFlush(any());
        given(accounts.saveAndFlush(any(Account.class))).willThrow(new DataAccessResourceFailureException("down"));
        pay("1", "Y", 0L).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to Update Account...")));
    }

    @Test
    void R18_anAccountWithoutACardIsNotFound() throws Exception {
        pay("4", "", null).andExpect(status().isOk()).andExpect(jsonPath("$.currentBalance").value("25.00"));
        pay("4", "Y", 0L).andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Account ID NOT found..."));
        verify(transactions, never()).saveAndFlush(any());
    }

    @Test
    void R19_nextIdIsTheLastPlusOneOrOneForAnEmptyFile() throws Exception {
        store.clear();
        pay("1", "Y", 0L).andExpect(jsonPath("$.transaction.tranId").value(id(1)));
    }

    @Test
    void R20_successMessageDuplicateIdAndOtherWriteErrors() throws Exception {
        pay("1", "Y", 0L).andExpect(jsonPath("$.message")
                .value("Payment successful.  Your Transaction ID is " + id(24) + "."));
        ReflectionTestUtils.setField(accountStore.get(1L), "currBal", new BigDecimal("10.00"));
        given(transactions.findFirstByOrderByTranIdDesc()).willReturn(Optional.of(store.get(id(23))));
        pay("1", "Y", 1L).andExpect(status().isConflict()).andExpect(jsonPath("$.code").value("DUPREC"))
                .andExpect(jsonPath("$.message").value("Tran ID already exist..."));
        given(transactions.saveAndFlush(any(Transaction.class)))
                .willThrow(new DataIntegrityViolationException("check constraint"));
        pay("1", "Y", 1L).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to Add Bill pay Transaction...")));
    }

    @Test
    void R21_returnCarriesTheFromFields() throws Exception {
        pay("1", "", null).andExpect(jsonPath("$.exit.fromTranId").value("CB00"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COBIL00C"));
    }

    @Test
    void R22_standardHeader() throws Exception {
        pay("1", "", null).andExpect(jsonPath("$.header.tranId").value("CB00"))
                .andExpect(jsonPath("$.header.programName").value("COBIL00C"));
    }
}
