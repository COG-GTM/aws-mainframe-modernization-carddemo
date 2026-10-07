package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.TransactionRepository;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;

/** {@code docs/modernization/rules/COTRN02C.md} against {@code POST /api/v1/transactions}. */
class TransactionAddRulesTest extends TransactionWebTest {

    private static void rejected(ResultActions result, String field, String message) throws Exception {
        result.andExpect(status().isBadRequest()).andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value(field)).andExpect(jsonPath("$.message").value(message));
    }

    @Test
    void R1_noSessionIsRejected() throws Exception {
        mvc.perform(post(TRANSACTIONS).contentType(MediaType.APPLICATION_JSON).content("{}"))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void R2_aPrefilledCardNumberIsProcessedAtOnce() throws Exception {
        add(form().put("accountId", "").put("cardNumber", card(2))).andExpect(status().isOk())
                .andExpect(jsonPath("$.form.accountId").value("00000000002"))
                .andExpect(jsonPath("$.form.cardNumber").value(card(2)));
    }

    @Test
    void R3_enterValidates() throws Exception {
        add(form()).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("VALIDATED"));
    }

    @Test
    void R4_pf3ReturnsToTheCallerOrTheMainMenu() throws Exception {
        add(form()).andExpect(jsonPath("$.exit.toProgram").value("COMEN01C"))
                .andExpect(jsonPath("$.exit.toTranId").value("CM00"));
        add(form(), "fromProgram", "COTRN00C").andExpect(jsonPath("$.exit.toProgram").value("COTRN00C"));
        add(form(), "fromProgram", "COACTVWC").andExpect(jsonPath("$.exit.toProgram").value("COACTVWC"))
                .andExpect(jsonPath("$.exit.toTranId").value("CAVW"));
    }

    @Test
    void R5_aClearedScreenAsksForAKey() throws Exception {
        ObjectNode cleared = json.createObjectNode();
        form().fieldNames().forEachRemaining(f -> cleared.put(f, ""));
        rejected(add(cleared), "accountId", "Account or Card Number must be entered...");
    }

    @Test
    void R6_pf5CopiesTheLastTransaction() throws Exception {
        ObjectNode keysOnly = json.createObjectNode().put("accountId", "1").put("copyLast", true);
        add(keysOnly).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("VALIDATED"))
                .andExpect(jsonPath("$.form.description").value("Purchase 23"))
                .andExpect(jsonPath("$.form.typeCode").value("01"))
                .andExpect(jsonPath("$.form.categoryCode").value("0001"))
                .andExpect(jsonPath("$.form.amount").value("+00000045.10"))
                .andExpect(jsonPath("$.form.origDate").value("2022-06-23"))
                .andExpect(jsonPath("$.form.procDate").value("2022-07-06"))
                .andExpect(jsonPath("$.form.merchantId").value("000000123"));
    }

    @Test
    void R7_otherKeysAreInvalid() throws Exception {
        mvc.perform(put(TRANSACTIONS).header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isMethodNotAllowed()).andExpect(jsonPath("$.code").value("INVALID_KEY"));
    }

    @Test
    void R8_nonNumericAccountIsRejected() throws Exception {
        rejected(addWith("accountId", "12A"), "accountId", "Account ID must be Numeric...");
    }

    @Test
    void R9_accountResolvesTheCardAndWinsOverAnEnteredCard() throws Exception {
        add(form().put("cardNumber", card(2))).andExpect(status().isOk())
                .andExpect(jsonPath("$.form.accountId").value("00000000001"))
                .andExpect(jsonPath("$.form.cardNumber").value(card(1)));
    }

    @Test
    void R10_nonNumericCardIsRejected() throws Exception {
        rejected(add(form().put("accountId", "").put("cardNumber", "4111-0000")), "cardNumber",
                "Card Number must be Numeric...");
    }

    @Test
    void R11_cardResolvesTheAccount() throws Exception {
        add(form().put("accountId", "").put("cardNumber", card(3))).andExpect(status().isOk())
                .andExpect(jsonPath("$.form.accountId").value("00000000003"));
    }

    @Test
    void R12_bothKeysBlankAreRejected() throws Exception {
        rejected(add(form().put("accountId", " ").put("cardNumber", "")), "accountId",
                "Account or Card Number must be entered...");
    }

    @Test
    void R13_accountNotInCxacaixAndOtherErrors() throws Exception {
        addWith("accountId", "77").andExpect(status().isNotFound()).andExpect(jsonPath("$.code").value("NOTFND"))
                .andExpect(jsonPath("$.message").value("Account ID NOT found..."));
        given(xrefs.findFirstByAcctIdOrderByCardNumAsc(anyLong()))
                .willThrow(new DataAccessResourceFailureException("down"));
        add(form()).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to lookup Acct in XREF AIX file...")));
    }

    @Test
    void R14_cardNotInCardxrefAndOtherErrors() throws Exception {
        add(form().put("accountId", "").put("cardNumber", "4111000000009999")).andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Card Number NOT found..."));
        given(xrefs.findById(anyString())).willThrow(new DataAccessResourceFailureException("down"));
        add(form().put("accountId", "").put("cardNumber", card(1))).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to lookup Card # in XREF file...")));
    }

    @Test
    void R15_typeCodeRequiredFirst() throws Exception {
        rejected(add(form().put("typeCode", "").put("categoryCode", "")), "typeCode", "Type CD can NOT be empty...");
    }

    @Test
    void R16_categoryCodeRequired() throws Exception {
        rejected(add(form().put("categoryCode", "").put("source", "")), "categoryCode",
                "Category CD can NOT be empty...");
    }

    @Test
    void R17_sourceRequired() throws Exception {
        rejected(add(form().put("source", "").put("description", "")), "source", "Source can NOT be empty...");
    }

    @Test
    void R18_descriptionRequired() throws Exception {
        rejected(add(form().put("description", "").put("amount", "")), "description",
                "Description can NOT be empty...");
    }

    @Test
    void R19_amountRequired() throws Exception {
        rejected(add(form().put("amount", "").put("origDate", "")), "amount", "Amount can NOT be empty...");
    }

    @Test
    void R20_origDateRequired() throws Exception {
        rejected(add(form().put("origDate", "").put("procDate", "")), "origDate", "Orig Date can NOT be empty...");
    }

    @Test
    void R21_procDateRequired() throws Exception {
        rejected(add(form().put("procDate", "").put("merchantId", "")), "procDate", "Proc Date can NOT be empty...");
    }

    @Test
    void R22_merchantFieldsRequiredInScreenOrder() throws Exception {
        ObjectNode form = form().put("merchantId", "").put("merchantName", "").put("merchantCity", "")
                .put("merchantZip", "");
        rejected(add(form), "merchantId", "Merchant ID can NOT be empty...");
        rejected(add(form.put("merchantId", "1")), "merchantName", "Merchant Name can NOT be empty...");
        rejected(add(form.put("merchantName", "Shop")), "merchantCity", "Merchant City can NOT be empty...");
        rejected(add(form.put("merchantCity", "Town")), "merchantZip", "Merchant Zip can NOT be empty...");
    }

    @Test
    void R23_typeAndCategoryMustBeNumeric() throws Exception {
        rejected(add(form().put("typeCode", "A1").put("categoryCode", "X")), "typeCode",
                "Type CD must be Numeric...");
        rejected(addWith("categoryCode", "00X1"), "categoryCode", "Category CD must be Numeric...");
    }

    @Test
    void R24_amountMustBeSignedEightDotTwo() throws Exception {
        for (String bad : List.of("12.34", "00000012.34", "+0000001234", "+0000001.234", "+00000012,34",
                "*00000012.34", "+00000012.3")) {
            rejected(addWith("amount", bad), "amount", "Amount should be in format -99999999.99");
        }
        addWith("amount", "+00000012.34").andExpect(status().isOk());
    }

    @Test
    void R25_datesMustBeYyyyMmDdShaped() throws Exception {
        rejected(addWith("origDate", "2022/07/06"), "origDate", "Orig Date should be in format YYYY-MM-DD");
        rejected(addWith("procDate", "20220706"), "procDate", "Proc Date should be in format YYYY-MM-DD");
        rejected(add(form().put("amount", "1").put("origDate", "x")), "amount",
                "Amount should be in format -99999999.99");
    }

    @Test
    void R26_amountReEditedDatesValidatedThenMerchantIdAndReferences() throws Exception {
        addWith("amount", "+00000099.90").andExpect(jsonPath("$.form.amount").value("+00000099.90"));
        rejected(addWith("origDate", "2022-02-30"), "origDate", "Orig Date - Not a valid date...");
        rejected(addWith("procDate", "2022-13-01"), "procDate", "Proc Date - Not a valid date...");
        rejected(add(form().put("procDate", "2022-13-01").put("merchantId", "12A")), "procDate",
                "Proc Date - Not a valid date...");
        rejected(addWith("merchantId", "12A"), "merchantId", "Merchant ID must be Numeric...");
        rejected(addWith("typeCode", "09"), "typeCode", "Type CD not found in TRANTYPE...");
        rejected(add(form().put("typeCode", "02").put("categoryCode", "1")), "categoryCode",
                "Category CD not found in TRANCATG for this Type CD...");
    }

    @Test
    void R27a_confirmYAdds() throws Exception {
        for (String y : List.of("Y", "y")) {
            add(form().put("confirm", y)).andExpect(status().isCreated())
                    .andExpect(jsonPath("$.state").value("ADDED"));
        }
    }

    @Test
    void R27b_confirmNOrBlankAsksForConfirmation() throws Exception {
        for (String n : List.of("N", "n", "", " ")) {
            add(form().put("confirm", n)).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("VALIDATED"))
                    .andExpect(jsonPath("$.message").value("Confirm to add this transaction..."));
        }
        verify(transactions, never()).saveAndFlush(any());
    }

    @Test
    void R27c_otherConfirmValuesAreInvalid() throws Exception {
        rejected(addWith("confirm", "X"), "confirm", "Invalid value. Valid values are (Y/N)...");
        verify(transactions, never()).saveAndFlush(any());
    }

    @Test
    void R28_nextIdIsTheLastPlusOneUnderTheIdLock() throws Exception {
        add(form().put("confirm", "Y")).andExpect(jsonPath("$.transaction.tranId").value(id(24)));
        InOrder order = inOrder(transactions);
        order.verify(transactions).lockIdAssignment(TransactionRepository.TRAN_ID_LOCK);
        order.verify(transactions).findFirstByOrderByTranIdDesc();
        order.verify(transactions).saveAndFlush(any(Transaction.class));
        store.clear();
        add(form().put("confirm", "Y")).andExpect(jsonPath("$.transaction.tranId").value(id(1)));
    }

    @Test
    void R29_theRecordIsBuiltFromTheEditedFields() throws Exception {
        add(form().put("confirm", "Y").put("typeCode", "1").put("categoryCode", "2")).andExpect(status().isCreated());
        Transaction t = store.get(id(24));
        assertThat(t.getTranTypeCd()).isEqualTo("01");
        assertThat(t.getTranCatCd()).isEqualTo(2);
        assertThat(t.getSource()).isEqualTo("POS TERM");
        assertThat(t.getDescription()).isEqualTo("Online purchase");
        assertThat(t.getAmount()).isEqualByComparingTo(new BigDecimal("-12.34"));
        assertThat(t.getCardNum()).isEqualTo(card(1));
        assertThat(t.getMerchantId()).isEqualTo(1);
        assertThat(t.getMerchantName()).isEqualTo("Corner Store");
        assertThat(t.getMerchantCity()).isEqualTo("Seattle");
        assertThat(t.getMerchantZip()).isEqualTo("98101");
        assertThat(t.getOrigTs()).isEqualTo("2022-07-06");
        assertThat(t.getProcTs()).isEqualTo("2022-07-06");
    }

    @Test
    void R30_successClearsTheFormAndReportsTheId() throws Exception {
        JsonNode added = body(add(form().put("confirm", "Y")).andExpect(status().isCreated())
                .andExpect(header().string(HttpHeaders.LOCATION, TRANSACTIONS + "/" + id(24))));
        assertThat(added.get("message").asText())
                .isEqualTo("Transaction added successfully.  Your Tran ID is " + id(24) + ".");
        added.get("form").fields().forEachRemaining(f -> {
            if (!f.getKey().equals("copyLast")) {
                assertThat(f.getValue().asText()).as(f.getKey()).isEmpty();
            }
        });
        assertThat(added.get("transaction").get("amount").asText()).isEqualTo("-00000012.34");
    }

    @Test
    void R31_duplicateIdIs409AndOtherWriteErrorsAbend() throws Exception {
        given(transactions.findFirstByOrderByTranIdDesc()).willReturn(Optional.of(store.get(id(22))));
        add(form().put("confirm", "Y")).andExpect(status().isConflict()).andExpect(jsonPath("$.code").value("DUPREC"))
                .andExpect(jsonPath("$.message").value("Tran ID already exist..."));
        given(transactions.saveAndFlush(any(Transaction.class)))
                .willThrow(new DataIntegrityViolationException("check constraint"));
        add(form().put("confirm", "Y")).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to Add Transaction...")));
    }

    @Test
    void R32_pf5NeedsValidKeysAndALastRecord() throws Exception {
        rejected(add(json.createObjectNode().put("copyLast", true)), "accountId",
                "Account or Card Number must be entered...");
        add(json.createObjectNode().put("cardNumber", card(2)).put("copyLast", true)).andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value("Confirm to add this transaction..."));
        store.clear();
        add(json.createObjectNode().put("accountId", "1").put("copyLast", true)).andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Transaction ID NOT found..."));
    }

    @Test
    void R33_returnCarriesTheFromFields() throws Exception {
        add(form()).andExpect(jsonPath("$.exit.fromTranId").value("CT02"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COTRN02C"));
    }

    @Test
    void R34_standardHeader() throws Exception {
        add(form()).andExpect(jsonPath("$.header.tranId").value("CT02"))
                .andExpect(jsonPath("$.header.programName").value("COTRN02C"));
    }
}
