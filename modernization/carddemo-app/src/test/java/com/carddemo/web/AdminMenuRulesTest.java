package com.carddemo.web;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.nullValue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.user.UserType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpHeaders;

/** {@code docs/modernization/rules/COADM01C.md}: one test per rule, R-id in the name. */
class AdminMenuRulesTest extends OnlineWebTest {

    private String admin() {
        return bearer("ADMIN001", UserType.ADMIN);
    }

    @Test
    void R1_noSessionContextReturnsToSignOn() throws Exception {
        mvc.perform(get(MENU + "/admin"))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.code").value("SIGNON_REQUIRED"))
                .andExpect(jsonPath("$.toProgram").value("COSGN00C"));
    }

    @Test
    void R2_firstEntrySendsTheAdminMenu() throws Exception {
        mvc.perform(get(MENU).header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.menu").value("admin"))
                .andExpect(jsonPath("$.programId").value("COADM01C"))
                .andExpect(jsonPath("$.map").value("COADM1A"))
                .andExpect(jsonPath("$.message").value(""))
                .andExpect(jsonPath("$.options", hasSize(6)));
    }

    @Test
    void R3_enterProcessesTheSelection() throws Exception {
        mvc.perform(select("admin", "1", UserType.ADMIN))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation.toProgram").value("COUSR00C"));
    }

    @Test
    void R4_pf3ReturnsToSignOn() throws Exception {
        mvc.perform(post(MENU + "/admin/exit").header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.fromProgram").value("COADM01C"))
                .andExpect(jsonPath("$.fromTranId").value("CA00"))
                .andExpect(jsonPath("$.toProgram").value("COSGN00C"));
    }

    @Test
    void R5_anyOtherKeyAnswersTheInvalidKeyMessage() throws Exception {
        mvc.perform(put(MENU + "/admin/selection").header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isMethodNotAllowed())
                .andExpect(jsonPath("$.code").value("INVALID_KEY"))
                .andExpect(jsonPath("$.message").value("Invalid key pressed. Please see below..."));
    }

    @Test
    void R6_aSelectionThatDoesNotTransferStaysOnTheMenu() throws Exception {
        mvc.perform(select("admin", "5", UserType.ADMIN))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation").value(nullValue()));
        mvc.perform(select("admin", "7", UserType.ADMIN))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.navigation").doesNotExist());
    }

    @ParameterizedTest
    @CsvSource({"'4 ',04,COUSR03C", "' 4',04,COUSR03C", "4,04,COUSR03C", "02,02,COUSR01C"})
    void R7_theOptionIsNormalisedLikeCobol(String input, String normalized, String target) throws Exception {
        mvc.perform(select("admin", input, UserType.ADMIN))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.option").value(normalized))
                .andExpect(jsonPath("$.navigation.toProgram").value(target));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "0", "00", "7", "12", "x", "4x"})
    void R8_anInvalidOptionNumberIsRejected(String input) throws Exception {
        mvc.perform(select("admin", input, UserType.ADMIN))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value("option"))
                .andExpect(jsonPath("$.message").value("Please enter a valid option number..."));
    }

    @ParameterizedTest
    @CsvSource({"1,COUSR00C,CU00", "2,COUSR01C,CU01", "3,COUSR02C,CU02", "4,COUSR03C,CU03"})
    void R9_aValidNonDummyOptionTransfersWithTheAdminMenuAsFromProgram(String option, String program,
            String tranId) throws Exception {
        mvc.perform(select("admin", option, UserType.ADMIN))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation.fromTranId").value("CA00"))
                .andExpect(jsonPath("$.navigation.fromProgram").value("COADM01C"))
                .andExpect(jsonPath("$.navigation.toProgram").value(program))
                .andExpect(jsonPath("$.navigation.toTranId").value(tranId))
                .andExpect(jsonPath("$.navigation.pgmContext").value("ENTER"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"5", "6"})
    void R10_aRowWhoseProgramIsNotInstalledAnswersNotInstalled(String option) throws Exception {
        mvc.perform(select("admin", option, UserType.ADMIN))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value("This option is not installed ..."))
                .andExpect(jsonPath("$.messageColor").value("GREEN"));
    }

    @Test
    void R11_pgmidErrorIsHandledAsNotInstalledInsteadOfAbending() throws Exception {
        // MAIN-PARA's HANDLE CONDITION PGMIDERR is active, so XCTL to COTRTLIC re-sends the menu (no abend).
        mvc.perform(select("admin", "5", UserType.ADMIN))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation").value(nullValue()))
                .andExpect(jsonPath("$.message").value("This option is not installed ..."));
    }

    @Test
    void R12_everySendCarriesTheHeaderFields() throws Exception {
        mvc.perform(get(MENU + "/admin").header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(jsonPath("$.header.tranId").value("CA00"))
                .andExpect(jsonPath("$.header.programName").value("COADM01C"))
                .andExpect(jsonPath("$.header.currentDate").value("07/06/22"))
                .andExpect(jsonPath("$.header.currentTime").value("13:45:10"))
                .andExpect(jsonPath("$.header.applId").value("CARDDEMO"));
    }

    @Test
    void R13_optionTextIsTheTwoDigitNumberDotSpaceNameAndUnusedFieldsAreBlank() throws Exception {
        mvc.perform(get(MENU + "/admin").header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(jsonPath("$.optionLines", hasSize(12)))
                .andExpect(jsonPath("$.optionLines[0]").value("01. User List (Security)"))
                .andExpect(jsonPath("$.optionLines[5]").value("06. Transaction Type Maintenance (Db2)"))
                .andExpect(jsonPath("$.optionLines[6]").value(""))
                .andExpect(jsonPath("$.optionLines[11]").value(""))
                .andExpect(jsonPath("$.options[4].programId").value("COTRTLIC"));
    }
}
