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

/**
 * {@code docs/modernization/rules/COMEN01C.md}: one test per rule, R-id in the name. R-9 (admin-only rows) and R-11
 * (DUMMY rows) need table rows the shipped {@code COMEN02Y} does not have: see {@link MainMenuTableVariantTest}.
 */
class MainMenuRulesTest extends OnlineWebTest {

    @Test
    void R1_noSessionContextReturnsToSignOn() throws Exception {
        mvc.perform(get(MENU + "/main"))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.code").value("SIGNON_REQUIRED"))
                .andExpect(jsonPath("$.toProgram").value("COSGN00C"));
        mvc.perform(get(MENU).header(HttpHeaders.AUTHORIZATION, "Bearer not.a.token"))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.code").value("SIGNON_REQUIRED"));
    }

    @Test
    void R2_firstEntrySendsTheMenuWithAnEmptyMessage() throws Exception {
        mvc.perform(get(MENU).header(HttpHeaders.AUTHORIZATION, bearer("USER0001", UserType.USER)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.menu").value("main"))
                .andExpect(jsonPath("$.programId").value("COMEN01C"))
                .andExpect(jsonPath("$.map").value("COMEN1A"))
                .andExpect(jsonPath("$.message").value(""))
                .andExpect(jsonPath("$.options", hasSize(11)));
    }

    @Test
    void R3_enterProcessesTheSelection() throws Exception {
        mvc.perform(select("main", "1", UserType.USER))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation.toProgram").value("COACTVWC"));
    }

    @Test
    void R4_pf3ReturnsToSignOn() throws Exception {
        mvc.perform(post(MENU + "/main/exit").header(HttpHeaders.AUTHORIZATION, bearer("USER0001", UserType.USER)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.fromProgram").value("COMEN01C"))
                .andExpect(jsonPath("$.fromTranId").value("CM00"))
                .andExpect(jsonPath("$.toProgram").value("COSGN00C"))
                .andExpect(jsonPath("$.toTranId").value("CC00"));
    }

    @Test
    void R5_anyOtherKeyAnswersTheInvalidKeyMessage() throws Exception {
        mvc.perform(put(MENU + "/main/selection").header(HttpHeaders.AUTHORIZATION, bearer("USER0001",
                UserType.USER)))
                .andExpect(status().isMethodNotAllowed())
                .andExpect(jsonPath("$.code").value("INVALID_KEY"))
                .andExpect(jsonPath("$.message").value("Invalid key pressed. Please see below..."));
    }

    @Test
    void R6_aSelectionThatDoesNotTransferStaysOnTheMenu() throws Exception {
        mvc.perform(select("main", "11", UserType.USER))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation").value(nullValue()));
        mvc.perform(select("main", "99", UserType.USER))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.navigation").doesNotExist());
    }

    @ParameterizedTest
    @CsvSource({"'1 ',01,COACTVWC", "' 1',01,COACTVWC", "1,01,COACTVWC", "01,01,COACTVWC", "'7 ',07,COTRN01C",
        "10,10,COBIL00C"})
    void R7_theOptionIsNormalisedLikeCobol(String input, String normalized, String target) throws Exception {
        mvc.perform(select("main", input, UserType.USER))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.option").value(normalized))
                .andExpect(jsonPath("$.navigation.toProgram").value(target));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "  ", "0", "00", "12", "99", "a", "1a", "-1"})
    void R8_anInvalidOptionNumberIsRejected(String input) throws Exception {
        mvc.perform(select("main", input, UserType.USER))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value("option"))
                .andExpect(jsonPath("$.message").value("Please enter a valid option number..."));
    }

    @Test
    void R9_anAdminOnlyTargetIsRejectedForAUserTokenWithTheProgramsMessage() throws Exception {
        mvc.perform(select("admin", "1", UserType.USER))
                .andExpect(status().isForbidden())
                .andExpect(jsonPath("$.code").value("NOTAUTH"))
                .andExpect(jsonPath("$.field").value(nullValue()))
                .andExpect(jsonPath("$.message").value("No access - Admin Only option..."));
        mvc.perform(get(MENU + "/admin").header(HttpHeaders.AUTHORIZATION, bearer("USER0001", UserType.USER)))
                .andExpect(status().isForbidden())
                .andExpect(jsonPath("$.message").value("No access - Admin Only option..."));
    }

    @Test
    void R10_pendingAuthorizationViewIsNotInstalledInThisEstate() throws Exception {
        mvc.perform(select("main", "11", UserType.USER))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.option").value("11"))
                .andExpect(jsonPath("$.message").value("This option Pending Authorization View is not installed..."))
                .andExpect(jsonPath("$.messageColor").value("RED"));
    }

    @Test
    void R11_dummyRowsAreCoveredByTheTableVariant() {
        // R-11 needs a DUMMY row; COMEN02Y has none. MainMenuTableVariantTest.R11_* asserts it over MockMvc.
        org.assertj.core.api.Assertions.assertThat(com.carddemo.user.menu.MenuCatalog.MAIN.options())
                .noneMatch(com.carddemo.user.menu.MenuOption::dummy);
    }

    @ParameterizedTest
    @CsvSource({"1,COACTVWC,CAVW", "2,COACTUPC,CAUP", "3,COCRDLIC,CCLI", "4,COCRDSLC,CCDL", "5,COCRDUPC,CCUP",
        "6,COTRN00C,CT00", "7,COTRN01C,CT01", "8,COTRN02C,CT02", "9,CORPT00C,CR00", "10,COBIL00C,CB00"})
    void R12_anyOtherOptionTransfersWithTheMenuAsFromProgram(String option, String program, String tranId)
            throws Exception {
        mvc.perform(select("main", option, UserType.USER))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation.fromTranId").value("CM00"))
                .andExpect(jsonPath("$.navigation.fromProgram").value("COMEN01C"))
                .andExpect(jsonPath("$.navigation.toProgram").value(program))
                .andExpect(jsonPath("$.navigation.toTranId").value(tranId))
                .andExpect(jsonPath("$.navigation.pgmContext").value("ENTER"))
                .andExpect(jsonPath("$.message").value(""));
    }

    @Test
    void R13_everySendCarriesTheHeaderFields() throws Exception {
        mvc.perform(get(MENU + "/main").header(HttpHeaders.AUTHORIZATION, bearer("USER0001", UserType.USER)))
                .andExpect(jsonPath("$.header.tranId").value("CM00"))
                .andExpect(jsonPath("$.header.programName").value("COMEN01C"))
                .andExpect(jsonPath("$.header.currentDate").value("07/06/22"))
                .andExpect(jsonPath("$.header.currentTime").value("13:45:10"))
                .andExpect(jsonPath("$.header.title01").value("AWS Mainframe Modernization"));
    }

    @Test
    void R14_optionTextIsTheTwoDigitNumberDotSpaceName() throws Exception {
        mvc.perform(get(MENU + "/main").header(HttpHeaders.AUTHORIZATION, bearer("USER0001", UserType.USER)))
                .andExpect(jsonPath("$.optionLines", hasSize(12)))
                .andExpect(jsonPath("$.optionLines[0]").value("01. Account View"))
                .andExpect(jsonPath("$.optionLines[8]").value("09. Transaction Reports"))
                .andExpect(jsonPath("$.optionLines[10]").value("11. Pending Authorization View"))
                .andExpect(jsonPath("$.optionLines[11]").value(""))
                .andExpect(jsonPath("$.options[0].number").value(1))
                .andExpect(jsonPath("$.options[0].label").value("01. Account View"))
                .andExpect(jsonPath("$.options[0].programId").value("COACTVWC"))
                .andExpect(jsonPath("$.options[0].adminOnly").value(false));
    }

    @Test
    void anAdministratorMayAlsoUseTheMainMenu() throws Exception {
        mvc.perform(select("main", "1", UserType.ADMIN))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation.toProgram").value("COACTVWC"));
    }

    @Test
    void anUnknownMenuIsNotFound() throws Exception {
        mvc.perform(get(MENU + "/nosuch").header(HttpHeaders.AUTHORIZATION, bearer("USER0001", UserType.USER)))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.code").value("NOTFND"));
    }
}
