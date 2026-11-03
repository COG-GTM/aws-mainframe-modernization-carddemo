package com.carddemo.auth.flow;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.testsupport.flow.CobolFlow;
import com.carddemo.testsupport.flow.ScreenKey;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;

/**
 * CC00 sign-on (COSGN00C, map COSGN0A) and the two menus it routes to: CM00 (COMEN01C, COMEN02Y options)
 * for type U and CA00 (COADM01C, COADM02Y options) for type A.
 */
class SignonAndMenuFlowTest extends AuthFlowTestSupport {

    private ResultActions signOnWith(String json) throws Exception {
        return mockMvc.perform(post("/api/v1/auth/signon").contentType(MediaType.APPLICATION_JSON).content(json));
    }

    @Test
    @CobolFlow(program = "COSGN00C", paragraph = "PROCESS-ENTER-KEY", key = ScreenKey.ENTER)
    void COSGN00C_PROCESS_ENTER_KEY_blankUserIdAsksForTheUserId() throws Exception {
        signOnWith("{\"userId\":\"  \",\"password\":\"PASSWORD\"}")
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("userId"))
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Please enter User ID ..."));
    }

    @Test
    @CobolFlow(program = "COSGN00C", paragraph = "PROCESS-ENTER-KEY", key = ScreenKey.ENTER)
    void COSGN00C_PROCESS_ENTER_KEY_blankPasswordAsksForThePassword() throws Exception {
        signOnWith("{\"userId\":\"ADMIN001\",\"password\":\"\"}")
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("password"))
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Please enter Password ..."));
    }

    @Test
    @CobolFlow(program = "COSGN00C", paragraph = "READ-USER-SEC-FILE", key = ScreenKey.ENTER)
    void COSGN00C_READ_USER_SEC_FILE_unknownUserIsNotFound() throws Exception {
        signOnWith("{\"userId\":\"NOSUCHID\",\"password\":\"PASSWORD\"}")
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("User not found. Try again ..."));
    }

    @Test
    @CobolFlow(program = "COSGN00C", paragraph = "READ-USER-SEC-FILE", key = ScreenKey.ENTER)
    void COSGN00C_READ_USER_SEC_FILE_wrongPasswordIsRefused() throws Exception {
        signOnWith("{\"userId\":\"USER0001\",\"password\":\"WRONGPWD\"}")
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("Wrong Password. Try again ..."));
    }

    /** COBOL upper-cases both fields (FUNCTION UPPER-CASE) before the read and the compare. */
    @Test
    @CobolFlow(program = "COSGN00C", paragraph = "READ-USER-SEC-FILE", key = ScreenKey.ENTER)
    void COSGN00C_READ_USER_SEC_FILE_adminIsRoutedToTheAdminMenu() throws Exception {
        signOnWith("{\"userId\":\"admin001\",\"password\":\"password\"}")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.userId").value("ADMIN001"))
                .andExpect(jsonPath("$.firstName").value("MARGARET"))
                .andExpect(jsonPath("$.userType").value("A"))
                .andExpect(jsonPath("$.nextScreen").value("ADMIN"))
                .andExpect(jsonPath("$.tokenType").value("Bearer"))
                .andExpect(jsonPath("$.token").isNotEmpty());
    }

    @Test
    @CobolFlow(program = "COSGN00C", paragraph = "READ-USER-SEC-FILE", key = ScreenKey.ENTER)
    void COSGN00C_READ_USER_SEC_FILE_regularUserIsRoutedToTheMainMenu() throws Exception {
        signOnWith("{\"userId\":\"USER0001\",\"password\":\"PASSWORD\"}")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.userType").value("U"))
                .andExpect(jsonPath("$.firstName").value("LAWRENCE"))
                .andExpect(jsonPath("$.nextScreen").value("MAIN"));
    }

    @Test
    @CobolFlow(program = "COMEN01C", paragraph = "BUILD-MENU-OPTIONS", key = ScreenKey.ENTER)
    void COMEN01C_BUILD_MENU_OPTIONS_userSeesTheElevenMainMenuOptions() throws Exception {
        String token = signOn("USER0001", "PASSWORD");
        mockMvc.perform(get("/api/v1/menu").header(HttpHeaders.AUTHORIZATION, bearer(token)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.menu").value("MAIN"))
                .andExpect(jsonPath("$.transactionId").value("CM00"))
                .andExpect(jsonPath("$.program").value("COMEN01C"))
                .andExpect(jsonPath("$.options.length()").value(11))
                .andExpect(jsonPath("$.options[0].name").value("Account View"))
                .andExpect(jsonPath("$.options[0].program").value("COACTVWC"))
                .andExpect(jsonPath("$.options[4].program").value("COCRDUPC"))
                .andExpect(jsonPath("$.options[8].program").value("CORPT00C"))
                .andExpect(jsonPath("$.options[9].program").value("COBIL00C"))
                .andExpect(jsonPath("$.options[10].name").value("Pending Authorization View"))
                .andExpect(jsonPath("$.options[10].available").value(false))
                .andExpect(jsonPath("$.options[?(@.adminOnly == true)]").isEmpty());
    }

    @Test
    @CobolFlow(program = "COMEN01C", paragraph = "PROCESS-ENTER-KEY", key = ScreenKey.ENTER)
    void COMEN01C_PROCESS_ENTER_KEY_validOptionNamesTheProgramToTransferTo() throws Exception {
        String token = signOn("USER0002", "PASSWORD");
        mockMvc.perform(get("/api/v1/menu/options/6").header(HttpHeaders.AUTHORIZATION, bearer(token)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.program").value("COTRN00C"))
                .andExpect(jsonPath("$.endpoint").value("GET /api/v1/transactions"));
    }

    @Test
    @CobolFlow(program = "COMEN01C", paragraph = "PROCESS-ENTER-KEY", key = ScreenKey.ENTER)
    void COMEN01C_PROCESS_ENTER_KEY_optionOutOfRangeIsRejected() throws Exception {
        String token = signOn("USER0002", "PASSWORD");
        for (int option : new int[] {0, 12}) {
            mockMvc.perform(get("/api/v1/menu/options/" + option).header(HttpHeaders.AUTHORIZATION, bearer(token)))
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.message").value("Please enter a valid option number..."));
        }
    }

    @Test
    @CobolFlow(program = "COMEN01C", paragraph = "PROCESS-ENTER-KEY", key = ScreenKey.ENTER)
    void COMEN01C_PROCESS_ENTER_KEY_optionNotInstalledIsReported() throws Exception {
        String token = signOn("USER0002", "PASSWORD");
        mockMvc.perform(get("/api/v1/menu/options/11").header(HttpHeaders.AUTHORIZATION, bearer(token)))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("This option Pending Authorization View is not installed..."));
    }

    /**
     * COMEN01C refuses an 'A' option to a 'U' user with "No access - Admin Only option... ". Every
     * COMEN02Y option is 'U', so that branch cannot be reached from the main menu; the same check guards
     * the admin functions themselves, which a 'U' token reaches directly over the API.
     */
    @Test
    @CobolFlow(program = "COMEN01C", paragraph = "PROCESS-ENTER-KEY", key = ScreenKey.ENTER)
    void COMEN01C_PROCESS_ENTER_KEY_adminOnlyFunctionIsRefusedToAUser() throws Exception {
        String token = signOn("USER0003", "PASSWORD");
        mockMvc.perform(get("/api/v1/users").header(HttpHeaders.AUTHORIZATION, bearer(token)))
                .andExpect(status().isForbidden())
                .andExpect(jsonPath("$.message").value("No access - Admin Only option... "));
    }

    @Test
    @CobolFlow(program = "COMEN01C", paragraph = "MAIN-PARA", key = ScreenKey.ENTER)
    void COMEN01C_MAIN_PARA_menuWithoutSignOnIsRefused() throws Exception {
        // COMEN01C without a COMMAREA (EIBCALEN = 0) goes back to the sign-on screen.
        mockMvc.perform(get("/api/v1/menu")).andExpect(status().isUnauthorized());
    }

    @Test
    @CobolFlow(program = "COADM01C", paragraph = "BUILD-MENU-OPTIONS", key = ScreenKey.ENTER)
    void COADM01C_BUILD_MENU_OPTIONS_adminSeesTheSixAdminMenuOptions() throws Exception {
        String token = signOn("ADMIN001", "PASSWORD");
        mockMvc.perform(get("/api/v1/menu").header(HttpHeaders.AUTHORIZATION, bearer(token)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.menu").value("ADMIN"))
                .andExpect(jsonPath("$.transactionId").value("CA00"))
                .andExpect(jsonPath("$.program").value("COADM01C"))
                .andExpect(jsonPath("$.options.length()").value(6))
                .andExpect(jsonPath("$.options[0].name").value("User List (Security)"))
                .andExpect(jsonPath("$.options[0].program").value("COUSR00C"))
                .andExpect(jsonPath("$.options[1].program").value("COUSR01C"))
                .andExpect(jsonPath("$.options[2].program").value("COUSR02C"))
                .andExpect(jsonPath("$.options[3].program").value("COUSR03C"))
                .andExpect(jsonPath("$.options[4].available").value(false))
                .andExpect(jsonPath("$.options[5].available").value(false));
    }

    @Test
    @CobolFlow(program = "COADM01C", paragraph = "PROCESS-ENTER-KEY", key = ScreenKey.ENTER)
    void COADM01C_PROCESS_ENTER_KEY_validOptionNamesTheProgramToTransferTo() throws Exception {
        String token = signOn("ADMIN002", "PASSWORD");
        mockMvc.perform(get("/api/v1/menu/options/4").header(HttpHeaders.AUTHORIZATION, bearer(token)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.program").value("COUSR03C"))
                .andExpect(jsonPath("$.endpoint").value("DELETE /api/v1/users/{userId}"));
    }

    @Test
    @CobolFlow(program = "COADM01C", paragraph = "PROCESS-ENTER-KEY", key = ScreenKey.ENTER)
    void COADM01C_PROCESS_ENTER_KEY_optionOutOfRangeIsRejected() throws Exception {
        String token = signOn("ADMIN002", "PASSWORD");
        for (int option : new int[] {0, 7}) {
            mockMvc.perform(get("/api/v1/menu/options/" + option).header(HttpHeaders.AUTHORIZATION, bearer(token)))
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.message").value("Please enter a valid option number..."));
        }
    }

    /** COADM01C's PGMIDERR handler leaves the option name out of the text. */
    @Test
    @CobolFlow(program = "COADM01C", paragraph = "PGMIDERR-ERR-PARA", key = ScreenKey.ENTER)
    void COADM01C_PGMIDERR_ERR_PARA_optionNotInstalledIsReported() throws Exception {
        String token = signOn("ADMIN002", "PASSWORD");
        mockMvc.perform(get("/api/v1/menu/options/5").header(HttpHeaders.AUTHORIZATION, bearer(token)))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("This option is not installed ..."));
    }
}
