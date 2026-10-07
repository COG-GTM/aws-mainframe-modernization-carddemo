package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.user.UserSecurity;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.http.HttpHeaders;
import org.springframework.test.web.servlet.ResultActions;

/** {@code docs/modernization/rules/COUSR03C.md} against {@code DELETE /api/v1/users/{id}}. */
class UserDeleteRulesTest extends UserWebTest {

    private static final String ID = "USER0007";

    private static void rejected(ResultActions result, String field, String message) throws Exception {
        result.andExpect(status().isBadRequest()).andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value(field)).andExpect(jsonPath("$.message").value(message));
    }

    private ResultActions confirmDelete(String id) throws Exception {
        return remove(id, "confirm", "Y", "version", "0");
    }

    @Test
    void R1_noSessionReturnsToSignOn() throws Exception {
        mvc.perform(delete(USERS + "/{id}", ID)).andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.toProgram").value("COSGN00C"));
    }

    @Test
    void R2_noUserSelectedAsksForAUserId() throws Exception {
        rejected(remove(" "), "userId", "User ID can NOT be empty...");
    }

    @Test
    void R3_aSelectedUserIsLookedUpFirst() throws Exception {
        JsonNode selected = body(select(List.of(ID), List.of("d")).andExpect(status().isOk()));
        assertThat(selected.get("next").asText()).isEqualTo("DELETE /api/v1/users/USER0007?fromProgram=COUSR00C");
        remove(ID, "fromProgram", "COUSR00C").andExpect(jsonPath("$.state").value("VALIDATED"));
        assertThat(store).containsKey(ID);
    }

    @Test
    void R4_enterLooksTheUserUp() throws Exception {
        remove(ID).andExpect(status().isOk()).andExpect(jsonPath("$.user.userId").value(ID));
    }

    @Test
    void R5_pf3ReturnsWithoutDeleting() throws Exception {
        remove(ID, "fromProgram", "COUSR00C").andExpect(jsonPath("$.exit.toProgram").value("COUSR00C"));
        remove(ID).andExpect(jsonPath("$.exit.toProgram").value("COADM01C"));
        assertThat(store).containsKey(ID);
    }

    @Test
    void R6_clearShowsAnEmptyScreen() throws Exception {
        remove(ID, "confirm", "N").andExpect(status().isOk()).andExpect(jsonPath("$.state").value("CANCELLED"))
                .andExpect(jsonPath("$.user").doesNotExist()).andExpect(jsonPath("$.message").value(""));
        assertThat(store).containsKey(ID);
    }

    @Test
    void R7_pf5Deletes() throws Exception {
        confirmDelete(ID).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("DELETED"));
        assertThat(store).doesNotContainKey(ID);
    }

    @Test
    void R8_pf12ReturnsToTheAdminMenu() throws Exception {
        remove(ID, "fromProgram", " ").andExpect(jsonPath("$.exit.toProgram").value("COADM01C"))
                .andExpect(jsonPath("$.exit.toTranId").value("CA00"));
    }

    @Test
    void R9_otherKeysAreInvalid() throws Exception {
        mvc.perform(patch(USERS + "/{id}", ID).header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isMethodNotAllowed()).andExpect(jsonPath("$.code").value("INVALID_KEY"));
        rejected(remove(ID, "confirm", "X"), "confirm", "\"X\" is not a valid value to confirm...");
    }

    @Test
    void R10_enterWithABlankUserIdIsRejected() throws Exception {
        rejected(remove("  "), "userId", "User ID can NOT be empty...");
    }

    @Test
    void R11_theNamesAndTypeAreShownWithoutThePassword() throws Exception {
        remove(ID).andExpect(jsonPath("$.user.firstName").value("First7"))
                .andExpect(jsonPath("$.user.lastName").value("Last7"))
                .andExpect(jsonPath("$.user.userType").value("U"))
                .andExpect(jsonPath("$.user.password").doesNotExist());
    }

    @Test
    void R12_pf5WithABlankUserIdIsRejected() throws Exception {
        rejected(remove(" ", "confirm", "Y", "version", "0"), "userId", "User ID can NOT be empty...");
    }

    @Test
    void R13_pf5ReadsForUpdateBeforeTheDelete() throws Exception {
        store.get(ID).getVersion();
        org.springframework.test.util.ReflectionTestUtils.setField(store.get(ID), "version", 3L);
        confirmDelete(ID).andExpect(status().isConflict()).andExpect(jsonPath("$.code").value("CHANGED"));
        assertThat(store).containsKey(ID);
        rejected(remove(ID, "confirm", "Y"), "version",
                com.carddemo.user.admin.UserAdminMessages.MSG_VERSION_REQUIRED);
        remove(ID, "confirm", "Y", "version", "3").andExpect(status().isOk());
        assertThat(store).doesNotContainKey(ID);
    }

    @Test
    void R14_aSuccessfulReadAsksForPf5() throws Exception {
        remove(ID).andExpect(jsonPath("$.message").value("Press PF5 key to delete this user ..."));
    }

    @Test
    void R15_anUnknownUserIsNotFound() throws Exception {
        remove("NOBODY").andExpect(status().isNotFound()).andExpect(jsonPath("$.code").value("NOTFND"))
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
    }

    @Test
    void R16_otherReadErrorsCannotLookUp() throws Exception {
        given(users.findById(anyString())).willThrow(new DataAccessResourceFailureException("down"));
        remove(ID).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to lookup User...")));
    }

    @Test
    void R17_aDeleteClearsTheScreenWithAGreenMessage() throws Exception {
        confirmDelete(ID).andExpect(jsonPath("$.message").value("User USER0007 has been deleted ..."));
    }

    @Test
    void R18_pf5OnAnUnknownUserIsNotFound() throws Exception {
        confirmDelete("NOBODY").andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
    }

    @Test
    void R19_otherDeleteErrorsSayUnableToUpdate() throws Exception {
        willThrow(new DataAccessResourceFailureException("down")).given(users).delete(any(UserSecurity.class));
        confirmDelete(ID).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to Update User...")));
    }

    @Test
    void R20_returnCarriesTheFromFields() throws Exception {
        remove(ID).andExpect(jsonPath("$.exit.fromTranId").value("CU03"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COUSR03C"));
    }

    @Test
    void R21_theScreenHeaderNamesCu03() throws Exception {
        remove(ID).andExpect(jsonPath("$.header.tranId").value("CU03"))
                .andExpect(jsonPath("$.header.programName").value("COUSR03C"));
    }

    @Test
    void anAdminMayDeleteThemselvesLikeTheCobol() throws Exception {
        remove(ADMIN, "confirm", "Y", "version", "0").andExpect(status().isOk());
        assertThat(store).doesNotContainKey(ADMIN);
    }
}
