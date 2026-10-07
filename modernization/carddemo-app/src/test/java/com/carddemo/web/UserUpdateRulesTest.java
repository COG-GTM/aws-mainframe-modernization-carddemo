package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserType;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.http.HttpHeaders;
import org.springframework.test.web.servlet.ResultActions;

/** {@code docs/modernization/rules/COUSR02C.md} against {@code GET/PUT /api/v1/users/{id}}. */
class UserUpdateRulesTest extends UserWebTest {

    private static final String ID = "USER0003";

    private static void rejected(ResultActions result, String field, String message) throws Exception {
        result.andExpect(status().isBadRequest()).andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value(field)).andExpect(jsonPath("$.message").value(message));
    }

    @Test
    void R1_noSessionReturnsToSignOn() throws Exception {
        mvc.perform(get(USERS + "/{id}", ID)).andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.toProgram").value("COSGN00C"));
    }

    @Test
    void R2_noUserSelectedAsksForAUserId() throws Exception {
        rejected(fetch(" "), "userId", "User ID can NOT be empty...");
    }

    @Test
    void R3_aSelectedUserIsPreloaded() throws Exception {
        JsonNode selected = body(select(List.of(ID), List.of("U")).andExpect(status().isOk()));
        assertThat(selected.get("next").asText()).isEqualTo("GET /api/v1/users/USER0003?fromProgram=COUSR00C");
        fetch(ID, "fromProgram", "COUSR00C").andExpect(status().isOk())
                .andExpect(jsonPath("$.user.firstName").value("First3"));
    }

    @Test
    void R4_enterLooksTheUserUp() throws Exception {
        fetch(ID).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("SHOW"))
                .andExpect(jsonPath("$.user.userId").value(ID));
    }

    @Test
    void R5_pf3ReturnsToTheCallerOrTheAdminMenu() throws Exception {
        fetch(ID, "fromProgram", "COUSR00C").andExpect(jsonPath("$.exit.toProgram").value("COUSR00C"))
                .andExpect(jsonPath("$.exit.toTranId").value("CU00"));
        update(ID, updateForm(ID).put("lastName", "Changed"), "fromProgram", "COUSR00C")
                .andExpect(jsonPath("$.exit.toProgram").value("COUSR00C"));
        fetch(ID).andExpect(jsonPath("$.exit.toProgram").value("COADM01C"));
    }

    @Test
    void R6_clearKeepsNothingOnTheServer() throws Exception {
        rejected(update(ID, updateForm(ID).put("firstName", "")), "firstName", "First Name can NOT be empty...");
        fetch(ID).andExpect(jsonPath("$.user.firstName").value("First3"));
    }

    @Test
    void R7_pf5SavesTheUpdates() throws Exception {
        update(ID, updateForm(ID).put("firstName", "Renamed")).andExpect(status().isOk())
                .andExpect(jsonPath("$.state").value("UPDATED"));
        assertThat(store.get(ID).getFirstName()).isEqualTo("Renamed");
    }

    @Test
    void R8_pf12ReturnsToTheAdminMenu() throws Exception {
        fetch(ID, "fromProgram", "").andExpect(jsonPath("$.exit.toProgram").value("COADM01C"))
                .andExpect(jsonPath("$.exit.toTranId").value("CA00"));
    }

    @Test
    void R8a_otherKeysAreInvalid() throws Exception {
        mvc.perform(patch(USERS + "/{id}", ID).header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isMethodNotAllowed()).andExpect(jsonPath("$.code").value("INVALID_KEY"));
    }

    @Test
    void R9_enterWithABlankUserIdIsRejected() throws Exception {
        rejected(fetch("  "), "userId", "User ID can NOT be empty...");
    }

    @Test
    void R10_theRecordIsShownWithThePasswordInClear() throws Exception {
        fetch(ID).andExpect(jsonPath("$.user.firstName").value("First3"))
                .andExpect(jsonPath("$.user.lastName").value("Last3"))
                .andExpect(jsonPath("$.user.password").value("PASSWORD"))
                .andExpect(jsonPath("$.user.userType").value("U"))
                .andExpect(jsonPath("$.user.version").value(0));
    }

    @Test
    void R11_pf5WithABlankUserIdIsRejected() throws Exception {
        rejected(update(" ", updateForm(ID)), "userId", "User ID can NOT be empty...");
    }

    @Test
    void R12_firstNameIsRequired() throws Exception {
        rejected(update(ID, updateForm(ID).put("firstName", "").put("lastName", "")), "firstName",
                "First Name can NOT be empty...");
    }

    @Test
    void R13_lastNameIsRequired() throws Exception {
        rejected(update(ID, updateForm(ID).put("lastName", " ").put("password", "")), "lastName",
                "Last Name can NOT be empty...");
    }

    @Test
    void R14_passwordIsRequired() throws Exception {
        rejected(update(ID, updateForm(ID).put("password", "").put("userType", "")), "password",
                "Password can NOT be empty...");
    }

    @Test
    void R15_userTypeIsRequired() throws Exception {
        rejected(update(ID, updateForm(ID).put("userType", "")), "userType", "User Type can NOT be empty...");
        rejected(update(ID, updateForm(ID).put("userType", "Z")), "userType",
                "User Type must be A (Admin) or U (User)...");
    }

    @Test
    void R16_nothingChangedAsksToModify() throws Exception {
        update(ID, updateForm(ID).put("firstName", "First3   ").put("userType", "u")).andExpect(status().isOk())
                .andExpect(jsonPath("$.state").value("SHOW"))
                .andExpect(jsonPath("$.message").value("Please modify to update ..."));
        verify(users, never()).saveAndFlush(any(UserSecurity.class));
        assertThat(store.get(ID).getVersion()).isZero();
    }

    @Test
    void R17_aSuccessfulReadAsksForPf5() throws Exception {
        fetch(ID).andExpect(jsonPath("$.message").value("Press PF5 key to save your updates ..."));
    }

    @Test
    void R18_anUnknownUserIsNotFound() throws Exception {
        fetch("NOBODY").andExpect(status().isNotFound()).andExpect(jsonPath("$.code").value("NOTFND"))
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
    }

    @Test
    void R19_otherReadErrorsCannotLookUp() throws Exception {
        given(users.findById(anyString())).willThrow(new DataAccessResourceFailureException("down"));
        fetch(ID).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to lookup User...")));
    }

    @Test
    void R20_aRewriteKeepsTheFieldsWithAGreenMessage() throws Exception {
        update(ID, updateForm(ID).put("password", "NEWPASS1").put("userType", "A")).andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value("User USER0003 has been updated ..."))
                .andExpect(jsonPath("$.user.password").value("NEWPASS1"))
                .andExpect(jsonPath("$.user.userType").value("A"))
                .andExpect(jsonPath("$.user.version").value(1));
        assertThat(store.get(ID).getPassword()).isEqualTo("NEWPASS1");
        assertThat(store.get(ID).getUsrType()).isEqualTo(UserType.ADMIN);
    }

    @Test
    void R21_pf5OnAnUnknownUserIsNotFound() throws Exception {
        update("NOBODY", updateForm(ID)).andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
    }

    @Test
    void R22_otherRewriteErrorsCannotUpdate() throws Exception {
        given(users.saveAndFlush(any(UserSecurity.class))).willThrow(new DataAccessResourceFailureException("down"));
        update(ID, updateForm(ID).put("lastName", "Changed")).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to Update User...")));
    }

    @Test
    void R23_returnCarriesTheFromFields() throws Exception {
        fetch(ID).andExpect(jsonPath("$.exit.fromTranId").value("CU02"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COUSR02C"));
    }

    @Test
    void R24_theScreenHeaderNamesCu02() throws Exception {
        fetch(ID).andExpect(jsonPath("$.header.tranId").value("CU02"))
                .andExpect(jsonPath("$.header.programName").value("COUSR02C"));
    }

    @Test
    void aStaleVersionIsChangedAndAMissingVersionIsRejected() throws Exception {
        update(ID, updateForm(ID).put("lastName", "First")).andExpect(status().isOk());
        update(ID, updateForm(ID).put("lastName", "Second").put("version", 0)).andExpect(status().isConflict())
                .andExpect(jsonPath("$.code").value("CHANGED"));
        assertThat(store.get(ID).getLastName()).isEqualTo("First");
        rejected(update(ID, updateForm(ID).putNull("version")), "version",
                com.carddemo.user.admin.UserAdminMessages.MSG_VERSION_REQUIRED);
    }
}
