package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserType;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;

/** {@code docs/modernization/rules/COUSR01C.md} against {@code POST /api/v1/users}. */
class UserAddRulesTest extends UserWebTest {

    private static void rejected(ResultActions result, String field, String message) throws Exception {
        result.andExpect(status().isBadRequest()).andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value(field)).andExpect(jsonPath("$.message").value(message));
    }

    @Test
    void R1_noSessionReturnsToSignOn() throws Exception {
        mvc.perform(post(USERS).contentType(MediaType.APPLICATION_JSON).content("{}"))
                .andExpect(status().isUnauthorized()).andExpect(jsonPath("$.toProgram").value("COSGN00C"));
    }

    @Test
    void R2_anEmptyScreenStartsAtTheFirstName() throws Exception {
        rejected(add(json.createObjectNode()), "firstName", "First Name can NOT be empty...");
    }

    @Test
    void R3_enterAddsTheUser() throws Exception {
        add(addForm()).andExpect(status().isCreated()).andExpect(header().string(HttpHeaders.LOCATION,
                "/api/v1/users/JDOE01"));
        assertThat(store).containsKey("JDOE01");
    }

    @Test
    void R4_pf3ReturnsToTheAdminMenu() throws Exception {
        add(addForm()).andExpect(jsonPath("$.exit.toProgram").value("COADM01C"))
                .andExpect(jsonPath("$.exit.toTranId").value("CA00"));
    }

    @Test
    void R5_clearKeepsNothingOnTheServer() throws Exception {
        rejected(add(addForm().put("password", "")), "password", "Password can NOT be empty...");
        assertThat(store).doesNotContainKey("JDOE01");
        add(addForm()).andExpect(status().isCreated());
    }

    @Test
    void R6_otherKeysAreInvalid() throws Exception {
        mvc.perform(patch(USERS).header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isMethodNotAllowed()).andExpect(jsonPath("$.code").value("INVALID_KEY"));
    }

    @Test
    void R7_theScreenStaysOnCu01() throws Exception {
        add(addForm()).andExpect(jsonPath("$.header.tranId").value("CU01"));
    }

    @Test
    void R8_firstNameIsRequired() throws Exception {
        rejected(add(addForm().put("firstName", " ").put("lastName", "")), "firstName",
                "First Name can NOT be empty...");
    }

    @Test
    void R9_lastNameIsRequired() throws Exception {
        rejected(add(addForm().put("lastName", "").put("userId", "")), "lastName", "Last Name can NOT be empty...");
    }

    @Test
    void R10_userIdIsRequired() throws Exception {
        rejected(add(addForm().put("userId", "").put("password", "")), "userId", "User ID can NOT be empty...");
    }

    @Test
    void R11_passwordIsRequired() throws Exception {
        rejected(add(addForm().put("password", "").put("userType", "")), "password", "Password can NOT be empty...");
    }

    @Test
    void R12_userTypeIsRequired() throws Exception {
        rejected(add(addForm().put("userType", "")), "userType", "User Type can NOT be empty...");
    }

    @Test
    void R13_theRecordIsBuiltFromTheTypedFields() throws Exception {
        add(addForm().put("firstName", "Jane  ").put("password", "Mixed1").put("userType", "a"))
                .andExpect(status().isCreated());
        UserSecurity stored = store.get("JDOE01");
        assertThat(stored.getFirstName()).isEqualTo("Jane");
        assertThat(stored.getLastName()).isEqualTo("Doe");
        assertThat(stored.getPassword()).isEqualTo("Mixed1");
        assertThat(stored.getUsrType()).isEqualTo(UserType.ADMIN);
        rejected(add(addForm().put("userId", "JDOE02").put("userType", "X")), "userType",
                "User Type must be A (Admin) or U (User)...");
        rejected(add(addForm().put("userId", "TOOLONGID")), "userId", "User ID can be at most 8 characters...");
    }

    @Test
    void R14_returnCarriesTheFromFields() throws Exception {
        add(addForm()).andExpect(jsonPath("$.exit.fromTranId").value("CU01"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COUSR01C"));
    }

    @Test
    void R15_successClearsTheScreenWithAGreenMessage() throws Exception {
        add(addForm()).andExpect(status().isCreated()).andExpect(jsonPath("$.state").value("ADDED"))
                .andExpect(jsonPath("$.message").value("User JDOE01 has been added ..."))
                .andExpect(jsonPath("$.user.userId").value("JDOE01"))
                .andExpect(jsonPath("$.user.password").doesNotExist());
    }

    @Test
    void R16_aDuplicateIdAlreadyExists() throws Exception {
        ObjectNode duplicate = addForm().put("userId", userId(1));
        add(duplicate).andExpect(status().isConflict()).andExpect(jsonPath("$.code").value("DUPREC"))
                .andExpect(jsonPath("$.message").value("User ID already exist..."));
        assertThat(store.get(userId(1)).getFirstName()).isEqualTo("First1");
    }

    @Test
    void R17_otherWriteErrorsCannotAdd() throws Exception {
        given(users.saveAndFlush(any(UserSecurity.class))).willThrow(new DataAccessResourceFailureException("down"));
        add(addForm()).andExpect(status().isInternalServerError()).andExpect(jsonPath("$.code").value("ABEND"))
                .andExpect(jsonPath("$.message", endsWith("Unable to Add User...")));
    }

    @Test
    void R18_theScreenHeaderNamesCu01() throws Exception {
        add(addForm()).andExpect(jsonPath("$.header.tranId").value("CU01"))
                .andExpect(jsonPath("$.header.programName").value("COUSR01C"));
    }
}
