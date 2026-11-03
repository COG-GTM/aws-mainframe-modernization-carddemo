package com.carddemo.auth.flow;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.testsupport.flow.CobolFlow;
import com.carddemo.testsupport.flow.ScreenKey;
import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

/**
 * The admin menu's user functions on the sample USRSEC file, signed on as ADMIN001: CU00 list
 * (COUSR00C, 10 rows, PF7/PF8), CU01 add (COUSR01C), CU02 update (COUSR02C, ENTER fetches, PF5 saves)
 * and CU03 delete (COUSR03C, ENTER fetches, PF5 deletes). Tests that change the file use their own ids.
 */
class UserAdministrationFlowTest extends AuthFlowTestSupport {

    private String admin;

    @BeforeEach
    void signOnAsAdmin() throws Exception {
        admin = signOn("ADMIN001", "PASSWORD");
    }

    private ResultActions call(MockHttpServletRequestBuilder request) throws Exception {
        return mockMvc.perform(request.header(HttpHeaders.AUTHORIZATION, bearer(admin)));
    }

    private ResultActions add(Map<String, String> user) throws Exception {
        return call(post("/api/v1/users").contentType(MediaType.APPLICATION_JSON).content(JSON.writeValueAsString(user)));
    }

    private ResultActions update(String userId, Map<String, String> user) throws Exception {
        return call(put("/api/v1/users/" + userId).contentType(MediaType.APPLICATION_JSON)
                .content(JSON.writeValueAsString(user)));
    }

    private static Map<String, String> user(String userId, String first, String last, String password, String type) {
        Map<String, String> user = new LinkedHashMap<>();
        user.put("firstName", first);
        user.put("lastName", last);
        user.put("userId", userId);
        user.put("password", password);
        user.put("userType", type);
        return user;
    }

    @Test
    @CobolFlow(program = "COUSR00C", paragraph = "PROCESS-PAGE-FORWARD", key = ScreenKey.ENTER)
    void COUSR00C_PROCESS_PAGE_FORWARD_firstScreenListsTenUsersInIdOrder() throws Exception {
        call(get("/api/v1/users"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.size").value(10))
                .andExpect(jsonPath("$.content.length()").value(10))
                .andExpect(jsonPath("$.content[0].userId").value("ADMIN001"))
                .andExpect(jsonPath("$.content[0].firstName").value("MARGARET"))
                .andExpect(jsonPath("$.content[0].userType").value("A"))
                .andExpect(jsonPath("$.content[5].userId").value("USER0001"))
                .andExpect(jsonPath("$.content[5].userType").value("U"))
                .andExpect(jsonPath("$.content[0].password").doesNotExist());
    }

    @Test
    @CobolFlow(program = "COUSR00C", paragraph = "STARTBR-USER-SEC-FILE", key = ScreenKey.ENTER)
    void COUSR00C_STARTBR_USER_SEC_FILE_searchUserIdStartsTheBrowse() throws Exception {
        call(get("/api/v1/users").param("startUserId", "USER0003"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].userId").value("USER0003"))
                .andExpect(jsonPath("$.content[2].userId").value("USER0005"));
    }

    /** Eleven users after an add: PF8 shows the eleventh alone, PF7 the first ten again. */
    @Test
    @CobolFlow(program = "COUSR00C", paragraph = "PROCESS-PF8-KEY", key = ScreenKey.PF8)
    void COUSR00C_PROCESS_PF8_KEY_nextScreenShowsTheRemainingUsers() throws Exception {
        add(user("ZPAGE001", "Paging", "User", "PASSWORD", "U")).andExpect(status().isCreated());
        call(get("/api/v1/users").param("page", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content.length()").value(1))
                .andExpect(jsonPath("$.content[0].userId").value("ZPAGE001"))
                .andExpect(jsonPath("$.totalPages").value(2));
        call(get("/api/v1/users").param("page", "2"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content.length()").value(0));
        call(delete("/api/v1/users/ZPAGE001")).andExpect(status().isNoContent());
    }

    @Test
    @CobolFlow(program = "COUSR00C", paragraph = "PROCESS-PF7-KEY", key = ScreenKey.PF7)
    void COUSR00C_PROCESS_PF7_KEY_previousScreenAndNothingBeforeTheTop() throws Exception {
        call(get("/api/v1/users").param("page", "0"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].userId").value("ADMIN001"));
        // "You are already at the top of the page..." has no error equivalent (api-style.md §4).
        call(get("/api/v1/users").param("page", "-1"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("page"));
    }

    @Test
    @CobolFlow(program = "COUSR01C", paragraph = "PROCESS-ENTER-KEY", key = ScreenKey.ENTER)
    void COUSR01C_PROCESS_ENTER_KEY_eachEmptyFieldInScreenOrder() throws Exception {
        String[][] cases = {
                {"firstName", "First Name can NOT be empty..."},
                {"lastName", "Last Name can NOT be empty..."},
                {"userId", "User ID can NOT be empty..."},
                {"password", "Password can NOT be empty..."},
                {"userType", "User Type can NOT be empty..."}};
        for (String[] c : cases) {
            Map<String, String> request = user("ZEMPTY01", "Empty", "Fields", "PASSWORD", "U");
            request.put(c[0], " ");
            add(request).andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.fieldErrors[0].field").value(c[0]))
                    .andExpect(jsonPath("$.fieldErrors[0].message").value(c[1]));
        }
        call(get("/api/v1/users/ZEMPTY01")).andExpect(status().isNotFound());
    }

    @Test
    @CobolFlow(program = "COUSR01C", paragraph = "WRITE-USER-SEC-FILE", key = ScreenKey.ENTER)
    void COUSR01C_WRITE_USER_SEC_FILE_duplicateUserIdIsRefused() throws Exception {
        add(user("USER0001", "Dup", "User", "PASSWORD", "U"))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.message").value("User ID already exist..."));
    }

    @Test
    @CobolFlow(program = "COUSR01C", paragraph = "WRITE-USER-SEC-FILE", key = ScreenKey.ENTER)
    void COUSR01C_WRITE_USER_SEC_FILE_addedUserCanSignOn() throws Exception {
        add(user("ZNEW0001", "Grace", "Hopper", "NEWPASS1", "U"))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.userId").value("ZNEW0001"))
                .andExpect(jsonPath("$.userType").value("U"))
                .andExpect(jsonPath("$.password").doesNotExist());
        call(get("/api/v1/users/ZNEW0001")).andExpect(status().isOk())
                .andExpect(jsonPath("$.firstName").value("Grace"))
                .andExpect(jsonPath("$.lastName").value("Hopper"));
        signOn("ZNEW0001", "NEWPASS1");
        call(delete("/api/v1/users/ZNEW0001")).andExpect(status().isNoContent());
    }

    @Test
    @CobolFlow(program = "COUSR02C", paragraph = "READ-USER-SEC-FILE", key = ScreenKey.ENTER)
    void COUSR02C_READ_USER_SEC_FILE_enterFetchesTheUser() throws Exception {
        call(get("/api/v1/users/USER0002"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.userId").value("USER0002"))
                .andExpect(jsonPath("$.firstName").value("AJITH"))
                .andExpect(jsonPath("$.userType").value("U"));
        call(get("/api/v1/users/NOSUCHID"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
    }

    @Test
    @CobolFlow(program = "COUSR02C", paragraph = "UPDATE-USER-INFO", key = ScreenKey.PF5)
    void COUSR02C_UPDATE_USER_INFO_eachEmptyFieldInScreenOrder() throws Exception {
        String[][] cases = {
                {"firstName", "First Name can NOT be empty..."},
                {"lastName", "Last Name can NOT be empty..."},
                {"password", "Password can NOT be empty..."},
                {"userType", "User Type can NOT be empty..."}};
        for (String[] c : cases) {
            Map<String, String> request = user("USER0004", "AVERARDO", "MAZZI", "PASSWORD", "U");
            request.put(c[0], "");
            update("USER0004", request).andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.fieldErrors[0].field").value(c[0]))
                    .andExpect(jsonPath("$.fieldErrors[0].message").value(c[1]));
        }
    }

    @Test
    @CobolFlow(program = "COUSR02C", paragraph = "UPDATE-USER-INFO", key = ScreenKey.PF5)
    void COUSR02C_UPDATE_USER_INFO_unchangedUserAsksForAModification() throws Exception {
        String last = JSON.readTree(call(get("/api/v1/users/USER0005")).andReturn().getResponse().getContentAsString())
                .get("lastName").asText();
        update("USER0005", user("USER0005", "LEE", last, "PASSWORD", "U"))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("Please modify to update ..."));
    }

    @Test
    @CobolFlow(program = "COUSR02C", paragraph = "UPDATE-USER-SEC-FILE", key = ScreenKey.PF5)
    void COUSR02C_UPDATE_USER_SEC_FILE_changedFieldsAreRewritten() throws Exception {
        add(user("ZUPD0001", "Before", "Update", "PASSWORD", "U")).andExpect(status().isCreated());
        update("ZUPD0001", user("ZUPD0001", "After", "Update", "PASSWORD", "A"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.firstName").value("After"))
                .andExpect(jsonPath("$.userType").value("A"));
        call(get("/api/v1/users/ZUPD0001")).andExpect(jsonPath("$.firstName").value("After"));
        update("NOSUCHID", user("NOSUCHID", "No", "Body", "PASSWORD", "U"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
        call(delete("/api/v1/users/ZUPD0001")).andExpect(status().isNoContent());
    }

    @Test
    @CobolFlow(program = "COUSR03C", paragraph = "READ-USER-SEC-FILE", key = ScreenKey.ENTER)
    void COUSR03C_READ_USER_SEC_FILE_enterFetchesTheUserToDelete() throws Exception {
        call(get("/api/v1/users/ADMIN005"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.firstName").value("GRANVILLE"))
                .andExpect(jsonPath("$.userType").value("A"));
    }

    @Test
    @CobolFlow(program = "COUSR03C", paragraph = "DELETE-USER-SEC-FILE", key = ScreenKey.PF5)
    void COUSR03C_DELETE_USER_SEC_FILE_deletedUserIsGoneAndCannotSignOn() throws Exception {
        add(user("ZDEL0001", "Delete", "Me", "PASSWORD", "U")).andExpect(status().isCreated());
        call(delete("/api/v1/users/ZDEL0001")).andExpect(status().isNoContent());
        call(get("/api/v1/users/ZDEL0001"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
        call(delete("/api/v1/users/ZDEL0001"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
        mockMvc.perform(post("/api/v1/auth/signon").contentType(MediaType.APPLICATION_JSON)
                        .content("{\"userId\":\"ZDEL0001\",\"password\":\"PASSWORD\"}"))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("User not found. Try again ..."));
    }
}
