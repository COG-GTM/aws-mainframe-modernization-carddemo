package com.carddemo;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;

class UserAdminIntegrationTest extends IntegrationTestBase {

    private static Map<String, Object> newUser(String id) {
        Map<String, Object> body = new HashMap<>();
        body.put("userId", id);
        body.put("firstName", "JANE");
        body.put("lastName", "DOE");
        body.put("password", "secret1");
        body.put("userType", "U");
        return body;
    }

    @Test
    void userAdminIsAdminOnly() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/users")))
                .andExpect(status().isForbidden())
                .andExpect(jsonPath("$.errorCode").value("FORBIDDEN"));
    }

    @Test
    void listPagesUsers() throws Exception {
        mvc.perform(as(adminToken(), get("/api/v1/users").param("pageSize", "3")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items.length()").value(3))
                .andExpect(jsonPath("$.items[0].userId").value("ADMIN001"))
                .andExpect(jsonPath("$.hasNext").value(true));
        mvc.perform(as(adminToken(), get("/api/v1/users").param("startKey", "USER0003")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items.length()").value(2))
                .andExpect(jsonPath("$.items[0].userId").value("USER0004"));
    }

    @Test
    void getUser() throws Exception {
        mvc.perform(as(adminToken(), get("/api/v1/users/user0002")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.firstName").value("AJITH"))
                .andExpect(jsonPath("$.userType").value("U"));
        mvc.perform(as(adminToken(), get("/api/v1/users/NOBODY")))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
    }

    @Test
    void createUserThenSignOnWithUppercasedPassword() throws Exception {
        String admin = adminToken();
        mvc.perform(as(admin, withJson(post("/api/v1/users"), newUser("newusr1"))))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.userId").value("NEWUSR1"))
                .andExpect(jsonPath("$.message").value("User NEWUSR1 has been added ..."));
        token("NEWUSR1", "SECRET1");
        mvc.perform(as(admin, withJson(post("/api/v1/users"), newUser("NEWUSR1"))))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.errorCode").value("DUPLICATE"))
                .andExpect(jsonPath("$.message").value("User ID already exist..."));
    }

    @Test
    void createValidatesRequiredFields() throws Exception {
        Map<String, Object> body = newUser("X1");
        body.put("firstName", "");
        body.put("userType", "Z");
        mvc.perform(as(adminToken(), withJson(post("/api/v1/users"), body)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.legacyProgram").value("COUSR01C"))
                .andExpect(jsonPath("$.message").value("First Name can NOT be empty..."));
    }

    @Test
    void updateUser() throws Exception {
        String admin = adminToken();
        Map<String, Object> body = new HashMap<>();
        body.put("firstName", "LARRY");
        body.put("lastName", "THOMAS");
        body.put("userType", "A");
        body.put("version", 0);
        mvc.perform(as(admin, withJson(put("/api/v1/users/USER0001"), body)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.firstName").value("LARRY"))
                .andExpect(jsonPath("$.userType").value("A"))
                .andExpect(jsonPath("$.version").value(1))
                .andExpect(jsonPath("$.message").value("User USER0001 has been updated ..."));
        body.put("version", 1);
        mvc.perform(as(admin, withJson(put("/api/v1/users/USER0001"), body)))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("Please modify to update ..."));
        body.put("firstName", "LAWRENCE");
        body.put("version", 0);
        mvc.perform(as(admin, withJson(put("/api/v1/users/USER0001"), body)))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.errorCode").value("CONCURRENT_UPDATE"));
    }

    @Test
    void staleVersionConflictsEvenWhenValuesAreUnchanged() throws Exception {
        String admin = adminToken();
        Map<String, Object> body = new HashMap<>();
        body.put("firstName", "LARRY");
        body.put("lastName", "THOMAS");
        body.put("userType", "A");
        body.put("version", 0);
        mvc.perform(as(admin, withJson(put("/api/v1/users/USER0001"), body))).andExpect(status().isOk());
        mvc.perform(as(admin, withJson(put("/api/v1/users/USER0001"), body)))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.errorCode").value("CONCURRENT_UPDATE"));
    }

    @Test
    void demotedOrDeletedUsersLoseAccessImmediately() throws Exception {
        String admin = adminToken();
        String demoted = token("ADMIN002", "PASSWORD");
        String deleted = token("USER0005", "PASSWORD");
        mvc.perform(as(demoted, get("/api/v1/users"))).andExpect(status().isOk());
        var current = body(mvc.perform(as(admin, get("/api/v1/users/ADMIN002"))).andReturn());
        Map<String, Object> body = new HashMap<>();
        body.put("firstName", current.get("firstName").asText());
        body.put("lastName", current.get("lastName").asText());
        body.put("userType", "U");
        body.put("version", current.get("version").asLong());
        mvc.perform(as(admin, withJson(put("/api/v1/users/ADMIN002"), body))).andExpect(status().isOk());
        mvc.perform(as(demoted, get("/api/v1/users"))).andExpect(status().isForbidden());
        mvc.perform(as(demoted, get("/api/v1/menus/main"))).andExpect(status().isOk());

        mvc.perform(as(admin, delete("/api/v1/users/USER0005"))).andExpect(status().isNoContent());
        mvc.perform(as(deleted, get("/api/v1/menus/main")))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.errorCode").value("UNAUTHENTICATED"));
    }

    @Test
    void deleteUser() throws Exception {
        String admin = adminToken();
        mvc.perform(as(admin, delete("/api/v1/users/USER0005"))).andExpect(status().isNoContent());
        mvc.perform(as(admin, delete("/api/v1/users/USER0005")))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.legacyProgram").value("COUSR03C"));
    }
}
