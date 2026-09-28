package com.carddemo;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.Map;
import org.junit.jupiter.api.Test;

class AuthAndMenuIntegrationTest extends IntegrationTestBase {

    @Test
    void adminSignonRoutesToAdminMenu() throws Exception {
        mvc.perform(withJson(post("/api/v1/auth/signon"), Map.of("userId", "ADMIN001", "password", "PASSWORD")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.token").isNotEmpty())
                .andExpect(jsonPath("$.tokenType").value("Bearer"))
                .andExpect(jsonPath("$.role").value("ADMIN"))
                .andExpect(jsonPath("$.nextRoute").value("/admin"))
                .andExpect(jsonPath("$.firstName").value("MARGARET"));
    }

    @Test
    void signonUppercasesUserIdAndPasswordLikeCosgn00c() throws Exception {
        mvc.perform(withJson(post("/api/v1/auth/signon"), Map.of("userId", "user0001", "password", "password")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.userId").value("USER0001"))
                .andExpect(jsonPath("$.role").value("USER"))
                .andExpect(jsonPath("$.nextRoute").value("/menu"));
    }

    @Test
    void signonRequiresUserId() throws Exception {
        mvc.perform(withJson(post("/api/v1/auth/signon"), Map.of("userId", " ", "password", "PASSWORD")))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorCode").value("VALIDATION_ERROR"))
                .andExpect(jsonPath("$.message").value("Please enter User ID ..."))
                .andExpect(jsonPath("$.legacyProgram").value("COSGN00C"))
                .andExpect(jsonPath("$.fieldErrors[0].field").value("userId"));
    }

    @Test
    void signonRequiresPassword() throws Exception {
        mvc.perform(withJson(post("/api/v1/auth/signon"), Map.of("userId", "ADMIN001")))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Please enter Password ..."));
    }

    @Test
    void wrongPasswordIsRejected() throws Exception {
        mvc.perform(withJson(post("/api/v1/auth/signon"), Map.of("userId", "ADMIN001", "password", "WRONG")))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.errorCode").value("INVALID_CREDENTIALS"))
                .andExpect(jsonPath("$.message").value("Wrong Password. Try again ..."));
    }

    @Test
    void unknownUserIsRejected() throws Exception {
        mvc.perform(withJson(post("/api/v1/auth/signon"), Map.of("userId", "NOBODY", "password", "PASSWORD")))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.message").value("User not found. Try again ..."));
    }

    @Test
    void protectedEndpointsRequireToken() throws Exception {
        mvc.perform(get("/api/v1/menus/main"))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.errorCode").value("UNAUTHENTICATED"));
    }

    @Test
    void healthIsPublic() throws Exception {
        mvc.perform(get("/actuator/health")).andExpect(status().isOk());
    }

    @Test
    void mainMenuListsComen02yOptions() throws Exception {
        JsonNode menu = body(mvc.perform(as(userToken(), get("/api/v1/menus/main")))
                .andExpect(status().isOk()).andReturn());
        assertThat(menu.get("options")).hasSize(11);
        assertThat(menu.at("/options/0/legacyProgram").asText()).isEqualTo("COACTVWC");
        assertThat(menu.at("/options/9/legacyProgram").asText()).isEqualTo("COBIL00C");
        assertThat(menu.at("/options/10/installed").asBoolean()).isFalse();
    }

    @Test
    void adminMenuIsAdminOnly() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/menus/admin")))
                .andExpect(status().isForbidden())
                .andExpect(jsonPath("$.errorCode").value("FORBIDDEN"));
        mvc.perform(as(adminToken(), get("/api/v1/menus/admin")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.options.length()").value(6))
                .andExpect(jsonPath("$.options[0].legacyProgram").value("COUSR00C"));
    }
}
