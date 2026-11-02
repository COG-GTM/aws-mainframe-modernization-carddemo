package com.carddemo.auth.api;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.auth.domain.User;
import com.carddemo.auth.repository.UserRepository;
import com.carddemo.common.security.CallerIdentity;
import com.carddemo.common.security.TokenService;
import com.carddemo.testsupport.AbstractAuthenticationTest;
import com.jayway.jsonpath.JsonPath;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

/** COSGN00C sign-on, the shared token filter, the admin-only user API and COMEN01C/COADM01C. */
@SpringBootTest
@AutoConfigureMockMvc
class AuthenticationAndMenuTest extends AbstractAuthenticationTest {

    @Autowired
    private UserRepository users;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Override
    protected String protectedEndpoint() {
        return "/api/v1/menu";
    }

    @BeforeEach
    void setUp() {
        users.deleteAll();
        // As the data-loader stores USRSEC: BCrypt of the 8-byte, upper-case password.
        users.save(new User("ADMIN001", passwordEncoder.encode("PASSWORD"), "A"));
        users.save(new User("USER0001", passwordEncoder.encode("PASSWORD"), "U"));
    }

    private MockHttpServletRequestBuilder signon(String userId, String password) {
        return post("/api/v1/auth/signon").contentType(MediaType.APPLICATION_JSON)
                .content("{\"userId\": \"" + userId + "\", \"password\": \"" + password + "\"}");
    }

    private String tokenOf(String userId) throws Exception {
        String body = mockMvc.perform(signon(userId, "PASSWORD")).andReturn().getResponse().getContentAsString();
        return "Bearer " + JsonPath.read(body, "$.token");
    }

    @Test
    void signonIssuesATokenCarryingUserIdAndType() throws Exception {
        String body = mockMvc.perform(signon("admin001", "password"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.userId").value("ADMIN001"))
                .andExpect(jsonPath("$.userType").value("A"))
                .andExpect(jsonPath("$.nextScreen").value("ADMIN"))
                .andExpect(jsonPath("$.tokenType").value("Bearer"))
                .andExpect(jsonPath("$.expiresAt").exists())
                .andReturn().getResponse().getContentAsString();
        assertThat(tokens.verify(JsonPath.read(body, "$.token"))).isEqualTo(new CallerIdentity("ADMIN001", "A"));

        body = mockMvc.perform(signon("USER0001", "PASSWORD"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.nextScreen").value("MAIN"))
                .andReturn().getResponse().getContentAsString();
        assertThat(tokens.verify(JsonPath.read(body, "$.token"))).isEqualTo(new CallerIdentity("USER0001", "U"));
    }

    @Test
    void signonFailuresUseTheCobolMessages() throws Exception {
        mockMvc.perform(signon("ADMIN001", "WRONGPWD"))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("Wrong Password. Try again ..."))
                .andExpect(jsonPath("$.token").doesNotExist());
        mockMvc.perform(signon("NOBODY01", "PASSWORD"))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("User not found. Try again ..."));
        mockMvc.perform(signon("", "PASSWORD"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Please enter User ID ..."));
        mockMvc.perform(signon("ADMIN001", ""))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Please enter Password ..."));
    }

    @Test
    void userAdministrationIsAdminOnly() throws Exception {
        mockMvc.perform(get("/api/v1/users").header(HttpHeaders.AUTHORIZATION, tokenOf("USER0001")))
                .andExpect(status().isForbidden())
                .andExpect(jsonPath("$.status").value(403))
                .andExpect(jsonPath("$.error").value("Forbidden"))
                .andExpect(jsonPath("$.message").value("No access - Admin Only option... "));
        mockMvc.perform(post("/api/v1/users").header(HttpHeaders.AUTHORIZATION, tokenOf("USER0001"))
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"userId\": \"HACKER01\", \"password\": \"PASSWORD\", \"userType\": \"A\"}"))
                .andExpect(status().isForbidden());
        assertThat(users.existsById("HACKER01")).isFalse();
        mockMvc.perform(get("/api/v1/users").header(HttpHeaders.AUTHORIZATION, tokenOf("ADMIN001")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(2));
        mockMvc.perform(get("/api/v1/users"))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void mainMenuListsComen02yInCobolOrder() throws Exception {
        mockMvc.perform(get("/api/v1/menu").header(HttpHeaders.AUTHORIZATION, tokenOf("USER0001")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.menu").value("MAIN"))
                .andExpect(jsonPath("$.transactionId").value("CM00"))
                .andExpect(jsonPath("$.program").value("COMEN01C"))
                .andExpect(jsonPath("$.options.length()").value(11))
                .andExpect(jsonPath("$.options[*].name").value(org.hamcrest.Matchers.contains(
                        "Account View", "Account Update", "Credit Card List", "Credit Card View",
                        "Credit Card Update", "Transaction List", "Transaction View", "Transaction Add",
                        "Transaction Reports", "Bill Payment", "Pending Authorization View")))
                .andExpect(jsonPath("$.options[*].program").value(org.hamcrest.Matchers.contains(
                        "COACTVWC", "COACTUPC", "COCRDLIC", "COCRDSLC", "COCRDUPC", "COTRN00C", "COTRN01C",
                        "COTRN02C", "CORPT00C", "COBIL00C", "COPAUS0C")))
                .andExpect(jsonPath("$.options[0].number").value(1))
                .andExpect(jsonPath("$.options[*].adminOnly").value(org.hamcrest.Matchers.everyItem(
                        org.hamcrest.Matchers.is(false))))
                .andExpect(jsonPath("$.options[10].available").value(false));
    }

    @Test
    void adminMenuListsCoadm02yInCobolOrder() throws Exception {
        mockMvc.perform(get("/api/v1/menu").header(HttpHeaders.AUTHORIZATION, tokenOf("ADMIN001")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.menu").value("ADMIN"))
                .andExpect(jsonPath("$.transactionId").value("CA00"))
                .andExpect(jsonPath("$.program").value("COADM01C"))
                .andExpect(jsonPath("$.options[*].name").value(org.hamcrest.Matchers.contains(
                        "User List (Security)", "User Add (Security)", "User Update (Security)",
                        "User Delete (Security)", "Transaction Type List/Update (Db2)",
                        "Transaction Type Maintenance (Db2)")))
                .andExpect(jsonPath("$.options[*].program").value(org.hamcrest.Matchers.contains(
                        "COUSR00C", "COUSR01C", "COUSR02C", "COUSR03C", "COTRTLIC", "COTRTUPC")))
                .andExpect(jsonPath("$.options[*].adminOnly").value(org.hamcrest.Matchers.everyItem(
                        org.hamcrest.Matchers.is(true))))
                .andExpect(jsonPath("$.options[4].available").value(false));
    }

    @Test
    void optionSelectionFollowsComen01cAndCoadm01c() throws Exception {
        String user = tokenOf("USER0001");
        mockMvc.perform(get("/api/v1/menu/options/6").header(HttpHeaders.AUTHORIZATION, user))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.program").value("COTRN00C"))
                .andExpect(jsonPath("$.endpoint").value("GET /api/v1/transactions"));
        mockMvc.perform(get("/api/v1/menu/options/12").header(HttpHeaders.AUTHORIZATION, user))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Please enter a valid option number..."));
        mockMvc.perform(get("/api/v1/menu/options/11").header(HttpHeaders.AUTHORIZATION, user))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("This option Pending Authorization View is not installed..."));
        mockMvc.perform(get("/api/v1/menu/options/5").header(HttpHeaders.AUTHORIZATION, tokenOf("ADMIN001")))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("This option is not installed ..."));
    }
}
