package com.carddemo.identity;

import static org.hamcrest.Matchers.hasSize;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.common.testing.ParityTestSupport;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

/** Parity with COSGN00C, COMEN01C/COADM01C and COUSR00C-03C. */
@SpringBootTest
@AutoConfigureMockMvc
class IdentityParityTest extends ParityTestSupport {

    @Autowired
    MockMvc mvc;

    private org.springframework.test.web.servlet.ResultActions signOn(String body) throws Exception {
        return mvc.perform(post("/api/v1/auth/signon").contentType(MediaType.APPLICATION_JSON).content(body));
    }

    @Test
    void cosgn00cAdminRoutesToAdminMenu() throws Exception {
        signOn("{\"userId\":\"admin001\",\"password\":\"password\"}")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.userType").value("A"))
                .andExpect(jsonPath("$.nextProgram").value("COADM01C"))
                .andExpect(jsonPath("$.menu", hasSize(6)))
                .andExpect(jsonPath("$.accessToken").isNotEmpty());
    }

    @Test
    void cosgn00cRegularUserRoutesToMainMenu() throws Exception {
        signOn("{\"userId\":\"USER0001\",\"password\":\"PASSWORD\"}")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.nextProgram").value("COMEN01C"))
                .andExpect(jsonPath("$.menu", hasSize(11)))
                .andExpect(jsonPath("$.menu[9].legacyProgram").value("COBIL00C"));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "{\"userId\":\"\",\"password\":\"PASSWORD\"}|400|Please enter User ID ...",
            "{\"userId\":\"USER0001\",\"password\":\" \"}|400|Please enter Password ...",
            "{\"userId\":\"NOBODY\",\"password\":\"PASSWORD\"}|401|User not found. Try again ...",
            "{\"userId\":\"USER0001\",\"password\":\"WRONG\"}|401|Wrong Password. Try again ..."
    })
    void cosgn00cErrorMessages(String body, int status, String message) throws Exception {
        signOn(body).andExpect(status().is(status)).andExpect(jsonPath("$.message").value(message));
    }

    @Test
    void protectedEndpointsRequireToken() throws Exception {
        mvc.perform(get("/api/v1/menu")).andExpect(status().isUnauthorized());
    }

    @Test
    void adminOptionsAreAdminOnly() throws Exception {
        mvc.perform(get("/api/v1/users").header("Authorization", userToken()))
                .andExpect(status().isForbidden())
                .andExpect(jsonPath("$.message").value("No access - Admin Only option... "));
    }

    @Test
    void cousr00cListsTenPerPageInKeyOrder() throws Exception {
        for (int i = 3; i <= 12; i++) {
            mvc.perform(post("/api/v1/users").header("Authorization", adminToken())
                    .contentType(MediaType.APPLICATION_JSON)
                    .content(String.format("{\"userId\":\"USER%04d\",\"firstName\":\"F\",\"lastName\":\"L\","
                            + "\"password\":\"PASSWORD\",\"userType\":\"U\"}", i)))
                    .andExpect(status().isCreated());
        }
        mvc.perform(get("/api/v1/users").header("Authorization", adminToken()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items", hasSize(10)))
                .andExpect(jsonPath("$.items[0].userId").value("ADMIN001"))
                .andExpect(jsonPath("$.lastKey").value("USER0009"))
                .andExpect(jsonPath("$.hasNextPage").value(true));
        mvc.perform(get("/api/v1/users?after=USER0009&page=1").header("Authorization", adminToken()))
                .andExpect(jsonPath("$.items", hasSize(3)))
                .andExpect(jsonPath("$.pageNumber").value(2))
                .andExpect(jsonPath("$.hasNextPage").value(false));
        mvc.perform(get("/api/v1/users?after=USER0012&page=2").header("Authorization", adminToken()))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("You are already at the bottom of the page..."));
        mvc.perform(get("/api/v1/users?before=ADMIN001").header("Authorization", adminToken()))
                .andExpect(jsonPath("$.message").value("You are already at the top of the page..."));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "{\"userId\":\"NEW1\",\"lastName\":\"L\",\"password\":\"P\",\"userType\":\"U\"}|First Name can NOT be empty...",
            "{\"userId\":\"NEW1\",\"firstName\":\"F\",\"password\":\"P\",\"userType\":\"U\"}|Last Name can NOT be empty...",
            "{\"firstName\":\"F\",\"lastName\":\"L\",\"password\":\"P\",\"userType\":\"U\"}|User ID can NOT be empty...",
            "{\"userId\":\"NEW1\",\"firstName\":\"F\",\"lastName\":\"L\",\"userType\":\"U\"}|Password can NOT be empty...",
            "{\"userId\":\"NEW1\",\"firstName\":\"F\",\"lastName\":\"L\",\"password\":\"P\"}|User Type can NOT be empty..."
    })
    void cousr01cRequiredFieldsInScreenOrder(String body, String message) throws Exception {
        mvc.perform(post("/api/v1/users").header("Authorization", adminToken())
                        .contentType(MediaType.APPLICATION_JSON).content(body))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value(message));
    }

    @Test
    void cousr01cAddDuplicateAndSignOnWithNewUser() throws Exception {
        String body = "{\"userId\":\"NEWUSER1\",\"firstName\":\"Ada\",\"lastName\":\"Lovelace\","
                + "\"password\":\"Secret1\",\"userType\":\"U\"}";
        mvc.perform(post("/api/v1/users").header("Authorization", adminToken())
                        .contentType(MediaType.APPLICATION_JSON).content(body))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.message").value("User NEWUSER1 has been added ..."));
        mvc.perform(post("/api/v1/users").header("Authorization", adminToken())
                        .contentType(MediaType.APPLICATION_JSON).content(body))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.message").value("User ID already exist..."));
        signOn("{\"userId\":\"newuser1\",\"password\":\"secret1\"}").andExpect(status().isOk());
    }

    @Test
    void cousr02cUpdateRequiresAChange() throws Exception {
        String same = "{\"firstName\":\"LAWRENCE\",\"lastName\":\"THOMAS\",\"password\":\"PASSWORD\",\"userType\":\"U\"}";
        mvc.perform(put("/api/v1/users/USER0001").header("Authorization", adminToken())
                        .contentType(MediaType.APPLICATION_JSON).content(same))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Please modify to update ..."));
        String changed = same.replace("THOMAS", "TOMAS");
        mvc.perform(put("/api/v1/users/USER0001").header("Authorization", adminToken())
                        .contentType(MediaType.APPLICATION_JSON).content(changed))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value("User USER0001 has been updated ..."));
        mvc.perform(put("/api/v1/users/NOBODY").header("Authorization", adminToken())
                        .contentType(MediaType.APPLICATION_JSON).content(changed))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
    }

    @Test
    void cousr03cDelete() throws Exception {
        mvc.perform(delete("/api/v1/users/USER0002").header("Authorization", adminToken()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value("User USER0002 has been deleted ..."));
        mvc.perform(get("/api/v1/users/USER0002").header("Authorization", adminToken()))
                .andExpect(status().isNotFound());
    }
}
