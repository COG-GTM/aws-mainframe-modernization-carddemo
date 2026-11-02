package com.carddemo.auth.api;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.auth.domain.User;
import com.carddemo.auth.repository.UserRepository;
import com.carddemo.testsupport.AdminCaller;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.web.servlet.MockMvc;

/** COUSR00C-COUSR03C parity: 10-row browse, COBOL field order and messages, read-for-update paths. */
@SpringBootTest
@AutoConfigureMockMvc
@Import(AdminCaller.class)
class UserAdministrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private UserRepository users;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @BeforeEach
    void setUp() {
        users.deleteAll();
        for (int i = 1; i <= 12; i++) {
            User user = new User(String.format("USER%04d", i), passwordEncoder.encode("PASSWORD"), "U");
            user.setFirstName("First" + i);
            user.setLastName("Last" + i);
            users.save(user);
        }
    }

    @Test
    void listsTenUsersPerPageInKeyOrderLikeCousr00c() throws Exception {
        mockMvc.perform(get("/api/v1/users"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.size").value(10))
                .andExpect(jsonPath("$.content.length()").value(10))
                .andExpect(jsonPath("$.content[0].userId").value("USER0001"))
                .andExpect(jsonPath("$.content[9].userId").value("USER0010"))
                .andExpect(jsonPath("$.totalElements").value(12))
                .andExpect(jsonPath("$.totalPages").value(2));
        mockMvc.perform(get("/api/v1/users").param("page", "1"))
                .andExpect(jsonPath("$.content.length()").value(2))
                .andExpect(jsonPath("$.content[0].userId").value("USER0011"));
    }

    @Test
    void startsTheBrowseAtTheSearchUserId() throws Exception {
        mockMvc.perform(get("/api/v1/users").param("startUserId", "user0005"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].userId").value("USER0005"))
                .andExpect(jsonPath("$.totalElements").value(8));
    }

    @ParameterizedTest(name = "{0} missing -> {1}")
    @CsvSource(delimiter = '|', value = {
            "firstName|First Name can NOT be empty...",
            "lastName|Last Name can NOT be empty...",
            "userId|User ID can NOT be empty...",
            "password|Password can NOT be empty...",
            "userType|User Type can NOT be empty..."})
    void addChecksTheFieldsInCousr01cOrder(String blankField, String message) throws Exception {
        // Every field from blankField on is blank, so the answer names the first one COUSR01C checks.
        String[] order = {"firstName", "lastName", "userId", "password", "userType"};
        String[] values = {"New", "User", "NEWUSER1", "PASSWORD", "U"};
        StringBuilder json = new StringBuilder("{");
        boolean blank = false;
        for (int i = 0; i < order.length; i++) {
            blank |= order[i].equals(blankField);
            json.append(i == 0 ? "" : ",").append('"').append(order[i]).append("\":\"")
                    .append(blank ? "  " : values[i]).append('"');
        }
        mockMvc.perform(post("/api/v1/users").contentType(MediaType.APPLICATION_JSON).content(json + "}"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value(message))
                .andExpect(jsonPath("$.fieldErrors[0].field").value(blankField));
        assertThat(users.existsById("NEWUSER1")).isFalse();
    }

    @Test
    void acceptsOnlyUserTypesAAndU() throws Exception {
        mockMvc.perform(post("/api/v1/users").contentType(MediaType.APPLICATION_JSON).content("""
                        {"userId":"NEWUSER1","firstName":"New","lastName":"User","password":"PASSWORD","userType":"X"}
                        """))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("userType"));
        mockMvc.perform(post("/api/v1/users").contentType(MediaType.APPLICATION_JSON).content("""
                        {"userId":"newuser1","firstName":"New","lastName":"User","password":"PASSWORD","userType":"A"}
                        """))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.userId").value("NEWUSER1"))
                .andExpect(jsonPath("$.userType").value("A"));
    }

    @Test
    void updateRequiresAChangeLikeCousr02c() throws Exception {
        mockMvc.perform(put("/api/v1/users/USER0001").contentType(MediaType.APPLICATION_JSON).content("""
                        {"firstName":"First1","lastName":"Last1","password":"PASSWORD","userType":"U"}
                        """))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("Please modify to update ..."));
        mockMvc.perform(put("/api/v1/users/USER0001").contentType(MediaType.APPLICATION_JSON).content("""
                        {"firstName":"Changed","lastName":"Last1","password":"PASSWORD","userType":"A"}
                        """))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.firstName").value("Changed"))
                .andExpect(jsonPath("$.userType").value("A"));
    }

    @Test
    void updateChecksTheFieldsInCousr02cOrder() throws Exception {
        mockMvc.perform(put("/api/v1/users/USER0001").contentType(MediaType.APPLICATION_JSON).content("""
                        {"firstName":"","lastName":"","password":"PASSWORD","userType":"U"}
                        """))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("First Name can NOT be empty..."));
        mockMvc.perform(put("/api/v1/users/USER0001").contentType(MediaType.APPLICATION_JSON).content("""
                        {"userId":"USER0002","firstName":"A","lastName":"B","password":"PASSWORD","userType":"U"}
                        """))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("userId"));
    }

    @Test
    void unknownUserIdAnswersTheCobolNotFoundText() throws Exception {
        mockMvc.perform(get("/api/v1/users/NOBODY"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
        mockMvc.perform(put("/api/v1/users/NOBODY").contentType(MediaType.APPLICATION_JSON).content("""
                        {"firstName":"A","lastName":"B","password":"PASSWORD","userType":"U"}
                        """))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
        mockMvc.perform(delete("/api/v1/users/NOBODY"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("User ID NOT found..."));
    }

    @Test
    void deletesAUserLikeCousr03c() throws Exception {
        mockMvc.perform(delete("/api/v1/users/USER0012")).andExpect(status().isNoContent());
        assertThat(users.existsById("USER0012")).isFalse();
    }

    /** A password that already signs on (COSGN00C upper-cases it) is not a change and keeps the stored hash. */
    @Test
    void nameOnlyUpdateWithTheLowerCasePasswordKeepsTheStoredHash() throws Exception {
        String before = users.findById("USER0001").orElseThrow().getPasswordHash();
        mockMvc.perform(put("/api/v1/users/USER0001").contentType(MediaType.APPLICATION_JSON).content("""
                        {"firstName":"Renamed","lastName":"Last1","password":"password","userType":"U"}"""))
                .andExpect(status().isOk());
        assertThat(users.findById("USER0001").orElseThrow().getPasswordHash()).isEqualTo(before);
    }
}
