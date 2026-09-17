package com.carddemo.auth.api;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.auth.domain.User;
import com.carddemo.auth.repository.UserRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest
@AutoConfigureMockMvc
class AuthControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private UserRepository users;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @BeforeEach
    void setUp() {
        users.deleteAll();
        users.save(new User("ADMIN001", passwordEncoder.encode("Password1"), "A"));
        users.save(new User("USER0001", passwordEncoder.encode("Password1"), "U"));
    }

    @Test
    void routesAnAdminToTheAdminMenuAndAUserToTheMainMenu() throws Exception {
        mockMvc.perform(post("/api/v1/auth/signon")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"userId\": \"ADMIN001\", \"password\": \"Password1\"}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.menu").value("ADMIN"));

        mockMvc.perform(post("/api/v1/auth/signon")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"userId\": \"USER0001\", \"password\": \"Password1\"}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.menu").value("MAIN"));
    }

    @Test
    void rejectsAWrongPassword() throws Exception {
        mockMvc.perform(post("/api/v1/auth/signon")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"userId\": \"ADMIN001\", \"password\": \"nope12345\"}"))
                .andExpect(status().isUnprocessableEntity());
    }

    @Test
    void rejectsADuplicateUserId() throws Exception {
        mockMvc.perform(post("/api/v1/users")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {"userId": "ADMIN001", "password": "Password1", "userType": "A"}
                                """))
                .andExpect(status().isConflict());
    }

    @Test
    void createsAUserWithoutReturningThePassword() throws Exception {
        mockMvc.perform(post("/api/v1/users")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {"userId": "NEWUSR1", "firstName": "New", "lastName": "User",
                                 "password": "Password1", "userType": "U"}
                                """))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.userId").value("NEWUSR1"))
                .andExpect(jsonPath("$.password").doesNotExist())
                .andExpect(jsonPath("$.passwordHash").doesNotExist());
    }
}
