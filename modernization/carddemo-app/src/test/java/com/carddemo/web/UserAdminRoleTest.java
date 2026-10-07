package com.carddemo.web;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import static org.mockito.BDDMockito.given;

import com.carddemo.user.UserType;
import java.util.Optional;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataAccessResourceFailureException;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

/** COADM01C gates every user screen: a USER token gets 403 NOTAUTH on each endpoint and on the admin menu. */
class UserAdminRoleTest extends UserWebTest {

    static Stream<MockHttpServletRequestBuilder> adminOnly() {
        return Stream.of(get(USERS), get(USERS).param("after", "USER0009"),
                post(USERS + "/selection").contentType(MediaType.APPLICATION_JSON)
                        .content("{\"rows\":[{\"userId\":\"USER0001\",\"selection\":\"U\"}]}"),
                post(USERS).contentType(MediaType.APPLICATION_JSON).content("{}"),
                get(USERS + "/USER0001"),
                put(USERS + "/USER0001").contentType(MediaType.APPLICATION_JSON).content("{}"),
                delete(USERS + "/USER0001"), delete(USERS + "/USER0001").param("confirm", "Y").param("version", "0"),
                get(MENU + "/admin"),
                post(MENU + "/admin/selection").contentType(MediaType.APPLICATION_JSON).content("{\"option\":\"1\"}"));
    }

    @ParameterizedTest
    @MethodSource("adminOnly")
    void aUserTokenIsRefused(MockHttpServletRequestBuilder request) throws Exception {
        mvc.perform(request.header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isForbidden()).andExpect(jsonPath("$.code").value("NOTAUTH"))
                .andExpect(jsonPath("$.message").value("No access - Admin Only option..."));
    }

    @ParameterizedTest
    @MethodSource("adminOnly")
    void anAdminTokenOfADemotedUserIsRefusedAtOnce(MockHttpServletRequestBuilder request) throws Exception {
        String token = admin();
        given(users.findUsrTypeByUsrId(ADMIN)).willReturn(Optional.of(UserType.USER));
        mvc.perform(request.header(HttpHeaders.AUTHORIZATION, token))
                .andExpect(status().isForbidden()).andExpect(jsonPath("$.code").value("NOTAUTH"))
                .andExpect(jsonPath("$.message").value("No access - Admin Only option..."));
    }

    @Test
    void anAdminTokenOfADeletedUserIsRefused() throws Exception {
        String token = admin();
        given(users.findUsrTypeByUsrId(ADMIN)).willReturn(Optional.empty());
        mvc.perform(get(USERS).header(HttpHeaders.AUTHORIZATION, token))
                .andExpect(status().isForbidden()).andExpect(jsonPath("$.code").value("NOTAUTH"));
    }

    @Test
    void theAdminCheckDeniesWhenUsrsecCannotBeRead() throws Exception {
        String token = admin();
        given(users.findUsrTypeByUsrId(ADMIN)).willThrow(new DataAccessResourceFailureException("down"));
        mvc.perform(get(MENU + "/admin").header(HttpHeaders.AUTHORIZATION, token))
                .andExpect(status().isForbidden()).andExpect(jsonPath("$.code").value("NOTAUTH"));
    }

    @Test
    void aStillAdministratorPasses() throws Exception {
        mvc.perform(get(MENU + "/admin").header(HttpHeaders.AUTHORIZATION, admin())).andExpect(status().isOk());
    }

    @Test
    void noTokenOnAnAdminPathIsStillSignOnRequired() throws Exception {
        mvc.perform(get(USERS)).andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.code").value("SIGNON_REQUIRED"));
    }
}
