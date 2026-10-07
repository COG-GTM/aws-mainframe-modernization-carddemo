package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.user.UserType;
import com.fasterxml.jackson.databind.JsonNode;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.http.HttpHeaders;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.test.web.servlet.MvcResult;

/** {@code docs/modernization/rules/COSGN00C.md}: one test per rule, R-id in the name. */
class SignOnRulesTest extends OnlineWebTest {

    @Autowired
    JwtDecoder decoder;

    @Test
    void R1_firstEntrySendsAnEmptySignOnScreenWithTheCursorOnUserId() throws Exception {
        mvc.perform(get(LOGIN))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value(""))
                .andExpect(jsonPath("$.cursor").value("userId"))
                .andExpect(jsonPath("$.header.tranId").value("CC00"))
                .andExpect(jsonPath("$.header.programName").value("COSGN00C"));
    }

    @Test
    void R2_enterProcessesTheCredentials() throws Exception {
        givenUser("USER0001", "PASSWORD", UserType.USER);
        mvc.perform(login("USER0001", "PASSWORD")).andExpect(status().isOk());
        verify(users).findById("USER0001");
    }

    @Test
    void R3_pf3SendsTheThankYouTextAndEndsTheConversation() throws Exception {
        mvc.perform(post("/api/v1/auth/logout"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value("Thank you for using CardDemo application..."));
    }

    @Test
    void R4_anyOtherKeyAnswersTheInvalidKeyMessage() throws Exception {
        for (var request : new org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder[] {
            put(LOGIN), delete(LOGIN)}) {
            mvc.perform(request)
                    .andExpect(status().isMethodNotAllowed())
                    .andExpect(jsonPath("$.code").value("INVALID_KEY"))
                    .andExpect(jsonPath("$.field").value(nullValue()))
                    .andExpect(jsonPath("$.message").value("Invalid key pressed. Please see below..."));
        }
    }

    @ParameterizedTest
    @CsvSource(value = {"'',''", "'        ',PASSWORD", "NULL,PASSWORD", "NULL,NULL", "'',''"}, nullValues = "NULL")
    void R5_blankUserIdIsRejectedBeforeThePasswordIsInspected(String userId, String password) throws Exception {
        mvc.perform(login(userId, password))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value("userId"))
                .andExpect(jsonPath("$.message").value("Please enter User ID ..."));
    }

    @Test
    void R5_lowValuesAreBlankToo() throws Exception {
        mvc.perform(login("\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000", "PASSWORD"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Please enter User ID ..."));
    }

    @ParameterizedTest
    @CsvSource(value = {"USER0001,''", "USER0001,'   '", "USER0001,NULL"}, nullValues = "NULL")
    void R6_blankPasswordIsRejected(String userId, String password) throws Exception {
        mvc.perform(login(userId, password))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value("password"))
                .andExpect(jsonPath("$.message").value("Please enter Password ..."));
    }

    @Test
    void R7_userIdAndPasswordAreUpperCasedBeforeTheReadAndTheCompare() throws Exception {
        givenUser("USER0001", "PASSWORD", UserType.USER);
        mvc.perform(login("user0001", "password"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.userId").value("USER0001"));
        verify(users).findById("USER0001");
    }

    @ParameterizedTest
    @CsvSource(value = {"'',PASSWORD", "USER0001,''"})
    void R8_anEditErrorSkipsTheUsrsecRead(String userId, String password) throws Exception {
        mvc.perform(login(userId, password)).andExpect(status().isBadRequest());
        verify(users, never()).findById(anyString());
    }

    @Test
    void R9_matchingCredentialsCarryUserIdAndTypeInTheTokenAndNavigationFromCc00() throws Exception {
        givenUser("USER0001", "PASSWORD", UserType.USER);
        MvcResult result = mvc.perform(login("USER0001", "PASSWORD"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.tokenType").value("Bearer"))
                .andExpect(jsonPath("$.role").value("USER"))
                .andExpect(jsonPath("$.userType").value("U"))
                .andExpect(jsonPath("$.navigation.fromTranId").value("CC00"))
                .andExpect(jsonPath("$.navigation.fromProgram").value("COSGN00C"))
                .andExpect(jsonPath("$.navigation.pgmContext").value("ENTER"))
                .andExpect(jsonPath("$.navigation.acctId").value(nullValue()))
                .andReturn();
        JsonNode body = json.readTree(result.getResponse().getContentAsString());
        Jwt jwt = decoder.decode(body.get("token").asText());
        assertThat(jwt.getSubject()).isEqualTo("USER0001");
        assertThat(jwt.getClaimAsString("role")).isEqualTo("USER");
        assertThat(jwt.getClaimAsString("usrType")).isEqualTo("U");
        assertThat(jwt.getHeaders()).containsEntry("alg", "HS256");
        assertThat(jwt.getExpiresAt()).isAfter(jwt.getIssuedAt());
    }

    @Test
    void R10_anAdministratorIsSentToTheAdminMenu() throws Exception {
        givenUser("ADMIN001", "PASSWORD", UserType.ADMIN);
        MvcResult result = mvc.perform(login("ADMIN001", "PASSWORD"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.role").value("ADMIN"))
                .andExpect(jsonPath("$.targetMenu").value("COADM01C"))
                .andExpect(jsonPath("$.targetMenuUrl").value("/api/v1/menu/admin"))
                .andExpect(jsonPath("$.navigation.toProgram").value("COADM01C"))
                .andExpect(jsonPath("$.navigation.toTranId").value("CA00"))
                .andReturn();
        String token = json.readTree(result.getResponse().getContentAsString()).get("token").asText();
        mvc.perform(get(MENU).header(HttpHeaders.AUTHORIZATION, "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.programId").value("COADM01C"));
    }

    @Test
    void R11_aRegularUserIsSentToTheMainMenu() throws Exception {
        givenUser("USER0001", "PASSWORD", UserType.USER);
        MvcResult result = mvc.perform(login("USER0001", "PASSWORD"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.targetMenu").value("COMEN01C"))
                .andExpect(jsonPath("$.targetMenuUrl").value("/api/v1/menu/main"))
                .andExpect(jsonPath("$.navigation.toProgram").value("COMEN01C"))
                .andExpect(jsonPath("$.navigation.toTranId").value("CM00"))
                .andReturn();
        String token = json.readTree(result.getResponse().getContentAsString()).get("token").asText();
        mvc.perform(get(MENU).header(HttpHeaders.AUTHORIZATION, "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.programId").value("COMEN01C"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"WRONG", "PASSWOR", "PASSWORX", " PASSWOR"})
    void R12_aDifferentPasswordAnswersWrongPassword(String password) throws Exception {
        givenUser("USER0001", "PASSWORD", UserType.USER);
        mvc.perform(login("USER0001", password))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.code").value("WRONG_PASSWORD"))
                .andExpect(jsonPath("$.field").value("password"))
                .andExpect(jsonPath("$.message").value("Wrong Password. Try again ..."))
                .andExpect(jsonPath("$.token").doesNotExist());
    }

    @Test
    void R13_anUnknownUserAnswersUserNotFound() throws Exception {
        mvc.perform(login("NOBODY", "PASSWORD"))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.code").value("NOTFND"))
                .andExpect(jsonPath("$.field").value("userId"))
                .andExpect(jsonPath("$.message").value("User not found. Try again ..."));
    }

    @Test
    void R14_anyOtherReadFailureAnswersUnableToVerify() throws Exception {
        given(users.findById("USER0001")).willThrow(new DataAccessResourceFailureException("USRSEC closed"));
        mvc.perform(login("USER0001", "PASSWORD"))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.code").value("OTHER"))
                .andExpect(jsonPath("$.field").value("userId"))
                .andExpect(jsonPath("$.message").value("Unable to verify the User ..."));
    }

    @Test
    void R15_everySendCarriesTheHeaderFields() throws Exception {
        mvc.perform(get(LOGIN))
                .andExpect(jsonPath("$.header.title01").value("AWS Mainframe Modernization"))
                .andExpect(jsonPath("$.header.title02").value("CardDemo"))
                .andExpect(jsonPath("$.header.tranId").value("CC00"))
                .andExpect(jsonPath("$.header.programName").value("COSGN00C"))
                .andExpect(jsonPath("$.header.currentDate").value("07/06/22"))
                .andExpect(jsonPath("$.header.currentTime").value("13:45:10"))
                .andExpect(jsonPath("$.header.applId").value("CARDDEMO"))
                .andExpect(jsonPath("$.header.sysId").value("CDMO"));
    }

    @Test
    void errorsUseTheUniformBody() throws Exception {
        mvc.perform(login("NOBODY", "PASSWORD"))
                .andExpect(header().string(HttpHeaders.CONTENT_TYPE, "application/problem+json"))
                .andExpect(jsonPath("$.status").value(401))
                .andExpect(jsonPath("$.detail").value("User not found. Try again ..."))
                .andExpect(jsonPath("$.code").exists())
                .andExpect(jsonPath("$.field").exists())
                .andExpect(jsonPath("$.message").exists());
    }

    @Test
    void inputLongerThanTheBmsFieldIsRejected() throws Exception {
        mvc.perform(login("USER00011", "PASSWORD"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value("userId"))
                .andExpect(jsonPath("$.message").value("User ID can be at most 8 characters"));
        verify(users, never()).findById(anyString());
    }

    @Test
    void malformedJsonIsAnInvalidRequest() throws Exception {
        mvc.perform(post(LOGIN).contentType("application/json").content("{"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value(nullValue()));
    }
}
