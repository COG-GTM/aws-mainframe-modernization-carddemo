package com.carddemo.user.signon;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import com.carddemo.user.UserPasswords;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRecord;
import com.carddemo.user.UserSecurityRepository;
import com.carddemo.user.UserType;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.dao.DataAccessResourceFailureException;

/** ADR-0023: plain-text compare until the first sign-on stores the hash, then the hash accepts exactly the same input. */
class PasswordHashSignOnTest {

    private final UserPasswords passwords = new UserPasswords();
    private final UserSecurityRepository repo = mock(UserSecurityRepository.class);
    private final SignOnService service = new SignOnService(repo, passwords);

    private static final List<String> INPUTS = List.of("pass", "PASS", "pass    ", "Pass", " pass", "passx", "pas",
            "PASS    X", "PASSWORD", "");

    private UserSecurity user(String password, String hash) {
        UserSecurity u = UserSecurity.from(new UserSecurityRecord("USER0001", "A", "B", password, UserType.USER));
        u.setPasswordHash(hash);
        given(repo.findById("USER0001")).willReturn(Optional.of(u));
        return u;
    }

    @Test
    void firstPlainTextSignOnStoresAHashOfTheStoredField() {
        user("PASS", null);
        assertThat(service.signOn("USER0001", "pass")).isInstanceOf(SignOnResult.SignedOn.class);
        ArgumentCaptor<String> hash = ArgumentCaptor.forClass(String.class);
        verify(repo).storePasswordHash(eq("USER0001"), eq("PASS"), hash.capture());
        assertThat(hash.getValue()).startsWith("{bcrypt}").doesNotContain("PASS");
        UserSecurity hashed = UserSecurity.from(new UserSecurityRecord("USER0001", "A", "B", "PASS", UserType.USER));
        hashed.setPasswordHash(hash.getValue());
        assertThat(passwords.matches(hashed, "PASS")).isTrue();
    }

    @Test
    void aRejectedSignOnStoresNothing() {
        user("PASS", null);
        assertThat(service.signOn("USER0001", "wrong")).isInstanceOf(SignOnResult.Rejected.class);
        verify(repo, never()).storePasswordHash(anyString(), anyString(), anyString());
    }

    @Test
    void theHashAcceptsExactlyTheInputsThePlainTextCompareAccepted() {
        for (String input : INPUTS) {
            user("PASS", null);
            boolean plain = service.signOn("USER0001", input) instanceof SignOnResult.SignedOn;
            user("PASS", passwords.hash("PASS"));
            boolean hashed = service.signOn("USER0001", input) instanceof SignOnResult.SignedOn;
            assertThat(hashed).as("input [%s]", input).isEqualTo(plain);
        }
    }

    @Test
    void aHashedUserIsVerifiedAgainstTheHashAndNotRehashed() {
        user("PASS", passwords.hash("PASS"));
        assertThat(service.signOn("USER0001", "pass")).isInstanceOf(SignOnResult.SignedOn.class);
        assertThat(service.signOn("USER0001", "other")).isInstanceOf(SignOnResult.Rejected.class);
        verify(repo, never()).storePasswordHash(anyString(), anyString(), anyString());
    }

    @Test
    void aFailedHashUpgradeDoesNotFailTheSignOn() {
        user("PASS", null);
        given(repo.storePasswordHash(anyString(), anyString(), anyString()))
                .willThrow(new DataAccessResourceFailureException("down"));
        assertThat(service.signOn("USER0001", "PASS")).isInstanceOf(SignOnResult.SignedOn.class);
    }
}
