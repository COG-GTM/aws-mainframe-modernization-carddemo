package com.carddemo.user.admin;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;

import com.carddemo.user.UserPasswords;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRecord;
import com.carddemo.user.UserSecurityRepository;
import com.carddemo.user.UserType;
import java.util.Optional;
import org.junit.jupiter.api.Test;

/** COUSR01C/COUSR02C write the 8-byte {@code SEC-USR-PWD} value and its hash together (ADR-0023). */
class UserPasswordWriteTest {

    private final UserPasswords passwords = new UserPasswords();
    private final UserSecurityRepository repo = mock(UserSecurityRepository.class);

    UserPasswordWriteTest() {
        given(repo.saveAndFlush(any(UserSecurity.class))).willAnswer(i -> i.getArgument(0));
        given(repo.lockVersion(anyString())).willReturn(Optional.of(0L));
    }

    @Test
    void addStoresThePlainFieldAndItsHash() {
        UserSecurity added = new UserAddService(repo, passwords)
                .add(new UserForm("ZZPW0001", "First", "Last", "PW01", "U"));
        assertThat(added.getPassword()).isEqualTo("PW01");
        assertThat(added.getPasswordHash()).startsWith("{bcrypt}");
        assertThat(passwords.matches(added, "PW01")).isTrue();
        assertThat(passwords.matches(added, "PW02")).isFalse();
    }

    @Test
    void aPasswordChangeReplacesBothValues() {
        UserSecurity user = stored("OLDPW", passwords.hash("OLDPW"));
        new UserUpdateService(repo, new UserLookup(repo), passwords)
                .update("ZZPW0001", new UserForm("ZZPW0001", "First", "Last", "NEWPW", "U"), 0L);
        assertThat(user.getPassword()).isEqualTo("NEWPW");
        assertThat(passwords.matches(user, "NEWPW")).isTrue();
        assertThat(passwords.matches(user, "OLDPW")).isFalse();
    }

    @Test
    void anUpdateWithoutPasswordChangeKeepsTheHash() {
        String hash = passwords.hash("SAMEPW");
        UserSecurity user = stored("SAMEPW", hash);
        new UserUpdateService(repo, new UserLookup(repo), passwords)
                .update("ZZPW0001", new UserForm("ZZPW0001", "Other", "Last", "SAMEPW", "U"), 0L);
        assertThat(user.getFirstName()).isEqualTo("Other");
        assertThat(user.getPasswordHash()).isEqualTo(hash);
    }

    @Test
    void anUpdateOfANotYetHashedUserHashesIt() {
        UserSecurity user = stored("SAMEPW", null);
        new UserUpdateService(repo, new UserLookup(repo), passwords)
                .update("ZZPW0001", new UserForm("ZZPW0001", "Other", "Last", "SAMEPW", "A"), 0L);
        assertThat(passwords.matches(user, "SAMEPW")).isTrue();
        assertThat(user.getPasswordHash()).isNotNull();
    }

    private UserSecurity stored(String password, String hash) {
        UserSecurity user = UserSecurity.from(new UserSecurityRecord("ZZPW0001", "First", "Last", password,
                UserType.USER));
        user.setPasswordHash(hash);
        given(repo.findById("ZZPW0001")).willReturn(Optional.of(user));
        return user;
    }
}
