package com.carddemo.user.signon;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;

import com.carddemo.common.online.ScreenInput;
import com.carddemo.user.UserPasswords;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRecord;
import com.carddemo.user.UserSecurityRepository;
import com.carddemo.user.UserType;
import java.util.Optional;
import org.junit.jupiter.api.Test;

/** COBOL edits behind COSGN00C: figurative-constant blanks, ASCII-only upper-casing, PIC X(08) comparison. */
class SignOnServiceTest {

    @Test
    void spacesOrLowValuesButNotAMixOfBoth() {
        assertThat(ScreenInput.isSpacesOrLowValues(null)).isTrue();
        assertThat(ScreenInput.isSpacesOrLowValues("")).isTrue();
        assertThat(ScreenInput.isSpacesOrLowValues("    ")).isTrue();
        assertThat(ScreenInput.isSpacesOrLowValues("\u0000\u0000")).isTrue();
        assertThat(ScreenInput.isSpacesOrLowValues(" \u0000")).isFalse();
        assertThat(ScreenInput.isSpacesOrLowValues(" a ")).isFalse();
    }

    @Test
    void upperCaseChangesOnlyAsciiLetters() {
        assertThat(ScreenInput.upperCase("user0001")).isEqualTo("USER0001");
        assertThat(ScreenInput.upperCase("straße")).isEqualTo("STRAßE");
        assertThat(ScreenInput.upperCase("ıi")).isEqualTo("ıI");
    }

    @Test
    void storedPasswordIsComparedAsAnEightByteSpacePaddedField() {
        assertThat(SignOnService.pic8("PASS")).isEqualTo("PASS    ");
        assertThat(SignOnService.pic8("PASSWORDX")).isEqualTo("PASSWORDX");
        UserSecurityRepository repo = mock(UserSecurityRepository.class);
        given(repo.findById("USER0001")).willReturn(Optional.of(UserSecurity.from(
                new UserSecurityRecord("USER0001", "A", "B", "PASS", UserType.USER))));
        SignOnService service = new SignOnService(repo, new UserPasswords());
        assertThat(service.signOn("USER0001", "pass    ")).isInstanceOf(SignOnResult.SignedOn.class);
        assertThat(service.signOn("USER0001", "pass")).isInstanceOf(SignOnResult.SignedOn.class);
        assertThat(service.signOn("USER0001", " pass")).isInstanceOf(SignOnResult.Rejected.class);
    }

    @Test
    void routingByUserType() {
        assertThat(new SignOnResult.SignedOn("A", UserType.ADMIN, "", "").targetProgram()).isEqualTo("COADM01C");
        assertThat(new SignOnResult.SignedOn("U", UserType.USER, "", "").targetProgram()).isEqualTo("COMEN01C");
    }
}
