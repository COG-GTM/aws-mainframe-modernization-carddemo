package com.carddemo.common;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import org.junit.jupiter.api.Test;

class AbendExceptionTest {

    @Test
    void carddemoAbendKeepsCode999LikeCee3abdCallers() {
        IllegalStateException cause = new IllegalStateException("status 23");
        AbendException e = AbendException.carddemo("ERROR READING ACCOUNT FILE", cause);

        assertThat(e).isInstanceOf(RuntimeException.class).hasCause(cause);
        assertThat(e.abendCode()).isEqualTo(999);
        assertThat(e.abendLabel()).isEqualTo("U0999");
        assertThat(e.getMessage()).isEqualTo("USER ABEND U0999: ERROR READING ACCOUNT FILE");
    }

    @Test
    void otherCodesArePaddedToFourDigits() {
        assertThat(new AbendException(4, "x").abendLabel()).isEqualTo("U0004");
        assertThat(new AbendException(4095, null).getMessage()).isEqualTo("USER ABEND U4095: ");
    }

    @Test
    void rejectsCodesOutsideTheUserAbendRange() {
        assertThatIllegalArgumentException().isThrownBy(() -> new AbendException(4096, "x"));
        assertThatIllegalArgumentException().isThrownBy(() -> new AbendException(-1, "x"));
    }
}
