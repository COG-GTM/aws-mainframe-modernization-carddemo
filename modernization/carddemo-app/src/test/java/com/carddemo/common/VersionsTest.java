package com.carddemo.common;

import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.springframework.orm.ObjectOptimisticLockingFailureException;

class VersionsTest {

    @Test
    void matchingVersionPasses() {
        assertThatCode(() -> Versions.requireCurrent(Object.class, 123L, 4, 4)).doesNotThrowAnyException();
    }

    @Test
    void staleVersionIsRejectedBeforeAnyChange() {
        assertThatThrownBy(() -> Versions.requireCurrent(Object.class, 123L, 4, 5))
                .isInstanceOf(ObjectOptimisticLockingFailureException.class);
    }
}
