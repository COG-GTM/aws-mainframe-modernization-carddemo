package com.carddemo.runtime.cobol;

import java.util.Optional;

/**
 * Implemented by enums that model a set of level-88 values on one item, e.g.
 * {@code enum UserType implements CobolCode { ADMIN("A"), USER("U") }}.
 */
public interface CobolCode {

    /** The VALUE literal of the condition (unpadded). */
    String code();

    /** Finds the constant whose code matches {@code value} (COBOL comparison: trailing spaces ignored). */
    static <E extends Enum<E> & CobolCode> Optional<E> fromCode(Class<E> type, String value) {
        for (E e : type.getEnumConstants()) {
            if (PicX.equal(e.code(), value)) {
                return Optional.of(e);
            }
        }
        return Optional.empty();
    }
}
