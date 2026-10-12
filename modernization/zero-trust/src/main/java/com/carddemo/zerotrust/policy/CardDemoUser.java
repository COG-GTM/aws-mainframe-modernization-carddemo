package com.carddemo.zerotrust.policy;

import java.util.Optional;

/**
 * One user from the security file, reduced to the two fields authorization needs:
 * the user id and the user type byte. The record deliberately carries no credential
 * material — passwords are never decoded.
 */
public record CardDemoUser(String id, String type) {

    public Subject toSubject() {
        return new Subject(id, SubjectKind.HUMAN, Optional.of(type));
    }
}
