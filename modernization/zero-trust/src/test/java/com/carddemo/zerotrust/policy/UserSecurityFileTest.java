package com.carddemo.zerotrust.policy;

import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class UserSecurityFileTest {

    private static final Path USRSEC =
            Path.of("../../app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS").toAbsolutePath().normalize();

    @Test
    void parsesTheShippedSecurityFile() {
        List<CardDemoUser> users = UserSecurityFile.load(USRSEC);
        assertThat(users).hasSize(10);
        assertThat(users.stream().filter(u -> u.type().equals("A"))).hasSize(5);
        assertThat(users.stream().filter(u -> u.type().equals("U"))).hasSize(5);
        assertThat(users.stream().map(CardDemoUser::id))
                .contains("ADMIN001", "ADMIN005", "USER0001", "USER0005");
    }

    @Test
    void userRecordCarriesNoPasswordField() {
        // The record type has exactly two components (id and type) — there is nowhere a
        // password could be carried. The parser never decodes the PWD bytes.
        assertThat(CardDemoUser.class.getRecordComponents()).hasSize(2);
        for (CardDemoUser u : UserSecurityFile.load(USRSEC)) {
            assertThat(u.id()).doesNotContain("PASSWOR");
            assertThat(u.type()).hasSize(1);
        }
    }

    @Test
    void usersDeriveSubjects() {
        List<CardDemoUser> users = UserSecurityFile.load(USRSEC);
        Subject admin = users.stream().filter(u -> u.id().equals("ADMIN001")).findFirst()
                .orElseThrow().toSubject();
        assertThat(admin.kind()).isEqualTo(SubjectKind.HUMAN);
        assertThat(admin.userType()).contains("A");
    }
}
