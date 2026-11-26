package com.carddemo.programs.cbact02c.io;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class InMemoryCardFileTest {

    @Test
    void wrongLengthRecordGivesStatus04WithData() {
        InMemoryCardFile file = new InMemoryCardFile(List.of("SHORT"));
        file.open();

        CardFile.ReadResult result = file.read();

        assertEquals("04", result.status());
        assertEquals("SHORT", result.record());
    }

    @Test
    void ordersKeysByEbcdicCollation() {
        String digit = "1".repeat(150);
        String upper = "A".repeat(150);
        String lower = "a".repeat(150);
        String space = " " + "Z".repeat(149);
        InMemoryCardFile file = new InMemoryCardFile(List.of(digit, upper, lower, space));
        file.open();

        assertEquals(space, file.read().record());
        assertEquals(lower, file.read().record());
        assertEquals(upper, file.read().record());
        assertEquals(digit, file.read().record());
    }

    @Test
    void reopenRestartsFromFirstKey() {
        String a = "A".repeat(150);
        InMemoryCardFile file = new InMemoryCardFile(List.of(a));
        file.open();
        file.read();
        file.close();
        file.open();

        assertEquals(a, file.read().record());
    }
}
