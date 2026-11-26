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
