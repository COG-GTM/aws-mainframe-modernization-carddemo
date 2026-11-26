package com.carddemo.programs.cousr03c.gateway;

import com.carddemo.programs.cousr03c.copybook.SecUserData;
import org.junit.jupiter.api.Test;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class InMemoryUserSecurityFileTest {

    private static final Charset EBCDIC = Charset.forName("IBM037");

    private static List<String> unloadedKeys(InMemoryUserSecurityFile f, Charset cs) {
        byte[] out = f.unload(cs);
        return java.util.stream.IntStream.range(0, out.length / SecUserData.LENGTH)
                .mapToObj(i -> SecUserData.parse(
                        java.util.Arrays.copyOfRange(out, i * SecUserData.LENGTH, (i + 1) * SecUserData.LENGTH), cs).usrId())
                .toList();
    }

    @Test
    void unloadUsesKsdsByteOrderOfTargetCharset() {
        InMemoryUserSecurityFile f = new InMemoryUserSecurityFile();
        f.put(SecUserData.of("90000001", "A", "B", "PASSWORD", "U"));
        f.put(SecUserData.of("A0000001", "C", "D", "PASSWORD", "U"));
        // EBCDIC: letters (X'C1') collate before digits (X'F0'); ASCII is the reverse.
        assertEquals(List.of("A0000001", "90000001"), unloadedKeys(f, EBCDIC));
        assertEquals(List.of("90000001", "A0000001"), unloadedKeys(f, StandardCharsets.ISO_8859_1));
    }

    @Test
    void loadRejectsPartialTrailingRecord() {
        byte[] bad = new byte[SecUserData.LENGTH + 10];
        IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
                () -> InMemoryUserSecurityFile.load(bad, EBCDIC));
        assertEquals("USRSEC dataset length 90 is not a multiple of 80", e.getMessage());
    }
}
