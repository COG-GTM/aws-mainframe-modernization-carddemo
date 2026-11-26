package com.carddemo.programs.cbstm03a.support;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class CobolStringTest {

    @Test
    void delimitedByStopsAtFirstOccurrence() {
        assertEquals("Mary Smith     ", CobolString.into(" ".repeat(15))
                .delimitedBy("Mary Ann  ", " ").bySize(" ").delimitedBy("Smith     ", " ").result());
    }

    @Test
    void missingDelimiterTransfersWholeItem() {
        assertEquals("<p>abc</p>", CobolString.into(" ".repeat(10))
                .delimitedBy("<p>", "*").delimitedBy("abc", "*").delimitedBy("</p>", "*").result());
    }

    @Test
    void receiverIsNotClearedAndOverflowIsSilentlyTruncated() {
        assertEquals("abXXX", CobolString.into("XXXXX").bySize("ab").result());
        assertEquals("abcde", CobolString.into("     ").bySize("abcdefgh").result());
    }

    @Test
    void leadingDelimiterTransfersNothing() {
        assertEquals("|    ", CobolString.into("     ").delimitedBy("  abc", "  ").bySize("|").result());
    }
}
