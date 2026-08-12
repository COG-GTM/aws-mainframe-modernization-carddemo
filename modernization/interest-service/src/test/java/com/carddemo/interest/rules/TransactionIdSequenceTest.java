package com.carddemo.interest.rules;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/** Business-rule tests for BR-7a (transaction id generation). */
class TransactionIdSequenceTest {

    @Test
    @DisplayName("Ids are the run-date parm followed by a six-digit counter starting at one")
    void idsStartAtOne() {
        TransactionIdSequence sequence = new TransactionIdSequence("2022071800");
        assertEquals("2022071800000001", sequence.next());
        assertEquals("2022071800000002", sequence.next());
    }

    @Test
    @DisplayName("The counter wraps at one million, as PIC 9(06) does without ON SIZE ERROR")
    void counterWrapsAtOneMillion() {
        TransactionIdSequence sequence = new TransactionIdSequence("2022071800");
        String id = null;
        for (int index = 0; index < 1_000_000; index++) {
            id = sequence.next();
        }
        assertEquals("2022071800000000", id, "the millionth id wraps to suffix 000000");
        assertEquals(16, id.length(), "TRAN-ID is PIC X(16) and must never overflow it");
        assertEquals("2022071800000001", sequence.next());
    }

    @Test
    @DisplayName("A run date that is not the ten-character JCL PARM is rejected")
    void runDateMustMatchTheParm() {
        assertThrows(IllegalArgumentException.class, () -> new TransactionIdSequence("2022"));
    }
}
