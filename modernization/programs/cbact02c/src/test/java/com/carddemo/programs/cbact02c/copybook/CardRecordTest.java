package com.carddemo.programs.cbact02c.copybook;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CardRecordTest {

    @Test
    void layoutMatchesCvact02yRecln150() {
        assertEquals(150, CardRecord.CARD_NUM_LEN + CardRecord.CARD_ACCT_ID_LEN + CardRecord.CARD_CVV_CD_LEN
                + CardRecord.CARD_EMBOSSED_NAME_LEN + CardRecord.CARD_EXPIRAION_DATE_LEN
                + CardRecord.CARD_ACTIVE_STATUS_LEN + CardRecord.FILLER_LEN);
        assertEquals(91, CardRecord.FILLER_OFFSET);
    }

    @Test
    void serialisesNumericFieldsZeroFilledAndAlphaSpacePadded() {
        CardRecord card = new CardRecord("12345", 42, 7, "NAME", "2030-01-31", "Y", null);

        String image = card.toFixedWidth();

        assertEquals(150, image.length());
        assertEquals("12345           ", image.substring(0, 16));
        assertEquals("00000000042", image.substring(16, 27));
        assertEquals("007", image.substring(27, 30));
        assertEquals("NAME" + " ".repeat(46), image.substring(30, 80));
        assertEquals("2030-01-31Y", image.substring(80, 91));
        assertEquals(" ".repeat(59), image.substring(91));
    }

    @Test
    void alphanumericFieldsTruncateOnTheRight() {
        CardRecord card = new CardRecord("12345678901234567890", 0, 0, "X".repeat(60), "2030-01-31-EXTRA", "YN", "");

        assertEquals("1234567890123456", card.cardNum());
        assertEquals("X".repeat(50), card.cardEmbossedName());
        assertEquals("2030-01-31", card.cardExpiraionDate());
        assertEquals("Y", card.cardActiveStatus());
    }

    @Test
    void rejectsValuesThatDoNotFitThePic() {
        assertThrows(IllegalArgumentException.class, () -> new CardRecord("1", 100_000_000_000L, 0, "", "", "", ""));
        assertThrows(IllegalArgumentException.class, () -> new CardRecord("1", -1, 0, "", "", "", ""));
        assertThrows(IllegalArgumentException.class, () -> new CardRecord("1", 0, 1000, "", "", "", ""));
    }

    @Test
    void parseRejectsNonNumericAcctIdAndCvv() {
        String good = new CardRecord("1", 1, 1, "N", "2030-01-31", "Y", "").toFixedWidth();
        String badAcct = good.substring(0, 16) + "0000000004X" + good.substring(27);
        String badCvv = good.substring(0, 27) + " 12" + good.substring(30);

        IllegalArgumentException e1 = assertThrows(IllegalArgumentException.class, () -> CardRecord.fromFixedWidth(badAcct));
        IllegalArgumentException e2 = assertThrows(IllegalArgumentException.class, () -> CardRecord.fromFixedWidth(badCvv));
        assertEquals("CARD-ACCT-ID is not numeric: '0000000004X'", e1.getMessage());
        assertEquals("CARD-CVV-CD is not numeric: ' 12'", e2.getMessage());
    }

    @Test
    void parsePadsShortImageWithSpaces() {
        String image = "4444333322221111" + "00000000099" + "321";
        CardRecord card = CardRecord.fromFixedWidth(image);

        assertEquals(99L, card.cardAcctId());
        assertEquals(321, card.cardCvvCd());
        assertEquals(" ".repeat(50), card.cardEmbossedName());
        assertEquals(150, card.toFixedWidth().length());
    }
}
