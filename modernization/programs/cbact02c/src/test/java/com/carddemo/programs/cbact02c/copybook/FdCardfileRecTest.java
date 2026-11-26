package com.carddemo.programs.cbact02c.copybook;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class FdCardfileRecTest {

    @Test
    void splitsKeyAndData() {
        String image = "4444333322221111" + "D".repeat(134);
        FdCardfileRec rec = FdCardfileRec.fromFixedWidth(image);

        assertEquals("4444333322221111", rec.fdCardNum());
        assertEquals(134, rec.fdCardData().length());
        assertEquals(image, rec.toFixedWidth());
    }

    @Test
    void shortImageIsSpacePadded() {
        FdCardfileRec rec = FdCardfileRec.fromFixedWidth("123");

        assertEquals("123" + " ".repeat(13), rec.fdCardNum());
        assertEquals(" ".repeat(134), rec.fdCardData());
    }
}
