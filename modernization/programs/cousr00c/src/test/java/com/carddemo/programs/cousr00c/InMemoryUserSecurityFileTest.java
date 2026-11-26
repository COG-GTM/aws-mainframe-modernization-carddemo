package com.carddemo.programs.cousr00c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class InMemoryUserSecurityFileTest {

    private final InMemoryUserSecurityFile file = new InMemoryUserSecurityFile(Fixtures.users(3));

    private String nextId() {
        return file.readNext().record().secUsrId();
    }

    private String prevId() {
        return file.readPrev().record().secUsrId();
    }

    @Test
    void startbrGteqPositionsAtCeilingKey() {
        assertEquals(CicsResp.NORMAL, file.startBr("USER0002").resp());
        assertEquals("USER0002", nextId());
        assertEquals("USER0003", nextId());
        assertEquals(CicsResp.ENDFILE, file.readNext().response().resp());
        assertEquals(CicsResp.NORMAL, file.endBr().resp());
        assertFalse(file.browseActive());
    }

    @Test
    void startbrLowValuesStartsAtFirstRecord() {
        file.startBr(Cobol.lowValues(8));
        assertEquals("USER0001", nextId());
    }

    @Test
    void startbrPastLastKeyIsNotfndAndLeavesNoBrowse() {
        assertEquals(CicsResp.NOTFND, file.startBr("USER9999").resp());
        assertFalse(file.browseActive());
        assertEquals(CicsResp.INVREQ, file.endBr().resp());
        assertEquals(CicsResp.INVREQ, file.readNext().response().resp());
    }

    @Test
    void startbrHighValuesThenReadprevReturnsLastRecord() {
        assertEquals(CicsResp.NORMAL, file.startBr(Cobol.highValues(8)).resp());
        assertEquals(CicsResp.ENDFILE, file.readNext().response().resp());
        file.startBr(Cobol.highValues(8));
        assertEquals("USER0003", prevId());
        assertEquals("USER0002", prevId());
    }

    @Test
    void readprevAfterStartbrNeedsExactKey() {
        file.startBr("USER0002");
        assertEquals("USER0002", prevId());
        assertEquals("USER0001", prevId());
        assertEquals(CicsResp.ENDFILE, file.readPrev().response().resp());
        file.startBr("USER000");
        assertEquals(CicsResp.NOTFND, file.readPrev().response().resp());
    }

    @Test
    void directionChangeRereadsLastRecord() {
        file.startBr("USER0001");
        assertEquals("USER0001", nextId());
        assertEquals("USER0002", nextId());
        assertEquals("USER0002", prevId());
        assertEquals("USER0001", prevId());
        assertEquals("USER0001", nextId());
    }

    @Test
    void injectedFailureIsReturnedOnceAndCallsAreRecorded() {
        file.injectFailure(InMemoryUserSecurityFile.Op.STARTBR, new CicsResponse(CicsResp.NOTOPEN, 0));
        assertEquals(CicsResp.NOTOPEN, file.startBr("USER0001").resp());
        assertEquals(CicsResp.NORMAL, file.startBr("USER0001").resp());
        assertTrue(file.browseActive());
        assertEquals(2, file.calls().size());
    }

    @Test
    void deleteUsesPaddedKey() {
        file.delete("USER0002");
        file.startBr("USER0002");
        assertEquals("USER0003", nextId());
    }
}
