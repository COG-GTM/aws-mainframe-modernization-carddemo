package com.carddemo.programs.cotrn00c;

import com.carddemo.programs.cotrn00c.copybook.CobolText;
import org.junit.jupiter.api.Test;

import static com.carddemo.programs.cotrn00c.Fixtures.id;
import static org.junit.jupiter.api.Assertions.assertEquals;

class InMemoryTransactFileTest {

    private final InMemoryTransactFile file = new InMemoryTransactFile(Fixtures.trans(5));

    @Test
    void startbrIsGteqAndReadnextReadsFromPosition() {
        file.remove(id(3));
        assertEquals(CicsResp.NORMAL, file.startBr(id(3)).resp());
        assertEquals(id(4), file.readNext().record().tranId());
        assertEquals(id(5), file.readNext().record().tranId());
        assertEquals(CicsResp.ENDFILE, file.readNext().response().resp());
        assertEquals(CicsResp.NORMAL, file.endBr().resp());
    }

    @Test
    void startbrPastLastKeyIsNotfndAndLeavesNoBrowse() {
        assertEquals(CicsResp.NOTFND, file.startBr(id(9)).resp());
        assertEquals(CicsResp.INVREQ, file.readNext().response().resp());
        assertEquals(CicsResp.INVREQ, file.endBr().resp());
    }

    @Test
    void highValuesPositionsAtEndOfFile() {
        assertEquals(CicsResp.NORMAL, file.startBr(CobolText.highValues(16)).resp());
        assertEquals(CicsResp.ENDFILE, file.readNext().response().resp());
        file.startBr(CobolText.highValues(16));
        assertEquals(id(5), file.readPrev().record().tranId());
    }

    @Test
    void readprevAfterStartbrReadsKeyThenWalksBack() {
        file.startBr(id(2));
        assertEquals(id(2), file.readPrev().record().tranId());
        assertEquals(id(1), file.readPrev().record().tranId());
        assertEquals(CicsResp.ENDFILE, file.readPrev().response().resp());
    }

    @Test
    void readprevAfterStartbrOnMissingKeyIsNotfnd() {
        file.remove(id(3));
        file.startBr(id(3));
        assertEquals(CicsResp.NOTFND, file.readPrev().response().resp());
    }

    @Test
    void changingDirectionRereadsSameRecord() {
        file.startBr(id(2));
        file.readNext();
        assertEquals(id(3), file.readNext().record().tranId());
        assertEquals(id(3), file.readPrev().record().tranId());
        assertEquals(id(2), file.readPrev().record().tranId());
    }

    @Test
    void injectedFailureIsReturnedOnce() {
        file.injectFailure(InMemoryTransactFile.Op.STARTBR, new CicsResponse(CicsResp.NOTOPEN, 60));
        assertEquals(new CicsResponse(CicsResp.NOTOPEN, 60), file.startBr(id(1)));
        assertEquals(CicsResp.NORMAL, file.startBr(id(1)).resp());
    }
}
