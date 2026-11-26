package com.carddemo.programs.cobil00c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cobil00c.copybook.AccountRecord;
import com.carddemo.programs.cobil00c.copybook.CardDemoCommarea;
import com.carddemo.programs.cobil00c.copybook.CardXrefRecord;
import com.carddemo.programs.cobil00c.copybook.Cobol;
import com.carddemo.programs.cobil00c.copybook.Dfhaid;
import com.carddemo.programs.cobil00c.copybook.Dfhbmsca;
import com.carddemo.programs.cobil00c.copybook.TranRecord;
import com.carddemo.programs.cobil00c.gateway.CicsAbendException;
import com.carddemo.programs.cobil00c.gateway.CicsResp;
import com.carddemo.programs.cobil00c.gateway.InMemoryAccountDataGateway;
import com.carddemo.programs.cobil00c.gateway.InMemoryCardXrefAixGateway;
import com.carddemo.programs.cobil00c.gateway.InMemoryTransactGateway;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class Cobil00cTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T10:15:30Z"), ZoneOffset.UTC);
    private static final String TS = "2026-11-26 10:15:30.000000";

    private InMemoryAccountDataGateway acctdat;
    private InMemoryCardXrefAixGateway cxacaix;
    private InMemoryTransactGateway transact;
    private Cobil00c program;

    @BeforeEach
    void setUp() {
        setUp(List.of(
                account(1, "194.00"),
                account(2, "0.00"),
                account(3, "-25.50"),
                account(4, "1234567890.12")),
                List.of(
                        new CardXrefRecord("4111111111111111", 100000001, 1),
                        new CardXrefRecord("4222222222222222", 100000004, 4)),
                List.of(tran("0000000000000003"), tran("0000000000000005"), tran("0000000000000001")));
    }

    private void setUp(List<AccountRecord> accounts, List<CardXrefRecord> xrefs, List<TranRecord> trans) {
        acctdat = new InMemoryAccountDataGateway(accounts);
        cxacaix = new InMemoryCardXrefAixGateway(xrefs);
        transact = new InMemoryTransactGateway(trans);
        program = new Cobil00c(acctdat, cxacaix, transact, CLOCK);
    }

    static AccountRecord account(long id, String bal) {
        return new AccountRecord(id, "Y", new BigDecimal(bal), new BigDecimal("5000.00"), new BigDecimal("1000.00"),
                "2014-11-20", "2025-05-20", "2025-05-20", BigDecimal.ZERO, BigDecimal.ZERO, "A000000000", "GRP1");
    }

    static TranRecord tran(String id) {
        return new TranRecord(id, "01", 1, "POS TERM", "Purchase", new BigDecimal("10.00"), 1, "M", "C", "Z",
                "4111111111111111", "2022-06-10 19:27:53.000000", "");
    }

    static CardDemoCommarea reenter() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromTranId("CM00");
        c.fromProgram("COMEN01C");
        c.userId("USER0001");
        c.userType("U");
        c.pgmContext("1");
        return c;
    }

    static CardDemoCommarea firstEntry() {
        CardDemoCommarea c = reenter();
        c.pgmContext("0");
        return c;
    }

    private BillPayResult enter(String acct, String confirm) {
        return program.execute(BillPayRequest.of(reenter(), Dfhaid.DFHENTER, BillPayScreenInput.of(acct, confirm)));
    }

    private static BillPayScreen last(BillPayResult r) {
        return r.finalScreen().orElseThrow();
    }

    private static List<String> messages(BillPayResult r) {
        List<String> out = new ArrayList<>();
        r.screensSent().forEach(s -> out.add(s.message()));
        return out;
    }

    @Nested
    class MainPara {

        @Test
        void eibcalenZeroTransfersToSignon() {
            BillPayResult r = program.execute(BillPayRequest.noCommarea());
            assertEquals(BillPayResult.Action.XCTL, r.action());
            assertEquals("COSGN00C", r.nextProgram());
            assertEquals("CB00", r.commarea().fromTranId());
            assertEquals("COBIL00C", r.commarea().fromProgram());
            assertTrue(r.commarea().isPgmEnter());
            assertTrue(r.screensSent().isEmpty());
        }

        @Test
        void firstEntrySendsEmptyScreenAndSetsReenter() {
            BillPayResult r = program.execute(BillPayRequest.of(firstEntry(), Dfhaid.DFHENTER, null));
            assertEquals(BillPayResult.Action.RETURN, r.action());
            assertEquals("CB00", r.nextTransId());
            assertTrue(r.commarea().isPgmReenter());
            assertEquals(1, r.screensSent().size());
            BillPayScreen s = last(r);
            assertEquals(Cobol.lowValues(11), s.actidin());
            assertEquals(Cobol.lowValues(14), s.curbal());
            assertEquals(Cobol.lowValues(1), s.confirm());
            assertEquals(Cobol.spaces(78), s.errmsg());
            assertEquals("ACTIDIN", s.cursorField());
            assertEquals(Dfhbmsca.DFHDFCOL, s.errmsgColor());
            assertEquals(218, r.commarea().toBytes().length());
        }

        @Test
        void headerInfoIsPopulatedOnEverySend() {
            BillPayScreen s = last(program.execute(BillPayRequest.of(firstEntry(), Dfhaid.DFHENTER, null)));
            assertEquals("CB00", s.trnname());
            assertEquals("COBIL00C", s.pgmname());
            assertEquals("      AWS Mainframe Modernization       ", s.title01());
            assertEquals("              CardDemo                  ", s.title02());
            assertEquals("11/26/26", s.curdate());
            assertEquals("10:15:30", s.curtime());
        }

        @Test
        void firstEntryWithSelectedTransactionRunsEnterKeyProcessing() {
            CardDemoCommarea c = firstEntry();
            c.cb00TrnSelected("00000000001XXXXX");
            BillPayResult r = program.execute(BillPayRequest.of(c, Dfhaid.DFHENTER, null));
            assertEquals(2, r.screensSent().size());
            BillPayScreen s = last(r);
            assertEquals("00000000001", s.actidin());
            assertEquals("+0000000194.00", s.curbal());
            assertEquals("Confirm to make a bill payment...", s.message());
            // ACTIDINL and CONFIRML are both -1: cursor goes to the first in map order
            assertEquals("ACTIDIN", s.cursorField());
        }

        @Test
        void shortCommareaIsSpacePaddedAndPreserved() {
            String base = reenter().toBytes().substring(0, CardDemoCommarea.BASE_LENGTH);
            BillPayResult r = program.execute(new BillPayRequest(CardDemoCommarea.BASE_LENGTH, base, Dfhaid.DFHPF4,
                    BillPayScreenInput.empty()));
            assertEquals(Cobol.spaces(16), r.commarea().cb00TrnSelected());
            assertEquals(Cobol.spaces(8), r.commarea().cb00PageNum());
            assertEquals("USER0001", r.commarea().userId());
            assertEquals(base, r.commarea().toBytes().substring(0, CardDemoCommarea.BASE_LENGTH));
        }

        @Test
        void pf3WithoutFromProgramReturnsToMainMenu() {
            CardDemoCommarea c = reenter();
            c.fromProgram("");
            BillPayResult r = program.execute(BillPayRequest.of(c, Dfhaid.DFHPF3, BillPayScreenInput.empty()));
            assertEquals(BillPayResult.Action.XCTL, r.action());
            assertEquals("COMEN01C", r.nextProgram());
            assertEquals("COBIL00C", r.commarea().fromProgram());
            assertEquals("CB00", r.commarea().fromTranId());
            assertEquals("0", r.commarea().pgmContext());
        }

        @Test
        void pf3WithLowValueFromProgramReturnsToMainMenu() {
            CardDemoCommarea c = reenter();
            c.fromProgram(Cobol.lowValues(8));
            BillPayResult r = program.execute(BillPayRequest.of(c, Dfhaid.DFHPF3, BillPayScreenInput.empty()));
            assertEquals("COMEN01C", r.nextProgram());
        }

        @Test
        void pf3ReturnsToCallingProgram() {
            CardDemoCommarea c = reenter();
            c.fromProgram("COADM01C");
            BillPayResult r = program.execute(BillPayRequest.of(c, Dfhaid.DFHPF3, BillPayScreenInput.empty()));
            assertEquals("COADM01C", r.nextProgram());
            assertEquals("COADM01C", r.commarea().toProgram());
        }

        @Test
        void pf4ClearsTheScreen() {
            BillPayResult r = program.execute(BillPayRequest.of(reenter(), Dfhaid.DFHPF4,
                    BillPayScreenInput.of("00000000001", "+0000000194.00", "Y")));
            assertEquals(1, r.screensSent().size());
            BillPayScreen s = last(r);
            assertEquals(Cobol.spaces(11), s.actidin());
            assertEquals(Cobol.spaces(14), s.curbal());
            assertEquals(" ", s.confirm());
            assertEquals("", s.message());
            assertEquals("ACTIDIN", s.cursorField());
            assertTrue(transact.find("0000000000000006").isEmpty());
        }

        @ParameterizedTest
        @ValueSource(strings = {"DFHPF5", "DFHPF12", "DFHCLEAR", "DFHPA1"})
        void otherKeyShowsInvalidKeyMessageAndKeepsInput(String aid) {
            BillPayResult r = program.execute(BillPayRequest.of(reenter(), Dfhaid.valueOf(aid),
                    BillPayScreenInput.of("00000000001", "+0000000194.00", "Y")));
            BillPayScreen s = last(r);
            assertEquals("Invalid key pressed. Please see below...", s.message());
            assertEquals("Invalid key pressed. Please see below...          " + Cobol.spaces(28), s.errmsg());
            assertEquals("00000000001", s.actidin());
            assertEquals("+0000000194.00", s.curbal());
            assertEquals(BillPayResult.Action.RETURN, r.action());
        }
    }

    @Nested
    class ProcessEnterKey {

        @Test
        void emptyAccountIdIsRejected() {
            BillPayResult r = enter("           ", " ");
            assertEquals(List.of("Acct ID can NOT be empty..."), messages(r));
            assertEquals("ACTIDIN", last(r).cursorField());
        }

        @Test
        void lowValueAccountIdIsRejected() {
            BillPayResult r = enter(null, null);
            assertEquals(List.of("Acct ID can NOT be empty..."), messages(r));
        }

        @Test
        void blankConfirmShowsBalanceAndAsksForConfirmation() {
            BillPayResult r = enter("00000000001", " ");
            assertEquals(1, r.screensSent().size());
            BillPayScreen s = last(r);
            assertEquals("+0000000194.00", s.curbal());
            assertEquals("Confirm to make a bill payment...", s.message());
            assertEquals("CONFIRM", s.cursorField());
            assertEquals(3, transact.records().size());
            assertEquals(new BigDecimal("194.00"), acctdat.find("00000000001").orElseThrow().acctCurrBal());
        }

        @Test
        void lowValueConfirmBehavesLikeBlank() {
            BillPayResult r = enter("00000000001", null);
            assertEquals("Confirm to make a bill payment...", last(r).message());
        }

        @ParameterizedTest
        @ValueSource(strings = {"X", "1", "?"})
        void invalidConfirmValueIsRejected(String confirm) {
            BillPayResult r = program.execute(BillPayRequest.of(reenter(), Dfhaid.DFHENTER,
                    BillPayScreenInput.of("00000000001", "+0000000777.00", confirm)));
            assertEquals(List.of("Invalid value. Valid values are (Y/N)..."), messages(r));
            BillPayScreen s = last(r);
            assertEquals("CONFIRM", s.cursorField());
            assertEquals("+0000000777.00", s.curbal());
        }

        @ParameterizedTest
        @ValueSource(strings = {"N", "n"})
        void declineClearsTheScreenWithoutPaying(String confirm) {
            BillPayResult r = enter("00000000001", confirm);
            assertEquals(1, r.screensSent().size());
            BillPayScreen s = last(r);
            assertEquals(Cobol.spaces(11), s.actidin());
            assertEquals("", s.message());
            assertEquals("ACTIDIN", s.cursorField());
            assertEquals(3, transact.records().size());
        }

        @Test
        void accountNotFound() {
            BillPayResult r = enter("00000000099", " ");
            assertEquals(List.of("Account ID NOT found..."), messages(r));
            assertEquals("ACTIDIN", last(r).cursorField());
        }

        @Test
        void accountIdIsUsedAsRawKeyBytesSoShortInputIsNotFound() {
            BillPayResult r = enter("1", "Y");
            assertEquals(List.of("Account ID NOT found..."), messages(r));
        }

        @Test
        void accountReadErrorDisplaysRespAndShowsLookupMessage() {
            acctdat.force(InMemoryAccountDataGateway.Op.READ, CicsResp.IOERR);
            BillPayResult r = enter("00000000001", " ");
            assertEquals(List.of("Unable to lookup Account..."), messages(r));
            assertEquals(List.of("RESP:00000001GREAS:00000000{"), r.displayLines());
        }

        @ParameterizedTest
        @ValueSource(strings = {"00000000002", "00000000003"})
        void nothingToPayWhenBalanceZeroOrNegative(String acct) {
            BillPayResult r = enter(acct, "Y");
            assertEquals(List.of("You have nothing to pay..."), messages(r));
            assertEquals("ACTIDIN", last(r).cursorField());
            assertEquals(3, transact.records().size());
        }

        @Test
        void negativeBalanceIsEditedWithLeadingMinus() {
            program.execute(BillPayRequest.of(reenter(), Dfhaid.DFHENTER, BillPayScreenInput.of("00000000003", " ")));
            BillPayScreen s = last(enter("00000000003", " "));
            assertEquals("-0000000025.50", s.curbal());
        }

        @ParameterizedTest
        @ValueSource(strings = {"Y", "y"})
        void confirmedPaymentWritesTransactionAndZeroesBalance(String confirm) {
            BillPayResult r = enter("00000000001", confirm);

            TranRecord t = transact.find("0000000000000006").orElseThrow();
            assertEquals("02", t.tranTypeCd());
            assertEquals(2, t.tranCatCd());
            assertEquals("POS TERM  ", t.tranSource());
            assertEquals(Cobol.alnum("BILL PAYMENT - ONLINE", 100), t.tranDesc());
            assertEquals(new BigDecimal("194.00"), t.tranAmt());
            assertEquals("4111111111111111", t.tranCardNum());
            assertEquals(999999999L, t.tranMerchantId());
            assertEquals(Cobol.alnum("BILL PAYMENT", 50), t.tranMerchantName());
            assertEquals(Cobol.alnum("N/A", 50), t.tranMerchantCity());
            assertEquals(Cobol.alnum("N/A", 10), t.tranMerchantZip());
            assertEquals(TS, t.tranOrigTs());
            assertEquals(TS, t.tranProcTs());

            assertEquals(new BigDecimal("0.00"), acctdat.find("00000000001").orElseThrow().acctCurrBal());

            // WRITE-TRANSACT-FILE sends the success screen, then PROCESS-ENTER-KEY sends it again
            assertEquals(2, r.screensSent().size());
            BillPayScreen s = last(r);
            assertEquals("Payment successful.  Your Transaction ID is 0000000000000006.", s.message());
            assertEquals(Dfhbmsca.DFHGREEN, s.errmsgColor());
            assertEquals(Cobol.spaces(11), s.actidin());
            assertEquals(Cobol.spaces(14), s.curbal());
            assertEquals(" ", s.confirm());
            assertEquals("ACTIDIN", s.cursorField());
            assertTrue(r.displayLines().isEmpty());
            assertFalse(transact.isBrowsing());
        }

        @Test
        void firstTransactionOnEmptyFileGetsIdOne() {
            setUp(List.of(account(1, "50.00")), List.of(new CardXrefRecord("4111111111111111", 1, 1)), List.of());
            BillPayResult r = enter("00000000001", "Y");
            assertTrue(transact.find("0000000000000001").isPresent());
            assertEquals("Payment successful.  Your Transaction ID is 0000000000000001.", last(r).message());
        }

        @Test
        void transactionIdWrapsOnOverflowWithoutSizeError() {
            setUp(List.of(account(1, "50.00")), List.of(new CardXrefRecord("4111111111111111", 1, 1)),
                    List.of(tran("9999999999999999")));
            enter("00000000001", "Y");
            assertTrue(transact.find("0000000000000000").isPresent());
        }

        @Test
        void balanceAboveTranAmtPictureIsHighOrderTruncated() {
            enter("00000000004", "Y");
            TranRecord t = transact.find("0000000000000006").orElseThrow();
            assertEquals(new BigDecimal("234567890.12"), t.tranAmt());
            assertEquals(new BigDecimal("1000000000.00"), acctdat.find("00000000004").orElseThrow().acctCurrBal());
            assertEquals("4222222222222222", t.tranCardNum());
        }
    }

    @Nested
    class FileErrorPaths {

        @Test
        void xrefNotFoundStillPostsPaymentWithBlankCard() {
            setUp(List.of(account(1, "194.00")), List.of(), List.of(tran("0000000000000005")));
            BillPayResult r = enter("00000000001", "Y");
            assertEquals(List.of("Account ID NOT found...",
                    "Payment successful.  Your Transaction ID is 0000000000000006.",
                    "Payment successful.  Your Transaction ID is 0000000000000006."), messages(r));
            assertEquals(Cobol.spaces(16), transact.find("0000000000000006").orElseThrow().tranCardNum());
            assertEquals(new BigDecimal("0.00"), acctdat.find("00000000001").orElseThrow().acctCurrBal());
        }

        @Test
        void xrefDuplicateKeyUsesFirstRecordAndShowsAixMessage() {
            setUp(List.of(account(1, "194.00")), List.of(new CardXrefRecord("4999999999999999", 1, 1),
                    new CardXrefRecord("4111111111111111", 1, 1)), List.of());
            BillPayResult r = enter("00000000001", "Y");
            assertEquals("Unable to lookup XREF AIX file...", messages(r).get(0));
            assertEquals(List.of("RESP:00000001EREAS:00000000{"), r.displayLines());
            assertEquals("4111111111111111", transact.find("0000000000000001").orElseThrow().tranCardNum());
        }

        @Test
        void startBrowseNotFoundCascadesToEndBrowseAbend() {
            transact.force(InMemoryTransactGateway.Op.STARTBR, CicsResp.NOTFND);
            CicsAbendException e = assertThrows(CicsAbendException.class, () -> enter("00000000001", "Y"));
            assertEquals("AEIP", e.abendCode());
            assertEquals(List.of("RESP:00000001FREAS:00000000{"), e.displayLines());
            assertEquals(3, transact.records().size());
            assertEquals(new BigDecimal("194.00"), acctdat.find("00000000001").orElseThrow().acctCurrBal());
        }

        @Test
        void startBrowseNotFoundMessagesBeforeAbend() {
            transact.force(InMemoryTransactGateway.Op.STARTBR, CicsResp.NOTFND);
            InMemoryTransactGateway inner = transact;
            Cobil00c p = new Cobil00c(acctdat, cxacaix, new RecordingTransact(inner), CLOCK);
            assertThrows(CicsAbendException.class, () -> p.execute(BillPayRequest.of(reenter(), Dfhaid.DFHENTER,
                    BillPayScreenInput.of("00000000001", "Y"))));
            assertEquals(List.of(CicsResp.NOTFND, CicsResp.INVREQ), RecordingTransact.lastResps);
        }

        @Test
        void startBrowseOtherErrorShowsLookupMessage() {
            transact.force(InMemoryTransactGateway.Op.STARTBR, CicsResp.IOERR);
            CicsAbendException e = assertThrows(CicsAbendException.class, () -> enter("00000000001", "Y"));
            assertEquals(List.of("RESP:00000001GREAS:00000000{", "RESP:00000001FREAS:00000000{"), e.displayLines());
        }

        @Test
        void readPrevErrorLeavesHighValuesKeyAndAbendsWithDataException() {
            transact.force(InMemoryTransactGateway.Op.READPREV, CicsResp.IOERR);
            CicsAbendException e = assertThrows(CicsAbendException.class, () -> enter("00000000001", "Y"));
            assertEquals("ASRA", e.abendCode());
            assertFalse(transact.isBrowsing());
        }

        @Test
        void duplicateTranIdStillDebitsTheAccount() {
            transact.force(InMemoryTransactGateway.Op.WRITE, CicsResp.DUPREC);
            BillPayResult r = enter("00000000001", "Y");
            assertEquals(List.of("Tran ID already exist...", "Tran ID already exist..."), messages(r));
            assertEquals("ACTIDIN", last(r).cursorField());
            assertEquals(Dfhbmsca.DFHDFCOL, last(r).errmsgColor());
            assertEquals(new BigDecimal("0.00"), acctdat.find("00000000001").orElseThrow().acctCurrBal());
            assertEquals("00000000001", last(r).actidin());
        }

        @Test
        void duplicateKeyOnWriteIsTreatedLikeDuplicateRecord() {
            transact.force(InMemoryTransactGateway.Op.WRITE, CicsResp.DUPKEY);
            assertEquals("Tran ID already exist...", messages(enter("00000000001", "Y")).get(0));
        }

        @Test
        void otherWriteErrorShowsAddMessage() {
            transact.force(InMemoryTransactGateway.Op.WRITE, CicsResp.NOSPACE);
            BillPayResult r = enter("00000000001", "Y");
            assertEquals("Unable to Add Bill pay Transaction...", messages(r).get(0));
            assertEquals(List.of("RESP:00000001HREAS:00000000{"), r.displayLines());
        }

        @Test
        void rewriteNotFoundAfterSuccessfulWriteKeepsGreenColour() {
            acctdat.force(InMemoryAccountDataGateway.Op.REWRITE, CicsResp.NOTFND);
            BillPayResult r = enter("00000000001", "Y");
            assertEquals(List.of("Payment successful.  Your Transaction ID is 0000000000000006.",
                    "Account ID NOT found...", "Account ID NOT found..."), messages(r));
            assertEquals(Dfhbmsca.DFHGREEN, last(r).errmsgColor());
            assertTrue(transact.find("0000000000000006").isPresent());
        }

        @Test
        void rewriteOtherErrorShowsUpdateMessage() {
            acctdat.force(InMemoryAccountDataGateway.Op.REWRITE, CicsResp.IOERR);
            BillPayResult r = enter("00000000001", "Y");
            assertEquals("Unable to Update Account...", last(r).message());
            assertEquals(List.of("RESP:00000001GREAS:00000000{"), r.displayLines());
        }
    }

    /** Delegating TRANSACT gateway that records RESP codes seen by the program. */
    static final class RecordingTransact implements com.carddemo.programs.cobil00c.gateway.TransactGateway {
        static List<CicsResp> lastResps = new ArrayList<>();
        private final InMemoryTransactGateway inner;

        RecordingTransact(InMemoryTransactGateway inner) {
            this.inner = inner;
            lastResps = new ArrayList<>();
        }

        @Override
        public com.carddemo.programs.cobil00c.gateway.CicsResponse<Void> startBrowse(String key) {
            var r = inner.startBrowse(key);
            lastResps.add(r.resp());
            return r;
        }

        @Override
        public com.carddemo.programs.cobil00c.gateway.CicsResponse<TranRecord> readPrev() {
            var r = inner.readPrev();
            lastResps.add(r.resp());
            return r;
        }

        @Override
        public void endBrowse() {
            inner.endBrowse();
        }

        @Override
        public com.carddemo.programs.cobil00c.gateway.CicsResponse<Void> write(TranRecord record) {
            return inner.write(record);
        }
    }

    @Test
    void resultHasNoScreenOnXctl() {
        assertNull(program.execute(BillPayRequest.noCommarea()).finalScreen().orElse(null));
    }
}
