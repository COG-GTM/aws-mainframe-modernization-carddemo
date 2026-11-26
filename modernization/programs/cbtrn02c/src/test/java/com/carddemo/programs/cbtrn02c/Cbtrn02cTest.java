package com.carddemo.programs.cbtrn02c;

import com.carddemo.programs.cbtrn02c.Fakes.FaultyIndexedFile;
import com.carddemo.programs.cbtrn02c.Fakes.FaultyReader;
import com.carddemo.programs.cbtrn02c.Fakes.FaultyWriter;
import com.carddemo.programs.cbtrn02c.copybook.AccountRecord;
import com.carddemo.programs.cbtrn02c.copybook.CardXrefRecord;
import com.carddemo.programs.cbtrn02c.copybook.DalytranRecord;
import com.carddemo.programs.cbtrn02c.copybook.TranCatBalRecord;
import com.carddemo.programs.cbtrn02c.copybook.TranRecord;
import com.carddemo.programs.cbtrn02c.io.InMemoryIndexedFile;
import com.carddemo.programs.cbtrn02c.io.LineSequentialReader;
import com.carddemo.programs.cbtrn02c.io.LineSequentialWriter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cbtrn02cTest {

    static final String CARD = "4859452612877065";
    static final Clock CLOCK = Clock.fixed(Instant.parse("2022-06-11T08:09:10.123456Z"), ZoneOffset.UTC);
    static final String PROC_TS = "2022-06-11-08.09.10.120000";

    final List<String> daily = new ArrayList<>();
    InMemoryIndexedFile tran;
    InMemoryIndexedFile xref;
    InMemoryIndexedFile acct;
    InMemoryIndexedFile tcat;
    LineSequentialWriter rejs;
    final List<String> sysout = new ArrayList<>();

    FaultyReader dalyIn;
    FaultyIndexedFile tranIo;
    FaultyIndexedFile xrefIo;
    FaultyWriter rejsIo;
    FaultyIndexedFile acctIo;
    FaultyIndexedFile tcatIo;

    @BeforeEach
    void setUp() {
        tran = new InMemoryIndexedFile(TranRecord.RECORD_LENGTH, 0, 16);
        xref = new InMemoryIndexedFile(CardXrefRecord.RECORD_LENGTH, 0, 16);
        acct = new InMemoryIndexedFile(AccountRecord.RECORD_LENGTH, 0, 11);
        tcat = new InMemoryIndexedFile(TranCatBalRecord.RECORD_LENGTH, 0, 17);
        rejs = new LineSequentialWriter(Cbtrn02c.REJECT_RECORD_LENGTH);
        xref.load(List.of(new CardXrefRecord(CARD, 7, 42, "").format()));
        acct.load(List.of(account("100.00", "1000.00", "200.00", "50.00", "2025-05-20")));
        dalyIn = new FaultyReader(new LineSequentialReader(daily, DalytranRecord.RECORD_LENGTH));
        tranIo = new FaultyIndexedFile(tran);
        xrefIo = new FaultyIndexedFile(xref);
        rejsIo = new FaultyWriter(rejs);
        acctIo = new FaultyIndexedFile(acct);
        tcatIo = new FaultyIndexedFile(tcat);
    }

    static String account(String bal, String limit, String cycCredit, String cycDebit, String expiry) {
        return new AccountRecord(42, "Y", new BigDecimal(bal), new BigDecimal(limit), new BigDecimal("500.00"),
                "2014-11-20", expiry, "2025-05-20", new BigDecimal(cycCredit), new BigDecimal(cycDebit),
                "A000000000", "GRP", "").format();
    }

    static String daily(String id, String card, String amt, String origTs) {
        return new DalytranRecord(id, "01", 5, "POS TERM", "Purchase", new BigDecimal(amt), 800000000L,
                "Merchant", "City", "72112", card, origTs, "", "").format();
    }

    static String daily(String id, String amt) {
        return daily(id, CARD, amt, "2022-06-10 19:27:53.000000");
    }

    int run() {
        return program().run();
    }

    Cbtrn02c program() {
        return new Cbtrn02c(dalyIn, tranIo, xrefIo, rejsIo, acctIo, tcatIo, CLOCK, sysout::add);
    }

    AccountRecord account42() {
        return AccountRecord.parse(acct.asMap().get("00000000042"));
    }

    Cbtrn02cAbendException abend() {
        return assertThrows(Cbtrn02cAbendException.class, this::run);
    }

    // ---- main line, 2000-POST-TRANSACTION, 2900-WRITE-TRANSACTION-FILE

    @Test
    void postsValidCreditTransaction() {
        daily.add(daily("T000000000000001", "25.50"));
        assertEquals(0, run());

        TranRecord t = TranRecord.parse(tran.asMap().get("T000000000000001"));
        DalytranRecord d = DalytranRecord.parse(daily.get(0));
        assertEquals(d.dalytranId(), t.tranId());
        assertEquals(new BigDecimal("25.50"), t.tranAmt());
        assertEquals(d.dalytranOrigTs(), t.tranOrigTs());
        assertEquals(PROC_TS, t.tranProcTs());
        assertEquals(d.format().substring(0, 304), t.format().substring(0, 304));
        assertEquals(" ".repeat(20), t.filler());

        assertEquals(List.of(
                "START OF EXECUTION OF PROGRAM CBTRN02C",
                "TCATBAL record not found for key : 00000000042010005.. Creating.",
                "TRANSACTIONS PROCESSED :000000001",
                "TRANSACTIONS REJECTED  :000000000",
                "END OF EXECUTION OF PROGRAM CBTRN02C"), sysout);
        assertTrue(rejs.records().isEmpty());
    }

    @Test
    void emptyInputEndsWithReturnCodeZero() {
        assertEquals(0, run());
        assertEquals("TRANSACTIONS PROCESSED :000000000", sysout.get(1));
    }

    @Test
    void tranfileIsOpenedOutputSoPriorContentIsReplaced() {
        tran.load(List.of("OLDTRAN000000001"));
        daily.add(daily("T000000000000001", "1.00"));
        run();
        assertEquals(List.of("T000000000000001"), new ArrayList<>(tran.asMap().keySet()));
    }

    // ---- 2800-UPDATE-ACCOUNT-REC

    @Test
    void creditAmountUpdatesBalanceAndCycleCredit() {
        daily.add(daily("T1", "25.50"));
        run();
        AccountRecord a = account42();
        assertEquals(new BigDecimal("125.50"), a.acctCurrBal());
        assertEquals(new BigDecimal("225.50"), a.acctCurrCycCredit());
        assertEquals(new BigDecimal("50.00"), a.acctCurrCycDebit());
    }

    @Test
    void zeroAmountCountsAsCredit() {
        daily.add(daily("T1", "0.00"));
        run();
        assertEquals(new BigDecimal("200.00"), account42().acctCurrCycCredit());
    }

    @Test
    void debitAmountUpdatesBalanceAndCycleDebit() {
        daily.add(daily("T1", "-30.25"));
        run();
        AccountRecord a = account42();
        assertEquals(new BigDecimal("69.75"), a.acctCurrBal());
        assertEquals(new BigDecimal("200.00"), a.acctCurrCycCredit());
        assertEquals(new BigDecimal("19.75"), a.acctCurrCycDebit());
    }

    @Test
    void accountAddTruncatesHighOrderDigitsLikeAddWithoutSizeError() {
        acct.load(List.of(account("9999999999.99", "9999999999.99", "0.00", "0.00", "2099-12-31")));
        daily.add(daily("T1", "0.02"));
        run();
        assertEquals(new BigDecimal("0.01"), account42().acctCurrBal());
    }

    @Test
    void accountRewriteInvalidKeyIsSilentAndTransactionIsStillWritten() {
        acctIo.fail("rewrite", "23");
        daily.add(daily("T1", "5.00"));
        assertEquals(0, run());
        assertEquals(1, tran.records().size());
        assertTrue(rejs.records().isEmpty());
    }

    @Test
    void accountRewriteOtherErrorIsIgnoredBecauseThereIsNoStatusCheck() {
        acctIo.fail("rewrite", "92");
        daily.add(daily("T1", "5.00"));
        assertEquals(0, run());
        assertEquals(1, tran.records().size());
    }

    // ---- 2700-UPDATE-TCATBAL / 2700-A / 2700-B

    @Test
    void createsCategoryBalanceWhenMissing() {
        daily.add(daily("T1", "-12.34"));
        run();
        TranCatBalRecord t = TranCatBalRecord.parse(tcat.asMap().get("00000000042010005"));
        assertEquals(new BigDecimal("-12.34"), t.tranCatBal());
        assertEquals(" ".repeat(22), t.filler());
    }

    @Test
    void updatesExistingCategoryBalance() {
        tcat.load(List.of("000000000420100050000001000{0000000000000000000000"));
        daily.add(daily("T1", "2.50"));
        run();
        assertEquals("000000000420100050000001025{0000000000000000000000", tcat.asMap().get("00000000042010005"));
        assertFalse(sysout.stream().anyMatch(s -> s.startsWith("TCATBAL record not found")));
    }

    @Test
    void initializeKeepsFillerFromPreviouslyReadCategoryRecord() {
        tcat.load(List.of("000000000420100050000001000{0000000000000000000000"));
        daily.add(daily("T1", "1.00"));
        daily.add(new DalytranRecord("T2", "03", 1, "OPERATOR", "Return", new BigDecimal("4.00"), 1, "M", "C",
                "Z", CARD, "2022-06-10 19:27:53.000000", "", "").format());
        run();
        assertEquals("000000000420300010000000040{0000000000000000000000", tcat.asMap().get("00000000042030001"));
    }

    @Test
    void categoryBalanceAddTruncatesToNineIntegerDigits() {
        tcat.load(List.of("00000000042010005" + "9999999999I" + " ".repeat(22)));
        daily.add(daily("T1", "0.01"));
        run();
        assertEquals(new BigDecimal("0.00"),
                TranCatBalRecord.parse(tcat.asMap().get("00000000042010005")).tranCatBal());
    }

    // ---- 1500-VALIDATE-TRAN / 1500-A-LOOKUP-XREF / 1500-B-LOOKUP-ACCT / 2500-WRITE-REJECT-REC

    @Test
    void rejectsUnknownCardWith100() {
        daily.add(daily("T1", "0000000000000000", "1.00", "2022-06-10 19:27:53.000000"));
        assertEquals(4, run());
        String rej = rejs.records().get(0);
        assertEquals(430, rej.length());
        assertEquals(daily.get(0), rej.substring(0, 350));
        assertEquals("0100" + pad("INVALID CARD NUMBER FOUND"), rej.substring(350));
        assertTrue(tran.records().isEmpty());
        assertEquals("TRANSACTIONS REJECTED  :000000001", sysout.get(2));
    }

    @Test
    void unknownCardIsRejectedEvenWhenNumericFieldsAreNotNumeric() {
        String raw = daily("T1", "0000000000000000", "1.00", "2022-06-10 19:27:53.000000");
        raw = raw.substring(0, 143) + "ABCDEFGHI" + raw.substring(152);
        daily.add(raw);
        assertEquals(4, run());
        assertEquals(raw, rejs.records().get(0).substring(0, 350));
        assertEquals("0100", rejs.records().get(0).substring(350, 354));
    }

    @Test
    void postedRecordCopiesNonNumericMerchantIdAndCategoryBytesLikeCobolMove() {
        String raw = daily("T000000000000001", "1.00");
        raw = raw.substring(0, 18) + "00AB" + raw.substring(22, 143) + "ABCDEFGHI" + raw.substring(152);
        daily.add(raw);
        assertEquals(0, run());
        String posted = tran.asMap().get("T000000000000001");
        assertEquals(raw.substring(0, 304), posted.substring(0, 304));
        assertEquals("0000000004201" + "00AB" + "0000000010{" + " ".repeat(22), tcat.asMap().get("000000000420100AB"));
    }

    @Test
    void rejectsMissingAccountWith101() {
        xref.load(List.of(new CardXrefRecord("1111222233334444", 1, 99, "").format()));
        daily.add(daily("T1", "1111222233334444", "1.00", "2022-06-10 19:27:53.000000"));
        assertEquals(4, run());
        assertEquals("0101" + pad("ACCOUNT RECORD NOT FOUND"), rejs.records().get(0).substring(350));
    }

    @Test
    void rejectsOverlimitWith102() {
        // 200.00 - 50.00 + 850.01 = 1000.01 > 1000.00
        daily.add(daily("T1", "850.01"));
        assertEquals(4, run());
        assertEquals("0102" + pad("OVERLIMIT TRANSACTION"), rejs.records().get(0).substring(350));
        assertEquals(new BigDecimal("100.00"), account42().acctCurrBal());
        assertTrue(tcat.records().isEmpty());
    }

    @Test
    void acceptsTransactionExactlyAtCreditLimit() {
        daily.add(daily("T1", "850.00"));
        assertEquals(0, run());
    }

    @Test
    void tempBalanceTruncatesToNineIntegerDigitsBeforeLimitCheck() {
        // 9,999,999,999.00 - 0 + 1.00 -> WS-TEMP-BAL S9(09)V99 keeps 0.00, which is within a 10.00 limit
        acct.load(List.of(account("0.00", "10.00", "9999999999.00", "0.00", "2099-12-31")));
        daily.add(daily("T1", "1.00"));
        assertEquals(0, run());
    }

    @Test
    void rejectsTransactionAfterExpiryWith103() {
        daily.add(daily("T1", CARD, "1.00", "2025-05-21 00:00:00.000000"));
        assertEquals(4, run());
        assertEquals("0103" + pad("TRANSACTION RECEIVED AFTER ACCT EXPIRATION"), rejs.records().get(0).substring(350));
    }

    @Test
    void acceptsTransactionOnExpiryDate() {
        daily.add(daily("T1", CARD, "1.00", "2025-05-20 23:59:59.000000"));
        assertEquals(0, run());
    }

    @Test
    void expiryReasonOverwritesOverlimitReason() {
        daily.add(daily("T1", CARD, "5000.00", "2026-01-01 00:00:00.000000"));
        run();
        assertEquals("0103", rejs.records().get(0).substring(350, 354));
    }

    @Test
    void mixedBatchCountsAndReturnCodeFour() {
        daily.add(daily("T1", "1.00"));
        daily.add(daily("T2", "0000000000000000", "1.00", "2022-06-10 19:27:53.000000"));
        daily.add(daily("T3", "2.00"));
        Cbtrn02c p = program();
        assertEquals(4, p.run());
        assertEquals(3, p.transactionCount());
        assertEquals(1, p.rejectCount());
        assertEquals(2, tran.records().size());
        assertEquals(new BigDecimal("103.00"), account42().acctCurrBal());
        assertEquals("000000000420100050000000030{" + " ".repeat(22), tcat.asMap().get("00000000042010005"));
    }

    // ---- ABEND paths (9999-ABEND-PROGRAM / 9910-DISPLAY-IO-STATUS)

    void assertAbend(String message, String fileStatusLine) {
        Cbtrn02cAbendException e = abend();
        assertEquals(999, e.abendCode());
        int i = sysout.indexOf(message);
        assertTrue(i > 0, () -> "missing '" + message + "' in " + sysout);
        assertEquals(fileStatusLine, sysout.get(i + 1));
        assertEquals("ABENDING PROGRAM", sysout.get(i + 2));
        assertEquals(sysout.size(), i + 3);
    }

    @Test
    void openFailuresAbend() {
        dalyIn.fail("open", "35");
        assertAbend("ERROR OPENING DALYTRAN", "FILE STATUS IS: NNNN0035");
    }

    @Test
    void tranfileOpenFailureAbends() {
        tranIo.fail("open", "37");
        assertAbend("ERROR OPENING TRANSACTION FILE", "FILE STATUS IS: NNNN0037");
    }

    @Test
    void xrefOpenFailureAbends() {
        xrefIo.fail("open", "35");
        assertAbend("ERROR OPENING CROSS REF FILE", "FILE STATUS IS: NNNN0035");
    }

    @Test
    void rejectsOpenFailureAbends() {
        rejsIo.fail("open", "34");
        assertAbend("ERROR OPENING DALY REJECTS FILE", "FILE STATUS IS: NNNN0034");
    }

    @Test
    void accountOpenFailureAbends() {
        acctIo.fail("open", "35");
        assertAbend("ERROR OPENING ACCOUNT MASTER FILE", "FILE STATUS IS: NNNN0035");
    }

    @Test
    void tcatbalOpenFailureAbends() {
        tcatIo.fail("open", "35");
        assertAbend("ERROR OPENING TRANSACTION BALANCE FILE", "FILE STATUS IS: NNNN0035");
    }

    @Test
    void dalytranReadErrorAbends() {
        dalyIn.fail("read", "30");
        assertAbend("ERROR READING DALYTRAN FILE", "FILE STATUS IS: NNNN0030");
    }

    @Test
    void tcatbalReadErrorOtherThanNotFoundAbends() {
        tcatIo.fail("read", "92");
        daily.add(daily("T1", "1.00"));
        assertAbend("ERROR READING TRANSACTION BALANCE FILE", "FILE STATUS IS: NNNN9050");
    }

    @Test
    void tcatbalReadInvalidKeyOtherThan23AbendsAfterCreatingMessage() {
        tcatIo.fail("read", "21");
        daily.add(daily("T1", "1.00"));
        assertAbend("ERROR READING TRANSACTION BALANCE FILE", "FILE STATUS IS: NNNN0021");
        assertTrue(sysout.contains("TCATBAL record not found for key : 00000000042010005.. Creating."));
    }

    @Test
    void tcatbalWriteErrorAbends() {
        tcatIo.fail("write", "22");
        daily.add(daily("T1", "1.00"));
        assertAbend("ERROR WRITING TRANSACTION BALANCE FILE", "FILE STATUS IS: NNNN0022");
    }

    @Test
    void tcatbalRewriteErrorAbends() {
        tcat.load(List.of("000000000420100050000001000{" + " ".repeat(22)));
        tcatIo.fail("rewrite", "23");
        daily.add(daily("T1", "1.00"));
        assertAbend("ERROR REWRITING TRANSACTION BALANCE FILE", "FILE STATUS IS: NNNN0023");
    }

    @Test
    void duplicateTransactionIdAbends() {
        daily.add(daily("T1", "1.00"));
        daily.add(daily("T1", "2.00"));
        assertAbend("ERROR WRITING TO TRANSACTION FILE", "FILE STATUS IS: NNNN0022");
        assertEquals(1, tran.records().size());
    }

    @Test
    void rejectWriteErrorAbends() {
        rejsIo.fail("write", "34");
        daily.add(daily("T1", "0000000000000000", "1.00", "2022-06-10 19:27:53.000000"));
        assertAbend("ERROR WRITING TO REJECTS FILE", "FILE STATUS IS: NNNN0034");
    }

    @Test
    void closeFailuresAbend() {
        dalyIn.fail("close", "42");
        assertAbend("ERROR CLOSING DALYTRAN FILE", "FILE STATUS IS: NNNN0042");
    }

    @Test
    void tranfileCloseFailureAbends() {
        tranIo.fail("close", "30");
        assertAbend("ERROR CLOSING TRANSACTION FILE", "FILE STATUS IS: NNNN0030");
    }

    @Test
    void xrefCloseFailureAbends() {
        xrefIo.fail("close", "30");
        assertAbend("ERROR CLOSING CROSS REF FILE", "FILE STATUS IS: NNNN0030");
    }

    @Test
    void rejectsCloseFailureDisplaysXrefStatusAsInCobol() {
        rejsIo.fail("close", "30");
        assertAbend("ERROR CLOSING DAILY REJECTS FILE", "FILE STATUS IS: NNNN0000");
    }

    @Test
    void accountCloseFailureAbends() {
        acctIo.fail("close", "30");
        assertAbend("ERROR CLOSING ACCOUNT FILE", "FILE STATUS IS: NNNN0030");
    }

    @Test
    void tcatbalCloseFailureAbends() {
        tcatIo.fail("close", "30");
        assertAbend("ERROR CLOSING TRANSACTION BALANCE FILE", "FILE STATUS IS: NNNN0030");
    }

    // ---- 9910-DISPLAY-IO-STATUS, Z-GET-DB2-FORMAT-TIMESTAMP

    @Test
    void ioStatusFormatting() {
        assertEquals("0035", Cbtrn02c.formatIoStatus04("35"));
        assertEquals("9050", Cbtrn02c.formatIoStatus04("92"));
        assertEquals("9065", Cbtrn02c.formatIoStatus04("9A"));
        assertEquals("3066", Cbtrn02c.formatIoStatus04("3B"));
    }

    @Test
    void db2TimestampUsesAsciiDigitsWhateverTheDefaultLocale() {
        java.util.Locale saved = java.util.Locale.getDefault();
        try {
            java.util.Locale.setDefault(java.util.Locale.forLanguageTag("ar-EG"));
            assertEquals(PROC_TS, program().getDb2FormatTimestamp());
        } finally {
            java.util.Locale.setDefault(saved);
        }
    }

    @Test
    void db2TimestampFormat() {
        assertEquals(PROC_TS, program().getDb2FormatTimestamp());
    }

    static String pad(String s) {
        return s + " ".repeat(76 - s.length());
    }
}
