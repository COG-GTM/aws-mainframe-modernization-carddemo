package com.carddemo.programs.cbact04c;

import com.carddemo.programs.cbact04c.copybook.AccountRecord;
import com.carddemo.programs.cbact04c.copybook.CardXrefRecord;
import com.carddemo.programs.cbact04c.copybook.DisGroupRecord;
import com.carddemo.programs.cbact04c.copybook.TranCatBalRecord;
import com.carddemo.programs.cbact04c.copybook.TranRecord;
import com.carddemo.programs.cbact04c.io.InMemoryIndexedFile;
import com.carddemo.programs.cbact04c.io.InMemorySequentialInputFile;
import com.carddemo.programs.cbact04c.io.InMemorySequentialOutputFile;
import com.carddemo.programs.cbact04c.io.IndexedFile;
import com.carddemo.programs.cbact04c.io.OpenMode;
import com.carddemo.programs.cbact04c.io.ReadResult;
import com.carddemo.programs.cbact04c.io.SequentialInputFile;
import com.carddemo.programs.cbact04c.io.SequentialOutputFile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cbact04cTest {

    static final Clock CLOCK = Clock.fixed(Instant.parse("2022-07-18T10:15:30.123Z"), ZoneOffset.UTC);
    static final String TS = "2022-07-18-10.15.30.120000";
    static final String PARM = "2022071800";

    List<TranCatBalRecord> tcatbal;
    List<CardXrefRecord> xrefs;
    List<DisGroupRecord> discgrps;
    List<AccountRecord> accounts;
    List<String> out;

    InMemoryIndexedFile<Long, AccountRecord> accountFile;
    InMemorySequentialOutputFile<TranRecord> transactFile;

    static AccountRecord account(long id, String bal, String groupId) {
        return new AccountRecord(id, "Y", new BigDecimal(bal), new BigDecimal("5000.00"), new BigDecimal("1000.00"),
                "2014-11-20", "2025-05-20", "2025-05-20", new BigDecimal("300.00"), new BigDecimal("-75.50"),
                "A000000000", groupId, "");
    }

    static TranCatBalRecord tcb(long acct, String type, int cat, String bal) {
        return new TranCatBalRecord(acct, type, cat, new BigDecimal(bal));
    }

    static DisGroupRecord dg(String group, String type, int cat, String rate) {
        return new DisGroupRecord(group, type, cat, new BigDecimal(rate));
    }

    @BeforeEach
    void setUp() {
        tcatbal = new ArrayList<>();
        xrefs = new ArrayList<>(List.of(
                new CardXrefRecord("4111111111111111", 1, 1),
                new CardXrefRecord("4222222222222222", 2, 2),
                new CardXrefRecord("4333333333333333", 3, 3)));
        discgrps = new ArrayList<>(List.of(
                dg("GOLD", "01", 1, "15.00"),
                dg("GOLD", "01", 2, "24.99"),
                dg("GOLD", "02", 1, "0.00"),
                dg("DEFAULT", "01", 1, "18.00"),
                dg("DEFAULT", "03", 1, "12.00")));
        accounts = new ArrayList<>(List.of(
                account(1, "1000.00", "GOLD"),
                account(2, "500.00", "GOLD"),
                account(3, "250.00", "SILVER")));
        out = new ArrayList<>();
    }

    Cbact04cFiles files() {
        accountFile = new InMemoryIndexedFile<>(AccountRecord::acctId, accounts);
        transactFile = new InMemorySequentialOutputFile<>();
        return new Cbact04cFiles(
                new InMemorySequentialInputFile<>(tcatbal),
                new InMemoryIndexedFile<>(CardXrefRecord::xrefAcctId, xrefs),
                new InMemoryIndexedFile<>(DisGroupRecord::key, discgrps),
                accountFile,
                transactFile);
    }

    int run() {
        return run(files());
    }

    int run(Cbact04cFiles f) {
        return new Cbact04c(f, CLOCK, out::add).run(PARM);
    }

    AbendException abend(Cbact04cFiles f) {
        return assertThrows(AbendException.class, () -> run(f));
    }

    @Nested
    class MainLoop {

        @Test
        void emptyCategoryBalanceFileWritesNothingAndReturnsZero() {
            assertEquals(0, run());
            assertEquals(List.of("START OF EXECUTION OF PROGRAM CBACT04C", "END OF EXECUTION OF PROGRAM CBACT04C"), out);
            assertTrue(transactFile.written().isEmpty());
            assertEquals(accounts, accountFile.records());
        }

        @Test
        void displaysEachCategoryBalanceRecordImage() {
            tcatbal.add(tcb(1, "01", 1, "100.00"));
            run();
            assertEquals("000000000010100010000001000{                      ", out.get(1));
        }

        @Test
        void accountIsRewrittenWhenAccountNumberChangesWithTotalInterestAndCycleTotalsZeroed() {
            tcatbal.add(tcb(1, "01", 1, "1000.00"));   // 1000 * 15.00 / 1200 = 12.50
            tcatbal.add(tcb(1, "01", 2, "123.45"));    // 123.45 * 24.99 / 1200 = 2.5708... -> 2.57
            tcatbal.add(tcb(2, "01", 1, "10.00"));
            run();
            AccountRecord a1 = accountFile.get(1L);
            assertEquals(new BigDecimal("1015.07"), a1.acctCurrBal());
            assertEquals(new BigDecimal("0.00"), a1.acctCurrCycCredit());
            assertEquals(new BigDecimal("0.00"), a1.acctCurrCycDebit());
            assertEquals(new BigDecimal("5000.00"), a1.acctCreditLimit());
        }

        @Test
        void lastAccountGroupIsNeverRewrittenAsInCobol() {
            tcatbal.add(tcb(1, "01", 1, "1000.00"));
            tcatbal.add(tcb(2, "01", 1, "1000.00"));
            run();
            assertEquals(new BigDecimal("1012.50"), accountFile.get(1L).acctCurrBal());
            assertEquals(account(2, "500.00", "GOLD"), accountFile.get(2L));
            assertEquals(2, transactFile.written().size());
        }

        @Test
        void singleAccountRunUpdatesNoAccount() {
            tcatbal.add(tcb(1, "01", 1, "1000.00"));
            run();
            assertEquals(accounts, accountFile.records());
            assertEquals(1, transactFile.written().size());
        }

        @Test
        void zeroRateSkipsInterestAndTransactionButAccountStillRewritten() {
            tcatbal.add(tcb(1, "02", 1, "1000.00"));   // GOLD/02/0001 rate 0.00
            tcatbal.add(tcb(2, "01", 1, "1.00"));
            run();
            assertEquals(1, transactFile.written().size());
            assertEquals("Int. for a/c 00000000002", transactFile.written().get(0).tranDesc().trim());
            AccountRecord a1 = accountFile.get(1L);
            assertEquals(new BigDecimal("1000.00"), a1.acctCurrBal());
            assertEquals(new BigDecimal("0.00"), a1.acctCurrCycCredit());
            assertEquals(new BigDecimal("0.00"), a1.acctCurrCycDebit());
        }

        @Test
        void totalInterestResetsPerAccount() {
            tcatbal.add(tcb(1, "01", 1, "1200.00"));   // 15.00
            tcatbal.add(tcb(2, "01", 1, "1200.00"));   // 15.00, must not carry account 1's total
            tcatbal.add(tcb(3, "01", 1, "0.00"));
            run();
            assertEquals(new BigDecimal("1015.00"), accountFile.get(1L).acctCurrBal());
            assertEquals(new BigDecimal("515.00"), accountFile.get(2L).acctCurrBal());
        }

        @Test
        void negativeInterestCreditsTheBalance() {
            tcatbal.add(tcb(1, "01", 1, "-10.01"));    // -0.125125 -> -0.12
            tcatbal.add(tcb(2, "01", 1, "0.00"));
            run();
            assertEquals(new BigDecimal("-0.12"), transactFile.written().get(0).tranAmt());
            assertEquals(new BigDecimal("999.88"), accountFile.get(1L).acctCurrBal());
        }
    }

    @Nested
    class ComputeInterest1300 {

        @ParameterizedTest
        @CsvSource({
                "1000.00, 15.00, 12.50",
                "123.45, 24.99, 2.57",
                "123.45, 17.99, 1.85",
                "-10.01, 15.00, -0.12",
                "0.79, 15.00, 0.00",
                "999999999.99, 9999.99, 333324999.91"
        })
        void monthlyInterestIsTruncatedNotRounded(String bal, String rate, String expected) {
            assertEquals(new BigDecimal(expected),
                    Cbact04c.computeMonthlyInterest(new BigDecimal(bal), new BigDecimal(rate)));
        }
    }

    @Nested
    class WriteTx1300B {

        @Test
        void transactionRecordFieldsMatchCobolMoves() {
            tcatbal.add(tcb(1, "01", 1, "1000.00"));
            run();
            TranRecord t = transactFile.written().get(0);
            assertEquals("2022071800000001", t.tranId());
            assertEquals("01", t.tranTypeCd());
            assertEquals(5, t.tranCatCd());
            assertEquals("System    ", t.tranSource());
            assertEquals(String.format("%-100s", "Int. for a/c 00000000001"), t.tranDesc());
            assertEquals(new BigDecimal("12.50"), t.tranAmt());
            assertEquals(0L, t.tranMerchantId());
            assertEquals(" ".repeat(50), t.tranMerchantName());
            assertEquals(" ".repeat(50), t.tranMerchantCity());
            assertEquals(" ".repeat(10), t.tranMerchantZip());
            assertEquals("4111111111111111", t.tranCardNum());
            assertEquals(TS, t.tranOrigTs());
            assertEquals(TS, t.tranProcTs());
        }

        @Test
        void tranIdSuffixIncrementsAcrossAccountsAndCategories() {
            tcatbal.add(tcb(1, "01", 1, "1.00"));
            tcatbal.add(tcb(1, "01", 2, "1.00"));
            tcatbal.add(tcb(2, "01", 1, "1.00"));
            run();
            assertEquals(List.of("2022071800000001", "2022071800000002", "2022071800000003"),
                    transactFile.written().stream().map(TranRecord::tranId).toList());
            assertEquals("4222222222222222", transactFile.written().get(2).tranCardNum());
        }

        @Test
        void parmIsPaddedToTenCharacters() {
            tcatbal.add(tcb(1, "01", 1, "1.00"));
            new Cbact04c(files(), CLOCK, out::add).run("20220718");
            assertEquals("20220718  000001", transactFile.written().get(0).tranId());
        }

        @Test
        void db2TimestampFormat() {
            Cbact04c p = new Cbact04c(files(), Clock.fixed(Instant.parse("2023-01-02T03:04:05.987Z"),
                    ZoneOffset.UTC), out::add);
            assertEquals("2023-01-02-03.04.05.980000", p.getDb2FormatTimestamp());
        }

        @Test
        void db2TimestampUsesAsciiDigitsRegardlessOfDefaultLocale() {
            java.util.Locale saved = java.util.Locale.getDefault();
            try {
                java.util.Locale.setDefault(java.util.Locale.forLanguageTag("ar-EG"));
                Cbact04c p = new Cbact04c(files(), Clock.fixed(Instant.parse("2023-01-02T03:04:05.987Z"),
                        ZoneOffset.UTC), out::add);
                assertEquals("2023-01-02-03.04.05.980000", p.getDb2FormatTimestamp());
            } finally {
                java.util.Locale.setDefault(saved);
            }
        }
    }

    @Nested
    class GetInterestRate1200 {

        @Test
        void missingGroupFallsBackToDefaultGroupWithSameTypeAndCategory() {
            tcatbal.add(tcb(3, "01", 1, "1200.00"));   // SILVER missing -> DEFAULT 18.00
            run();
            assertTrue(out.contains("DISCLOSURE GROUP RECORD MISSING"));
            assertTrue(out.contains("TRY WITH DEFAULT GROUP CODE"));
            assertEquals(new BigDecimal("18.00"), transactFile.written().get(0).tranAmt());
        }

        @Test
        void missingDefaultGroupAbends() {
            tcatbal.add(tcb(3, "02", 9, "1200.00"));
            AbendException e = abend(files());
            assertEquals(999, e.abendCode());
            assertEquals(0, e.timing());
            assertEquals(List.of(
                    "DISCLOSURE GROUP RECORD MISSING",
                    "TRY WITH DEFAULT GROUP CODE",
                    "ERROR READING DEFAULT DISCLOSURE GROUP",
                    "FILE STATUS IS: NNNN0023",
                    "ABENDING PROGRAM"), out.subList(out.size() - 5, out.size()));
        }

        @Test
        void discgrpReadErrorOtherThan23Abends() {
            tcatbal.add(tcb(1, "01", 1, "1.00"));
            Cbact04cFiles f = files();
            Cbact04cFiles broken = new Cbact04cFiles(f.tcatbalFile(), f.xrefFile(),
                    failingIndexed(f.discgrpFile(), "read", "92"), f.accountFile(), f.transactFile());
            abend(broken);
            assertEquals(List.of("ERROR READING DISCLOSURE GROUP FILE", "FILE STATUS IS: NNNN9050",
                    "ABENDING PROGRAM"), out.subList(out.size() - 3, out.size()));
        }
    }

    @Nested
    class Lookups1100And1110 {

        @Test
        void accountNotFoundAbends() {
            tcatbal.add(tcb(99, "01", 1, "1.00"));
            abend(files());
            assertEquals(List.of("ACCOUNT NOT FOUND: 00000000099", "ERROR READING ACCOUNT FILE",
                    "FILE STATUS IS: NNNN0023", "ABENDING PROGRAM"), out.subList(out.size() - 4, out.size()));
        }

        @Test
        void xrefNotFoundAbends() {
            xrefs.removeIf(x -> x.xrefAcctId() == 2);
            tcatbal.add(tcb(2, "01", 1, "1.00"));
            abend(files());
            assertEquals(List.of("ACCOUNT NOT FOUND: 00000000002", "ERROR READING XREF FILE",
                    "FILE STATUS IS: NNNN0023", "ABENDING PROGRAM"), out.subList(out.size() - 4, out.size()));
        }

        @Test
        void duplicateXrefAlternateKeyStatus02Abends() {
            xrefs.add(new CardXrefRecord("4999999999999999", 9, 2, ""));
            tcatbal.add(tcb(2, "01", 1, "1.00"));
            abend(files());
            assertEquals(List.of("ERROR READING XREF FILE", "FILE STATUS IS: NNNN0002", "ABENDING PROGRAM"),
                    out.subList(out.size() - 3, out.size()));
        }
    }

    @Nested
    class FileErrors {

        @ParameterizedTest
        @CsvSource({
                "tcatbal, ERROR OPENING TRANSACTION CATEGORY BALANCE",
                "xref, ERROR OPENING CROSS REF FILE35",
                "discgrp, ERROR OPENING DALY REJECTS FILE",
                "account, ERROR OPENING ACCOUNT MASTER FILE",
                "transact, ERROR OPENING TRANSACTION FILE"
        })
        void openFailureAbendsWithCobolMessage(String file, String message) {
            abend(failing(file, "open", "35"));
            assertEquals(List.of(message, "FILE STATUS IS: NNNN0035", "ABENDING PROGRAM"),
                    out.subList(out.size() - 3, out.size()));
        }

        @ParameterizedTest
        @CsvSource({
                "tcatbal, ERROR CLOSING TRANSACTION BALANCE FILE",
                "xref, ERROR CLOSING CROSS REF FILE",
                "discgrp, ERROR CLOSING DISCLOSURE GROUP FILE",
                "account, ERROR CLOSING ACCOUNT FILE",
                "transact, ERROR CLOSING TRANSACTION FILE"
        })
        void closeFailureAbendsWithCobolMessage(String file, String message) {
            abend(failing(file, "close", "30"));
            assertEquals(List.of(message, "FILE STATUS IS: NNNN0030", "ABENDING PROGRAM"),
                    out.subList(out.size() - 3, out.size()));
        }

        @Test
        void categoryBalanceReadErrorAbends() {
            abend(failing("tcatbal", "read", "30"));
            assertEquals(List.of("ERROR READING TRANSACTION CATEGORY FILE", "FILE STATUS IS: NNNN0030",
                    "ABENDING PROGRAM"), out.subList(out.size() - 3, out.size()));
        }

        @Test
        void accountRewriteErrorAbends() {
            tcatbal.add(tcb(1, "01", 1, "1.00"));
            tcatbal.add(tcb(2, "01", 1, "1.00"));
            abend(failing("account", "rewrite", "22"));
            assertEquals(List.of("ERROR RE-WRITING ACCOUNT FILE", "FILE STATUS IS: NNNN0022",
                    "ABENDING PROGRAM"), out.subList(out.size() - 3, out.size()));
        }

        @Test
        void transactionWriteErrorAbends() {
            tcatbal.add(tcb(1, "01", 1, "1.00"));
            abend(failing("transact", "write", "34"));
            assertEquals(List.of("ERROR WRITING TRANSACTION RECORD", "FILE STATUS IS: NNNN0034",
                    "ABENDING PROGRAM"), out.subList(out.size() - 3, out.size()));
        }
    }

    @Nested
    class DisplayIoStatus9910 {

        @ParameterizedTest
        @CsvSource({
                "00, 0000",
                "23, 0023",
                "35, 0035",
                "9A, 9065",
                "97, 9055",
                "4x, 4120"
        })
        void ioStatusFormatting(String status, String expected) {
            assertEquals(expected, Cbact04c.formatIoStatus(status));
        }
    }

    @Nested
    class AccessModes {

        @Test
        void accountFileOpenedIoAndOthersInput() {
            List<String> modes = new ArrayList<>();
            Cbact04cFiles f = files();
            Cbact04cFiles spying = new Cbact04cFiles(f.tcatbalFile(),
                    recordingOpen(f.xrefFile(), "xref", modes),
                    recordingOpen(f.discgrpFile(), "discgrp", modes),
                    recordingOpen(f.accountFile(), "account", modes),
                    f.transactFile());
            run(spying);
            assertEquals(List.of("xref:INPUT", "discgrp:INPUT", "account:I_O"), modes);
        }
    }

    // ---- fault-injection helpers ----

    Cbact04cFiles failing(String file, String op, String status) {
        Cbact04cFiles f = files();
        return new Cbact04cFiles(
                file.equals("tcatbal") ? failingSeqIn(f.tcatbalFile(), op, status) : f.tcatbalFile(),
                file.equals("xref") ? failingIndexed(f.xrefFile(), op, status) : f.xrefFile(),
                file.equals("discgrp") ? failingIndexed(f.discgrpFile(), op, status) : f.discgrpFile(),
                file.equals("account") ? failingIndexed(f.accountFile(), op, status) : f.accountFile(),
                file.equals("transact") ? failingSeqOut(f.transactFile(), op, status) : f.transactFile());
    }

    static <T> SequentialInputFile<T> failingSeqIn(SequentialInputFile<T> d, String op, String status) {
        return new SequentialInputFile<>() {
            public String open() {
                return op.equals("open") ? status : d.open();
            }

            public ReadResult<T> read() {
                return op.equals("read") ? ReadResult.status(status) : d.read();
            }

            public String close() {
                return op.equals("close") ? status : d.close();
            }
        };
    }

    static <K, T> IndexedFile<K, T> failingIndexed(IndexedFile<K, T> d, String op, String status) {
        return new IndexedFile<>() {
            public String open(OpenMode mode) {
                return op.equals("open") ? status : d.open(mode);
            }

            public ReadResult<T> read(K key) {
                return op.equals("read") ? ReadResult.status(status) : d.read(key);
            }

            public String rewrite(T record) {
                return op.equals("rewrite") ? status : d.rewrite(record);
            }

            public String close() {
                return op.equals("close") ? status : d.close();
            }
        };
    }

    static <T> SequentialOutputFile<T> failingSeqOut(SequentialOutputFile<T> d, String op, String status) {
        return new SequentialOutputFile<>() {
            public String open() {
                return op.equals("open") ? status : d.open();
            }

            public String write(T record) {
                return op.equals("write") ? status : d.write(record);
            }

            public String close() {
                return op.equals("close") ? status : d.close();
            }
        };
    }

    static <K, T> IndexedFile<K, T> recordingOpen(IndexedFile<K, T> d, String name, List<String> modes) {
        return new IndexedFile<>() {
            public String open(OpenMode mode) {
                modes.add(name + ":" + mode);
                return d.open(mode);
            }

            public ReadResult<T> read(K key) {
                return d.read(key);
            }

            public String rewrite(T record) {
                return d.rewrite(record);
            }

            public String close() {
                return d.close();
            }
        };
    }
}
