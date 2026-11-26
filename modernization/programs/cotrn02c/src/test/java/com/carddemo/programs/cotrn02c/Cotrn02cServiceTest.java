package com.carddemo.programs.cotrn02c;

import com.carddemo.programs.cotrn02c.copybook.CardDemoCommarea;
import com.carddemo.programs.cotrn02c.copybook.CardXrefRecord;
import com.carddemo.programs.cotrn02c.copybook.Cotrn2aMap.Field;
import com.carddemo.programs.cotrn02c.copybook.Dfhaid;
import com.carddemo.programs.cotrn02c.copybook.Dfhbmsca;
import com.carddemo.programs.cotrn02c.copybook.Pic;
import com.carddemo.programs.cotrn02c.copybook.TranRecord;
import com.carddemo.programs.cotrn02c.gateway.CicsResp;
import com.carddemo.programs.cotrn02c.gateway.InMemoryTransactFile.Command;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;
import java.util.function.UnaryOperator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cotrn02cServiceTest extends TestSupport {

    @Nested
    class MainPara {

        @Test
        void eibcalenZeroXctlsToSignon() {
            CicsOutcome o = service().execute(null, Dfhaid.DFHENTER, null);
            CicsOutcome.Xctl x = assertInstanceOf(CicsOutcome.Xctl.class, o);
            assertEquals("COSGN00C", x.program());
            assertEquals("CT02", x.commarea().getFromTranId());
            assertEquals("COTRN02C", x.commarea().getFromProgram());
            assertTrue(x.commarea().isPgmEnter());
            assertEquals('N', x.commarea().getCt02NextPageFlg());
        }

        @Test
        void firstEntrySendsEmptyMapWithHeaderAndCursorOnAccount() {
            CicsOutcome o = service().execute(firstEntryCommarea(""), Dfhaid.DFHENTER, null);
            CicsOutcome.SendMap s = assertInstanceOf(CicsOutcome.SendMap.class, o);
            assertEquals("COTRN02", s.mapset());
            assertEquals("COTRN2A", s.map());
            assertEquals("CT02", s.transId());
            assertTrue(s.commarea().isPgmReenter());
            Cotrn2aResponse r = s.screen();
            assertEquals("CT02", r.trnName());
            assertEquals("COTRN02C", r.pgmName());
            assertEquals("11/26/26", r.curDate());
            assertEquals("09:05:07", r.curTime());
            assertEquals("      AWS Mainframe Modernization       ", r.title01());
            assertEquals("              CardDemo                  ", r.title02());
            assertEquals(Pic.lowValues(11), r.acctId());
            assertEquals(Pic.lowValues(1), r.confirm());
            assertEquals(Pic.spaces(78), r.errMsg());
            assertEquals(Field.ACTIDIN, r.cursorField());
            assertEquals(Dfhbmsca.DFHDFCOL, r.errMsgColor());
        }

        @Test
        void shortCommareaIsSpacePaddedAndTreatedAsFirstEntry() {
            CicsOutcome o = service().execute("CM00COMEN01C", Dfhaid.DFHENTER, null);
            CicsOutcome.SendMap s = assertInstanceOf(CicsOutcome.SendMap.class, o);
            assertEquals("", s.screen().message());
            assertEquals("COMEN01C", s.commarea().getFromProgram());
            assertEquals(CardDemoCommarea.LENGTH, s.commarea().toRecord().length());
        }

        @Test
        void firstEntryWithSelectedTransactionLooksUpCardThenStopsAtFirstEmptyField() {
            Cotrn2aResponse r = screen(service().execute(firstEntryCommarea(CARD), Dfhaid.DFHENTER, null));
            assertEquals(CARD, r.cardNum());
            assertEquals(ACCT, r.acctId());
            assertEquals("Type CD can NOT be empty...", r.message());
            assertEquals(Field.ACTIDIN, r.cursorField());
        }

        @Test
        void pf3WithoutFromProgramReturnsToMenu() {
            CicsOutcome o = service().execute(reenterCommarea(""), Dfhaid.DFHPF3, Cotrn2aRequest.EMPTY);
            CicsOutcome.Xctl x = assertInstanceOf(CicsOutcome.Xctl.class, o);
            assertEquals("COMEN01C", x.program());
            assertEquals("COTRN02C", x.commarea().getFromProgram());
            assertEquals('0', x.commarea().getPgmContext());
        }

        @Test
        void pf3ReturnsToCallingProgram() {
            CicsOutcome o = service().execute(reenterCommarea("COTRN00C"), Dfhaid.DFHPF3, Cotrn2aRequest.EMPTY);
            assertEquals("COTRN00C", assertInstanceOf(CicsOutcome.Xctl.class, o).program());
        }

        @Test
        void pf4ClearsAllInputFields() {
            Cotrn2aResponse r = screen(service().execute(reenterCommarea("COTRN00C"), Dfhaid.DFHPF4,
                    valid().build()));
            assertEquals(Pic.spaces(11), r.acctId());
            assertEquals(Pic.spaces(16), r.cardNum());
            assertEquals(Pic.spaces(12), r.amount());
            assertEquals(Pic.spaces(60), r.desc());
            assertEquals(Pic.spaces(1), r.confirm());
            assertEquals("", r.message());
            assertEquals(Field.ACTIDIN, r.cursorField());
        }

        @Test
        void otherKeyShowsInvalidKeyMessageAndKeepsInput() {
            Cotrn2aResponse r = screen(service().execute(reenterCommarea("COTRN00C"), Dfhaid.DFHPF7,
                    valid().build()));
            assertEquals("Invalid key pressed. Please see below...", r.message());
            assertEquals(Pic.x("Invalid key pressed. Please see below...", 78), r.errMsg());
            assertEquals(ACCT, r.acctId());
            assertEquals(Pic.lowValues(16), r.cardNum());
        }
    }

    @Nested
    class ValidateInputKeyFields {

        @Test
        void neitherAccountNorCard() {
            Cotrn2aResponse r = screen(enter(valid().acctId(null).build()));
            assertEquals("Account or Card Number must be entered...", r.message());
            assertEquals(Field.ACTIDIN, r.cursorField());
        }

        @Test
        void spacesAreTreatedAsNotEntered() {
            Cotrn2aResponse r = screen(enter(valid().acctId("           ").cardNum("    ").build()));
            assertEquals("Account or Card Number must be entered...", r.message());
        }

        @Test
        void accountMustBeAllDigits() {
            Cotrn2aResponse r = screen(enter(valid().acctId("50").build()));
            assertEquals("Account ID must be Numeric...", r.message());
            assertEquals(Field.ACTIDIN, r.cursorField());
        }

        @Test
        void accountNotFound() {
            assertEquals("Account ID NOT found...", screen(enter(valid().acctId("00000000099").build())).message());
        }

        @Test
        void accountLookupPopulatesCardNumber() {
            Cotrn2aResponse r = screen(enter(valid().confirm(null).build()));
            assertEquals(CARD, r.cardNum());
            assertEquals("Confirm to add this transaction...", r.message());
        }

        @Test
        void accountWithSeveralCardsIsDupkeyOnAixPath() {
            xref.put(new CardXrefRecord("0500024453765741", 50, 50));
            Cotrn2aResponse r = screen(enter(valid().build()));
            assertEquals("Unable to lookup Acct in XREF AIX file...", r.message());
            assertEquals("RESP:000000015REAS:000000000", displayed.get(0));
        }

        @Test
        void accountTakesPrecedenceOverCard() {
            Cotrn2aResponse r = screen(enter(valid().cardNum("9999999999999999").confirm("N").build()));
            assertEquals(CARD, r.cardNum());
            assertEquals("Confirm to add this transaction...", r.message());
        }

        @Test
        void cardMustBeAllDigits() {
            Cotrn2aResponse r = screen(enter(valid().acctId(null).cardNum("05000244").build()));
            assertEquals("Card Number must be Numeric...", r.message());
            assertEquals(Field.CARDNIN, r.cursorField());
        }

        @Test
        void cardNotFound() {
            Cotrn2aResponse r = screen(enter(valid().acctId(null).cardNum("1111222233334444").build()));
            assertEquals("Card Number NOT found...", r.message());
            assertEquals(Field.CARDNIN, r.cursorField());
        }

        @Test
        void cardLookupPopulatesAccount() {
            Cotrn2aResponse r = screen(enter(valid().acctId(null).cardNum(CARD).confirm("n").build()));
            assertEquals(ACCT, r.acctId());
            assertEquals("Confirm to add this transaction...", r.message());
        }

        @Test
        void cardReadErrorOther() {
            xref.injectCardRead(CicsResp.IOERR);
            Cotrn2aResponse r = screen(enter(valid().acctId(null).cardNum(CARD).build()));
            assertEquals("Unable to lookup Card # in XREF file...", r.message());
            assertEquals("RESP:000000017REAS:000000000", displayed.get(0));
        }
    }

    @Nested
    class ValidateInputDataFields {

        static Cotrn2aRequest.Builder blank(Cotrn2aRequest.Builder b, String field) {
            return switch (field) {
                case "type" -> b.typeCd(null);
                case "cat" -> b.catCd("    ");
                case "source" -> b.source(null);
                case "desc" -> b.desc("");
                case "amount" -> b.amount(null);
                case "orig" -> b.origDate(null);
                case "proc" -> b.procDate(" ");
                case "mid" -> b.merchantId(null);
                case "mname" -> b.merchantName(null);
                case "mcity" -> b.merchantCity(null);
                case "mzip" -> b.merchantZip(null);
                default -> throw new IllegalArgumentException(field);
            };
        }

        @ParameterizedTest
        @CsvSource({
            "type,   Type CD can NOT be empty...,       TTYPCD",
            "cat,    Category CD can NOT be empty...,   TCATCD",
            "source, Source can NOT be empty...,        TRNSRC",
            "desc,   Description can NOT be empty...,   TDESC",
            "amount, Amount can NOT be empty...,        TRNAMT",
            "orig,   Orig Date can NOT be empty...,     TORIGDT",
            "proc,   Proc Date can NOT be empty...,     TPROCDT",
            "mid,    Merchant ID can NOT be empty...,   MID",
            "mname,  Merchant Name can NOT be empty..., MNAME",
            "mcity,  Merchant City can NOT be empty..., MCITY",
            "mzip,   Merchant Zip can NOT be empty...,  MZIP"
        })
        void mandatoryFields(String field, String message, Field cursor) {
            Cotrn2aResponse r = screen(enter(blank(valid(), field).build()));
            assertEquals(message, r.message());
            assertEquals(cursor, r.cursorField());
        }

        @Test
        void emptyChecksRunInMapOrderTypeBeforeMerchantZip() {
            Cotrn2aResponse r = screen(enter(valid().typeCd(null).merchantZip(null).build()));
            assertEquals("Type CD can NOT be empty...", r.message());
        }

        @Test
        void mixedSpacesAndLowValuesIsNotEmpty() {
            Cotrn2aResponse r = screen(enter(valid().typeCd(" \u0000").build()));
            assertEquals("Type CD must be Numeric...", r.message());
        }

        @ParameterizedTest
        @CsvSource({
            "'1 ', 0001, Type CD must be Numeric...,     TTYPCD",
            "AB,   0001, Type CD must be Numeric...,     TTYPCD",
            "01,   '1',  Category CD must be Numeric..., TCATCD",
            "01,   00X1, Category CD must be Numeric..., TCATCD"
        })
        void typeAndCategoryMustBeNumeric(String type, String cat, String message, Field cursor) {
            Cotrn2aResponse r = screen(enter(valid().typeCd(type).catCd(cat).build()));
            assertEquals(message, r.message());
            assertEquals(cursor, r.cursorField());
        }

        @ParameterizedTest
        @CsvSource({
            "'12.50'", "'00000012.50 '", "'*00000012.50'", "'+0000001A.50'", "'+00000012,50'",
            "'+00000012.5 '", "'+12345678'", "'-0000001.250'"
        })
        void amountMustMatchSignedPicture(String amount) {
            Cotrn2aResponse r = screen(enter(valid().amount(amount).build()));
            assertEquals("Amount should be in format -99999999.99", r.message());
            assertEquals(Field.TRNAMT, r.cursorField());
        }

        @ParameterizedTest
        @CsvSource({
            "2022/06/10", "22-06-10", "2022-6-10", "2022-06-1", "YYYY-MM-DD"
        })
        void origDateFormat(String date) {
            Cotrn2aResponse r = screen(enter(valid().origDate(date).build()));
            assertEquals("Orig Date should be in format YYYY-MM-DD", r.message());
            assertEquals(Field.TORIGDT, r.cursorField());
        }

        @Test
        void procDateFormat() {
            Cotrn2aResponse r = screen(enter(valid().procDate("20220611").build()));
            assertEquals("Proc Date should be in format YYYY-MM-DD", r.message());
            assertEquals(Field.TPROCDT, r.cursorField());
        }

        @ParameterizedTest
        @CsvSource({"2023-02-29", "2022-13-01", "2022-04-31", "2022-00-10", "2022-01-00", "1582-10-00", "1500-02-30"})
        void origDateMustBeACalendarDate(String date) {
            Cotrn2aResponse r = screen(enter(valid().origDate(date).build()));
            assertEquals("Orig Date - Not a valid date...", r.message());
            assertEquals(Field.TORIGDT, r.cursorField());
        }

        @Test
        void procDateMustBeACalendarDate() {
            Cotrn2aResponse r = screen(enter(valid().procDate("2022-02-30").build()));
            assertEquals("Proc Date - Not a valid date...", r.message());
            assertEquals(Field.TPROCDT, r.cursorField());
        }

        @Test
        void leapDayIsValid() {
            Cotrn2aResponse r = screen(enter(valid().origDate("2024-02-29").confirm(null).build()));
            assertEquals("Confirm to add this transaction...", r.message());
        }

        @Test
        void dateBeforeLilianEpochIsAcceptedBecauseMsg2513IsIgnored() {
            Cotrn2aResponse r = screen(enter(valid().origDate("1500-01-01").procDate("1582-10-14").confirm(null)
                    .build()));
            assertEquals("Confirm to add this transaction...", r.message());
        }

        @Test
        void merchantIdMustBeNumeric() {
            Cotrn2aResponse r = screen(enter(valid().merchantId("8000").build()));
            assertEquals("Merchant ID must be Numeric...", r.message());
            assertEquals(Field.MID, r.cursorField());
        }

        @Test
        void merchantIdIsCheckedAfterDates() {
            Cotrn2aResponse r = screen(enter(valid().merchantId("ABC").origDate("2022-02-30").build()));
            assertEquals("Orig Date - Not a valid date...", r.message());
        }

        @ParameterizedTest
        @CsvSource({
            "-00000012.50, -00000012.50",
            "+99999999.99, +99999999.99",
            "-00000000.00, +00000000.00"
        })
        void amountIsReformattedThroughEditedPicture(String in, String out) {
            Cotrn2aResponse r = screen(enter(valid().amount(in).confirm(null).build()));
            assertEquals(out, r.amount());
        }
    }

    @Nested
    class ProcessEnterKeyConfirm {

        @ParameterizedTest
        @CsvSource(value = {"N", "n", "' '", "NULL"}, nullValues = "NULL")
        void notConfirmed(String confirm) {
            Cotrn2aResponse r = screen(enter(valid().confirm(confirm).build()));
            assertEquals("Confirm to add this transaction...", r.message());
            assertEquals(Field.CONFIRM, r.cursorField());
            assertTrue(transact.records().isEmpty());
        }

        @Test
        void invalidConfirmValue() {
            Cotrn2aResponse r = screen(enter(valid().confirm("X").build()));
            assertEquals("Invalid value. Valid values are (Y/N)...", r.message());
            assertEquals(Field.CONFIRM, r.cursorField());
        }

        @Test
        void inputIsEchoedOnError() {
            Cotrn2aResponse r = screen(enter(valid().confirm("N").build()));
            assertEquals("Purchase at Test Store", r.desc().trim());
            assertEquals("2022-06-10", r.origDate());
            assertEquals("Test Store" + Pic.spaces(20), r.merchantName());
        }
    }

    @Nested
    class AddTransaction {

        private TranRecord existing(String id, String filler) {
            TranRecord t = new TranRecord();
            t.setTranId(id);
            t.setTypeCd("02");
            t.setFiller(filler);
            return t;
        }

        @Test
        void writesNextIdAndShowsSuccessInGreen() {
            transact.put(existing("0000000000000041", ""));
            transact.put(existing("0000000000000300", ""));
            Cotrn2aResponse r = screen(enter(valid().build()));
            assertEquals("Transaction added successfully.  Your Tran ID is 0000000000000301.", r.message());
            assertEquals(Dfhbmsca.DFHGREEN, r.errMsgColor());
            assertEquals(Field.ACTIDIN, r.cursorField());
            assertEquals(Pic.spaces(11), r.acctId());
            assertEquals(Pic.spaces(1), r.confirm());
            assertEquals(java.util.List.of("STARTBR", "READPREV", "ENDBR", "WRITE"), transact.journal());
            assertFalse(transact.isBrowsing());

            TranRecord w = transact.records().get("0000000000000301");
            assertEquals("01", w.getTypeCd());
            assertEquals(1, w.getCatCd());
            assertEquals(Pic.x("POS TERM", 10), w.getSource());
            assertEquals(Pic.x("Purchase at Test Store", 100), w.getDesc());
            assertEquals(new BigDecimal("12.50"), w.getAmt());
            assertEquals(CARD, w.getCardNum());
            assertEquals(800000000L, w.getMerchantId());
            assertEquals(Pic.x("Test Store", 50), w.getMerchantName());
            assertEquals(Pic.x("Sydney", 50), w.getMerchantCity());
            assertEquals(Pic.x("2000", 10), w.getMerchantZip());
            assertEquals(Pic.x("2022-06-10", 26), w.getOrigTs());
            assertEquals(Pic.x("2022-06-11", 26), w.getProcTs());
        }

        @Test
        void negativeAmountIsStoredSigned() {
            screen(enter(valid().amount("-00001234.56").build()));
            TranRecord w = transact.records().firstEntry().getValue();
            assertEquals(new BigDecimal("-1234.56"), w.getAmt());
            assertEquals("0000012345O", w.toRecord().substring(132, 143));
        }

        @Test
        void emptyFileStartsAtOne() {
            Cotrn2aResponse r = screen(enter(valid().build()));
            assertEquals("Transaction added successfully.  Your Tran ID is 0000000000000001.", r.message());
        }

        @Test
        void initializeKeepsFillerOfLastRecordRead() {
            transact.put(existing("0000000000000007", "LEGACYFILLER"));
            screen(enter(valid().build()));
            assertEquals(Pic.x("LEGACYFILLER", 20), transact.records().get("0000000000000008").getFiller());
        }

        @Test
        void highestIdWrapsOnAddWithoutSizeError() {
            transact.put(existing("9999999999999999", ""));
            transact.put(existing("0000000000000000", ""));
            Cotrn2aResponse r = screen(enter(valid().build()));
            assertEquals("Tran ID already exist...", r.message());
            assertEquals(Field.ACTIDIN, r.cursorField());
        }

        @Test
        void duplicateKeyOnWrite() {
            transact.inject(Command.WRITE, CicsResp.DUPKEY);
            assertEquals("Tran ID already exist...", screen(enter(valid().build())).message());
        }

        @Test
        void otherWriteError() {
            transact.inject(Command.WRITE, CicsResp.NOSPACE);
            Cotrn2aResponse r = screen(enter(valid().build()));
            assertEquals("Unable to Add Transaction...", r.message());
            assertEquals("RESP:000000018REAS:000000000", displayed.get(0));
            assertEquals(ACCT, r.acctId());
        }

        @Test
        void startBrowseNotFound() {
            transact.inject(Command.STARTBR, CicsResp.NOTFND);
            assertEquals("Transaction ID NOT found...", screen(enter(valid().build())).message());
            assertTrue(displayed.isEmpty());
        }

        @Test
        void startBrowseOtherError() {
            transact.inject(Command.STARTBR, CicsResp.NOTOPEN);
            assertEquals("Unable to lookup Transaction...", screen(enter(valid().build())).message());
            assertEquals(1, displayed.size());
        }

        @Test
        void readPrevError() {
            transact.inject(Command.READPREV, CicsResp.IOERR);
            Cotrn2aResponse r = screen(enter(valid().build()));
            assertEquals("Unable to lookup Transaction...", r.message());
            assertEquals(java.util.List.of("STARTBR", "READPREV"), transact.journal());
        }
    }

    @Nested
    class CopyLastTranData {

        private CicsOutcome pf5(Cotrn2aRequest req) {
            return service().execute(reenterCommarea("COTRN00C"), Dfhaid.DFHPF5, req);
        }

        private void last(UnaryOperator<TranRecord> f) {
            TranRecord t = new TranRecord();
            t.setTranId("0000000000000500");
            t.setTypeCd("03");
            t.setCatCd(2);
            t.setSource("OPERATOR");
            t.setDesc("X".repeat(100));
            t.setAmt(new BigDecimal("-45.10"));
            t.setMerchantId(123456789);
            t.setMerchantName("N".repeat(50));
            t.setMerchantCity("C".repeat(50));
            t.setMerchantZip("12345");
            t.setOrigTs("2022-06-10 19:27:53.000000");
            t.setProcTs("2022-06-12 10:00:00.000000");
            transact.put(f.apply(t));
        }

        @Test
        void copiesLastRecordIntoScreenWithTruncation() {
            last(t -> t);
            Cotrn2aResponse r = screen(pf5(Cotrn2aRequest.builder().acctId(ACCT).build()));
            assertEquals("Confirm to add this transaction...", r.message());
            assertEquals("03", r.typeCd());
            assertEquals("0002", r.catCd());
            assertEquals(Pic.x("OPERATOR", 10), r.source());
            assertEquals("-00000045.10", r.amount());
            assertEquals("X".repeat(60), r.desc());
            assertEquals("2022-06-10", r.origDate());
            assertEquals("2022-06-12", r.procDate());
            assertEquals("123456789", r.merchantId());
            assertEquals("N".repeat(30), r.merchantName());
            assertEquals("C".repeat(25), r.merchantCity());
            assertEquals(Pic.x("12345", 10), r.merchantZip());
            assertEquals(CARD, r.cardNum());
        }

        @Test
        void amountAboveEightIntegerDigitsIsTruncatedInEditedField() {
            last(t -> {
                t.setAmt(new BigDecimal("123456789.01"));
                return t;
            });
            assertEquals("+23456789.01", screen(pf5(Cotrn2aRequest.builder().acctId(ACCT).build())).amount());
        }

        @Test
        void copyWithConfirmAddsDuplicateUnderNextId() {
            last(t -> t);
            Cotrn2aResponse r = screen(pf5(Cotrn2aRequest.builder().acctId(ACCT).confirm("Y").build()));
            assertEquals("Transaction added successfully.  Your Tran ID is 0000000000000501.", r.message());
            TranRecord w = transact.records().get("0000000000000501");
            assertEquals(new BigDecimal("-45.10"), w.getAmt());
            assertEquals(Pic.x("X".repeat(60), 100), w.getDesc());
            assertEquals(Pic.x("2022-06-10", 26), w.getOrigTs());
        }

        @Test
        void keyFieldsValidatedFirst() {
            last(t -> t);
            Cotrn2aResponse r = screen(pf5(Cotrn2aRequest.EMPTY));
            assertEquals("Account or Card Number must be entered...", r.message());
            assertTrue(transact.journal().isEmpty());
        }

        @Test
        void emptyFileLeavesFieldsEmpty() {
            Cotrn2aResponse r = screen(pf5(Cotrn2aRequest.builder().acctId(ACCT).build()));
            assertEquals("Type CD can NOT be empty...", r.message());
        }
    }
}
