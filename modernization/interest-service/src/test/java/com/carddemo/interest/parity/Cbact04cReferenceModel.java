package com.carddemo.interest.parity;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A deliberately literal transliteration of COBOL program {@code CBACT04C} — the black-box oracle
 * of the parity harness.
 *
 * <p>This is the "JOBOL" anti-pattern on purpose, and it is the only place in the repository where
 * it is acceptable: one class, methods named after COBOL paragraphs, a shared mutable record area,
 * offsets instead of a domain model. It exists so the idiomatic implementation can be compared
 * against an independent re-reading of the COBOL, and it deliberately shares no production code —
 * it has its own EBCDIC and zoned-decimal handling, so a bug in the production record reader
 * cannot cancel itself out.
 *
 * <p>No mainframe was available for this engagement: the oracle's authority is the static COBOL
 * source, paragraph by paragraph, not a captured production run.
 */
final class Cbact04cReferenceModel {

    /** Outputs of one simulated run of the {@code INTCALC} step. */
    record Output(List<byte[]> transactionRecords, List<byte[]> rewrittenAccountRecords, int recordCount) {
    }

    private static final Charset CP037 = Charset.forName("IBM037");
    private static final int TCATBAL_LENGTH = 50;
    private static final int XREF_LENGTH = 50;
    private static final int ACCOUNT_LENGTH = 300;
    private static final int DISCGRP_LENGTH = 50;
    private static final int TRANSACTION_LENGTH = 350;

    private final List<byte[]> tcatbalRecords;
    private final List<byte[]> xrefRecords;
    private final List<byte[]> accountRecords;
    private final List<byte[]> discgrpRecords;
    private final String parmDate;
    private final String db2Timestamp;

    // WORKING-STORAGE (app/cbl/CBACT04C.cbl:166-173)
    private String wsLastAcctNum = " ".repeat(11);
    private BigDecimal wsTotalInt = BigDecimal.ZERO.setScale(2);
    private BigDecimal wsMonthlyInt = BigDecimal.ZERO.setScale(2);
    private boolean wsFirstTime = true;
    private int wsRecordCount;
    private int wsTranidSuffix;

    // Record areas
    private byte[] accountRecord;
    private String xrefCardNum = "";
    private BigDecimal disIntRate = BigDecimal.ZERO.setScale(2);

    private final List<byte[]> writtenTransactions = new ArrayList<>();
    private final List<byte[]> rewrittenAccounts = new ArrayList<>();

    Cbact04cReferenceModel(byte[] tcatbalImage, byte[] xrefImage, byte[] accountImage, byte[] discgrpImage,
                           String parmDate, String db2Timestamp) {
        this.tcatbalRecords = split(tcatbalImage, TCATBAL_LENGTH);
        this.xrefRecords = split(xrefImage, XREF_LENGTH);
        this.accountRecords = split(accountImage, ACCOUNT_LENGTH);
        this.discgrpRecords = split(discgrpImage, DISCGRP_LENGTH);
        this.parmDate = parmDate;
        this.db2Timestamp = db2Timestamp;
    }

    /** PROCEDURE DIVISION main loop, app/cbl/CBACT04C.cbl:188-222. */
    Output run() {
        for (byte[] tranCatBalRecord : tcatbalRecords) {
            wsRecordCount++;
            String trancatAcctId = text(tranCatBalRecord, 0, 11);
            if (!trancatAcctId.equals(wsLastAcctNum)) {
                if (!wsFirstTime) {
                    paragraph1050UpdateAccount();
                } else {
                    wsFirstTime = false;
                }
                wsTotalInt = BigDecimal.ZERO.setScale(2);
                wsLastAcctNum = trancatAcctId;
                paragraph1100GetAcctData(trancatAcctId);
                paragraph1110GetXrefData(trancatAcctId);
            }
            String disAcctGroupId = text(accountRecord, 112, 10);
            String trancatTypeCd = text(tranCatBalRecord, 11, 2);
            String trancatCd = text(tranCatBalRecord, 13, 4);
            paragraph1200GetInterestRate(disAcctGroupId, trancatTypeCd, trancatCd);
            if (disIntRate.signum() != 0) {
                paragraph1300ComputeInterest(tranCatBalRecord);
            }
        }
        // The ELSE branch at app/cbl/CBACT04C.cbl:219-221 that would post the final account is
        // unreachable: PERFORM UNTIL END-OF-FILE = 'Y' exits as soon as the read sets the flag.
        return new Output(List.copyOf(writtenTransactions), List.copyOf(rewrittenAccounts), wsRecordCount);
    }

    /** 1050-UPDATE-ACCOUNT, app/cbl/CBACT04C.cbl:350-356. */
    private void paragraph1050UpdateAccount() {
        BigDecimal acctCurrBal = zoned(accountRecord, 12, 12, 2);
        putZoned(accountRecord, 12, 12, 2, fit(acctCurrBal.add(wsTotalInt), 10, 2));
        putZoned(accountRecord, 78, 12, 2, BigDecimal.ZERO);
        putZoned(accountRecord, 90, 12, 2, BigDecimal.ZERO);
        rewrittenAccounts.add(accountRecord.clone());
    }

    /** 1100-GET-ACCT-DATA, app/cbl/CBACT04C.cbl:372-376. */
    private void paragraph1100GetAcctData(String acctId) {
        accountRecord = accountRecords.stream()
                .filter(record -> text(record, 0, 11).equals(acctId))
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("ACCOUNT NOT FOUND: " + acctId))
                .clone();
    }

    /** 1110-GET-XREF-DATA, app/cbl/CBACT04C.cbl:393-398 (alternate key FD-XREF-ACCT-ID). */
    private void paragraph1110GetXrefData(String acctId) {
        byte[] xrefRecord = xrefRecords.stream()
                .sorted((left, right) -> text(left, 0, 16).compareTo(text(right, 0, 16)))
                .filter(record -> text(record, 25, 11).equals(acctId))
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("XREF NOT FOUND: " + acctId));
        xrefCardNum = text(xrefRecord, 0, 16);
    }

    /** 1200-GET-INTEREST-RATE and 1200-A-GET-DEFAULT-INT-RATE, app/cbl/CBACT04C.cbl:415-460. */
    private void paragraph1200GetInterestRate(String acctGroupId, String tranTypeCd, String tranCatCd) {
        byte[] disGroupRecord = readDiscgrp(acctGroupId, tranTypeCd, tranCatCd);
        if (disGroupRecord == null) {
            disGroupRecord = readDiscgrp(pad("DEFAULT", 10), tranTypeCd, tranCatCd);
            if (disGroupRecord == null) {
                throw new IllegalStateException("ERROR READING DEFAULT DISCLOSURE GROUP");
            }
        }
        disIntRate = zoned(disGroupRecord, 16, 6, 2);
    }

    /** 1300-COMPUTE-INTEREST, app/cbl/CBACT04C.cbl:462-468. */
    private void paragraph1300ComputeInterest(byte[] tranCatBalRecord) {
        BigDecimal tranCatBal = zoned(tranCatBalRecord, 17, 11, 2);
        wsMonthlyInt = fit(tranCatBal.multiply(disIntRate)
                .divide(BigDecimal.valueOf(1200), 20, RoundingMode.DOWN), 9, 2);
        wsTotalInt = fit(wsTotalInt.add(wsMonthlyInt), 9, 2);
        paragraph1300BWriteTx();
    }

    /** 1300-B-WRITE-TX, app/cbl/CBACT04C.cbl:473-500. */
    private void paragraph1300BWriteTx() {
        // WS-TRANID-SUFFIX PIC 9(06), ADD 1 without ON SIZE ERROR: wraps at one million.
        wsTranidSuffix = (wsTranidSuffix + 1) % 1_000_000;
        byte[] record = blank(TRANSACTION_LENGTH);
        putText(record, 0, 16, parmDate + "%06d".formatted(wsTranidSuffix));
        putText(record, 16, 2, "01");
        putText(record, 18, 4, "0005");
        putText(record, 22, 10, "System");
        putText(record, 32, 100, "Int. for a/c " + text(accountRecord, 0, 11));
        putZoned(record, 132, 11, 2, wsMonthlyInt);
        putText(record, 143, 9, "000000000");
        putText(record, 152, 50, "");
        putText(record, 202, 50, "");
        putText(record, 252, 10, "");
        putText(record, 262, 16, xrefCardNum);
        putText(record, 278, 26, db2Timestamp);
        putText(record, 304, 26, db2Timestamp);
        writtenTransactions.add(record);
    }

    private byte[] readDiscgrp(String acctGroupId, String tranTypeCd, String tranCatCd) {
        String key = acctGroupId + tranTypeCd + tranCatCd;
        return discgrpRecords.stream()
                .filter(record -> text(record, 0, 16).equals(key))
                .findFirst()
                .orElse(null);
    }

    // ----- primitive COBOL data handling, independent of the production record reader -----

    private static List<byte[]> split(byte[] image, int recordLength) {
        List<byte[]> records = new ArrayList<>();
        for (int offset = 0; offset < image.length; offset += recordLength) {
            records.add(Arrays.copyOfRange(image, offset, offset + recordLength));
        }
        return records;
    }

    private static String text(byte[] record, int offset, int length) {
        return new String(record, offset, length, CP037);
    }

    private static void putText(byte[] record, int offset, int length, String value) {
        String text = value.length() > length ? value.substring(0, length) : value;
        byte[] bytes = (text + " ".repeat(length - text.length())).getBytes(CP037);
        System.arraycopy(bytes, 0, record, offset, length);
    }

    private static byte[] blank(int length) {
        byte[] record = new byte[length];
        Arrays.fill(record, (byte) 0x40);
        return record;
    }

    private static BigDecimal zoned(byte[] record, int offset, int digits, int scale) {
        String raw = text(record, offset, digits);
        char last = raw.charAt(digits - 1);
        String positive = "{ABCDEFGHI";
        String negative = "}JKLMNOPQR";
        String head = raw.substring(0, digits - 1);
        boolean isNegative = negative.indexOf(last) >= 0;
        char lastDigit;
        if (Character.isDigit(last)) {
            lastDigit = last;
        } else if (isNegative) {
            lastDigit = (char) ('0' + negative.indexOf(last));
        } else if (positive.indexOf(last) >= 0) {
            lastDigit = (char) ('0' + positive.indexOf(last));
        } else {
            throw new IllegalStateException("Bad zoned decimal: " + raw);
        }
        BigDecimal value = new BigDecimal(new BigInteger(head + lastDigit), scale);
        return isNegative ? value.negate() : value;
    }

    private static void putZoned(byte[] record, int offset, int digits, int scale, BigDecimal value) {
        BigDecimal fitted = fit(value, digits - scale, scale);
        String plain = fitted.abs().movePointRight(scale).toBigInteger().toString();
        String padded = "0".repeat(Math.max(0, digits - plain.length())) + plain;
        padded = padded.substring(padded.length() - digits);
        String overpunch = fitted.signum() < 0 ? "}JKLMNOPQR" : "{ABCDEFGHI";
        String encoded = padded.substring(0, digits - 1)
                + overpunch.charAt(padded.charAt(digits - 1) - '0');
        System.arraycopy(encoded.getBytes(CP037), 0, record, offset, digits);
    }

    private static BigDecimal fit(BigDecimal value, int integerDigits, int scale) {
        BigDecimal truncated = value.setScale(scale, RoundingMode.DOWN);
        BigDecimal modulus = BigDecimal.TEN.pow(integerDigits);
        if (truncated.abs().compareTo(modulus) >= 0) {
            BigDecimal magnitude = truncated.abs().remainder(modulus);
            return truncated.signum() < 0 ? magnitude.negate() : magnitude;
        }
        return truncated;
    }

    private static String pad(String value, int length) {
        return value + " ".repeat(length - value.length());
    }
}
