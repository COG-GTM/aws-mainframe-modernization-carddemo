package com.carddemo.posting.parity;

import java.io.ByteArrayOutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * A literal transliteration of {@code app/cbl/CBTRN02C.cbl}, used as the oracle of the parity
 * harness.
 *
 * <p>Deliberately unidiomatic and deliberately independent: it walks raw record images with its
 * own offset arithmetic, its own EBCDIC sign-overpunch handling and scaled {@code long} cents
 * instead of {@link java.math.BigDecimal}, and it shares no code with the production module. A
 * bug in {@code com.carddemo.mainframe.io} therefore cannot cancel itself out across the two
 * engines.
 *
 * <p>Structure follows the COBOL paragraph by paragraph so it can be read against the source:
 * {@link #run()} is the {@code PERFORM UNTIL END-OF-FILE} loop
 * ({@code app/cbl/CBTRN02C.cbl:202-219}), {@link #validateTran(int)} is
 * {@code 1500-VALIDATE-TRAN}, and so on.
 */
final class Cbtrn02cReferenceModel {

    private static final Charset CP037 = Charset.forName("IBM037");

    private static final int DALYTRAN_LEN = 350;
    private static final int TRANSACT_LEN = 350;
    private static final int REJECT_LEN = 430;
    private static final int XREF_LEN = 50;
    private static final int ACCT_LEN = 300;
    private static final int TCATBAL_LEN = 50;

    // CVTRA06Y / CVTRA05Y field offsets.
    private static final int TRAN_TYPE_CD = 16;
    private static final int TRAN_CAT_CD = 18;
    private static final int TRAN_AMT = 132;
    private static final int TRAN_CARD_NUM = 262;
    private static final int TRAN_ORIG_TS = 278;
    private static final int TRAN_PROC_TS = 304;

    // CVACT03Y field offsets.
    private static final int XREF_CARD_NUM = 0;
    private static final int XREF_ACCT_ID = 25;

    // CVACT01Y field offsets.
    private static final int ACCT_ID = 0;
    private static final int ACCT_CURR_BAL = 12;
    private static final int ACCT_CREDIT_LIMIT = 24;
    private static final int ACCT_EXPIRAION_DATE = 58;
    private static final int ACCT_CURR_CYC_CREDIT = 78;
    private static final int ACCT_CURR_CYC_DEBIT = 90;

    // CVTRA01Y field offsets.
    private static final int TRANCAT_ACCT_ID = 0;
    private static final int TRANCAT_TYPE_CD = 11;
    private static final int TRANCAT_CD = 13;
    private static final int TRAN_CAT_BAL = 17;

    /** Capacity of {@code PIC S9(10)V99}, in cents. */
    private static final long ACCT_MONEY_CAPACITY = 1_000_000_000_000L;
    /** Capacity of {@code PIC S9(09)V99}, in cents. */
    private static final long CAT_MONEY_CAPACITY = 100_000_000_000L;

    /** What the run produced, in the order the COBOL wrote it. */
    record Output(List<byte[]> transactionRecords,
                  List<byte[]> rejectRecords,
                  byte[] accountFileImage,
                  byte[] categoryBalanceFileImage,
                  int transactionsRead,
                  int categoryBalancesCreated,
                  int returnCode) {
    }

    private final byte[] dalytran;
    private final Map<String, byte[]> xrefByCard = new LinkedHashMap<>();
    private final Map<String, byte[]> acctById = new LinkedHashMap<>();
    private final Map<String, byte[]> tcatByKey = new LinkedHashMap<>();
    private final int originalTcatCount;
    private final String db2Timestamp;

    private final List<byte[]> transactionRecords = new ArrayList<>();
    private final List<byte[]> rejectRecords = new ArrayList<>();

    /** {@code 01 TRAN-CAT-BAL-RECORD} in working storage. */
    private byte[] tranCatBalRecord = blank(TCATBAL_LEN);

    Cbtrn02cReferenceModel(byte[] dalytran, byte[] xref, byte[] acct, byte[] tcatbal,
                           String db2Timestamp) {
        this.dalytran = dalytran;
        this.db2Timestamp = db2Timestamp;
        for (int offset = 0; offset < xref.length; offset += XREF_LEN) {
            byte[] record = Arrays.copyOfRange(xref, offset, offset + XREF_LEN);
            xrefByCard.putIfAbsent(text(record, XREF_CARD_NUM, 16), record);
        }
        for (int offset = 0; offset < acct.length; offset += ACCT_LEN) {
            byte[] record = Arrays.copyOfRange(acct, offset, offset + ACCT_LEN);
            acctById.putIfAbsent(text(record, ACCT_ID, 11), record);
        }
        for (int offset = 0; offset < tcatbal.length; offset += TCATBAL_LEN) {
            byte[] record = Arrays.copyOfRange(tcatbal, offset, offset + TCATBAL_LEN);
            tcatByKey.putIfAbsent(text(record, TRANCAT_ACCT_ID, 17), record);
        }
        this.originalTcatCount = tcatByKey.size();
    }

    /** PROCEDURE DIVISION main loop, {@code app/cbl/CBTRN02C.cbl:202-232}. */
    Output run() {
        int transactionCount = 0;
        int rejectCount = 0;
        for (int offset = 0; offset < dalytran.length; offset += DALYTRAN_LEN) {
            transactionCount++;
            int reason = validateTran(offset);
            if (reason == 0) {
                postTransaction(offset);
            } else {
                rejectCount++;
                writeRejectRec(offset, reason);
            }
        }
        return new Output(transactionRecords, rejectRecords,
                concat(acctById.values()), concat(new TreeMap<>(tcatByKey).values()),
                transactionCount, tcatByKey.size() - originalTcatCount,
                rejectCount > 0 ? 4 : 0);
    }

    /** {@code 1500-VALIDATE-TRAN} plus its two lookup paragraphs. */
    private int validateTran(int offset) {
        byte[] xref = xrefByCard.get(text(dalytran, offset + TRAN_CARD_NUM, 16));
        if (xref == null) {
            return 100;
        }
        byte[] acct = acctById.get(text(xref, XREF_ACCT_ID, 11));
        if (acct == null) {
            return 101;
        }
        int reason = 0;
        long tempBal = truncate(zoned(acct, ACCT_CURR_CYC_CREDIT, 12)
                        - zoned(acct, ACCT_CURR_CYC_DEBIT, 12)
                        + zoned(dalytran, offset + TRAN_AMT, 11),
                CAT_MONEY_CAPACITY);
        if (zoned(acct, ACCT_CREDIT_LIMIT, 12) < tempBal) {
            reason = 102;
        }
        if (text(acct, ACCT_EXPIRAION_DATE, 10)
                .compareTo(text(dalytran, offset + TRAN_ORIG_TS, 10)) < 0) {
            reason = 103;
        }
        return reason;
    }

    /** {@code 2000-POST-TRANSACTION}, {@code app/cbl/CBTRN02C.cbl:424-444}. */
    private void postTransaction(int offset) {
        byte[] xref = xrefByCard.get(text(dalytran, offset + TRAN_CARD_NUM, 16));
        String acctId = text(xref, XREF_ACCT_ID, 11);
        long amount = zoned(dalytran, offset + TRAN_AMT, 11);

        byte[] tranRecord = Arrays.copyOfRange(dalytran, offset, offset + TRANSACT_LEN);
        System.arraycopy(db2Timestamp.getBytes(CP037), 0, tranRecord, TRAN_PROC_TS, 26);

        updateTcatbal(offset, acctId, amount);
        updateAccountRec(acctId, amount);
        transactionRecords.add(tranRecord);
    }

    /** {@code 2700-UPDATE-TCATBAL} and branches, {@code app/cbl/CBTRN02C.cbl:467-542}. */
    private void updateTcatbal(int offset, String acctId, long amount) {
        String key = acctId + text(dalytran, offset + TRAN_TYPE_CD, 2)
                + text(dalytran, offset + TRAN_CAT_CD, 4);
        byte[] found = tcatByKey.get(key);
        boolean create = found == null;
        if (!create) {
            tranCatBalRecord = found.clone();
        }
        if (create) {
            // INITIALIZE leaves FILLER alone; the named fields are reset.
            putText(tranCatBalRecord, TRANCAT_ACCT_ID, 11, acctId);
            putText(tranCatBalRecord, TRANCAT_TYPE_CD, 2, text(dalytran, offset + TRAN_TYPE_CD, 2));
            putText(tranCatBalRecord, TRANCAT_CD, 4, text(dalytran, offset + TRAN_CAT_CD, 4));
            putZoned(tranCatBalRecord, TRAN_CAT_BAL, 11, 0);
        }
        long balance = truncate(zoned(tranCatBalRecord, TRAN_CAT_BAL, 11) + amount,
                CAT_MONEY_CAPACITY);
        putZoned(tranCatBalRecord, TRAN_CAT_BAL, 11, balance);
        tcatByKey.put(key, tranCatBalRecord.clone());
    }

    /** {@code 2800-UPDATE-ACCOUNT-REC}, {@code app/cbl/CBTRN02C.cbl:545-559}. */
    private void updateAccountRec(String acctId, long amount) {
        byte[] acct = acctById.get(acctId).clone();
        putZoned(acct, ACCT_CURR_BAL, 12,
                truncate(zoned(acct, ACCT_CURR_BAL, 12) + amount, ACCT_MONEY_CAPACITY));
        if (amount >= 0) {
            putZoned(acct, ACCT_CURR_CYC_CREDIT, 12,
                    truncate(zoned(acct, ACCT_CURR_CYC_CREDIT, 12) + amount, ACCT_MONEY_CAPACITY));
        } else {
            putZoned(acct, ACCT_CURR_CYC_DEBIT, 12,
                    truncate(zoned(acct, ACCT_CURR_CYC_DEBIT, 12) + amount, ACCT_MONEY_CAPACITY));
        }
        acctById.put(acctId, acct);
    }

    /** {@code 2500-WRITE-REJECT-REC}, {@code app/cbl/CBTRN02C.cbl:446-465}. */
    private void writeRejectRec(int offset, int reason) {
        byte[] record = blank(REJECT_LEN);
        System.arraycopy(dalytran, offset, record, 0, DALYTRAN_LEN);
        putText(record, DALYTRAN_LEN, 4, "%04d".formatted(reason));
        putText(record, DALYTRAN_LEN + 4, 76, switch (reason) {
            case 100 -> "INVALID CARD NUMBER FOUND";
            case 101 -> "ACCOUNT RECORD NOT FOUND";
            case 102 -> "OVERLIMIT TRANSACTION";
            case 103 -> "TRANSACTION RECEIVED AFTER ACCT EXPIRATION";
            default -> throw new IllegalStateException("Unknown reason " + reason);
        });
        rejectRecords.add(record);
    }

    // --- COBOL data handling, implemented from first principles -------------------------

    /** Reads a zoned-decimal {@code PIC S9(n)V99} field as cents, honouring the sign overpunch. */
    private static long zoned(byte[] record, int offset, int length) {
        long value = 0;
        boolean negative = false;
        for (int index = 0; index < length; index++) {
            int b = record[offset + index] & 0xFF;
            int digit = b & 0x0F;
            if (index == length - 1) {
                int zone = b & 0xF0;
                negative = zone == 0xD0;
            }
            value = value * 10 + digit;
        }
        return negative ? -value : value;
    }

    /** Writes cents back into a zoned-decimal field, low-order digit carrying the sign. */
    private static void putZoned(byte[] record, int offset, int length, long cents) {
        long magnitude = Math.abs(cents);
        for (int index = length - 1; index >= 0; index--) {
            int digit = (int) (magnitude % 10);
            magnitude /= 10;
            int zone = index == length - 1 ? (cents < 0 ? 0xD0 : 0xC0) : 0xF0;
            record[offset + index] = (byte) (zone | digit);
        }
    }

    /** Truncation of an unrounded COBOL {@code ADD}/{@code COMPUTE} into a narrower field. */
    private static long truncate(long cents, long capacity) {
        long remainder = Math.abs(cents) % capacity;
        return cents < 0 ? -remainder : remainder;
    }

    private static String text(byte[] record, int offset, int length) {
        return new String(record, offset, length, CP037);
    }

    private static void putText(byte[] record, int offset, int length, String value) {
        byte[] encoded = (value + " ".repeat(Math.max(0, length - value.length())))
                .substring(0, length).getBytes(CP037);
        System.arraycopy(encoded, 0, record, offset, length);
    }

    private static byte[] blank(int length) {
        byte[] record = new byte[length];
        Arrays.fill(record, (byte) 0x40);
        return record;
    }

    private static byte[] concat(Iterable<byte[]> records) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        records.forEach(out::writeBytes);
        return out.toByteArray();
    }
}
