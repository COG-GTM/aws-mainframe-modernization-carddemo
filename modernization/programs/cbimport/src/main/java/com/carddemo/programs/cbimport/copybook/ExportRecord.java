package com.carddemo.programs.cbimport.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.List;

/**
 * CVEXPORT: EXPORT-RECORD, the 500-byte multi-record branch migration export layout.
 *
 * <p>The header is decoded eagerly; EXPORT-RECORD-DATA (460 bytes) is kept raw and each REDEFINES
 * view is decoded on demand, so a view can be taken regardless of EXPORT-REC-TYPE, as in COBOL.
 *
 * @param recType          EXPORT-REC-TYPE PIC X(1)
 * @param timestamp        EXPORT-TIMESTAMP PIC X(26)
 * @param sequenceNum      EXPORT-SEQUENCE-NUM PIC 9(9) COMP (KSDS key, 4-byte unsigned binary)
 * @param branchId         EXPORT-BRANCH-ID PIC X(4)
 * @param regionCode       EXPORT-REGION-CODE PIC X(5)
 * @param recordData       EXPORT-RECORD-DATA PIC X(460), raw bytes
 * @param charset          character set of the alphanumeric/zoned bytes (IBM037 for the mainframe file)
 */
public record ExportRecord(String recType, String timestamp, long sequenceNum, String branchId,
                           String regionCode, byte[] recordData, Charset charset) {

    public static final int LENGTH = 500;
    public static final int RECORD_DATA_OFFSET = 40;
    public static final int RECORD_DATA_LENGTH = 460;

    public static ExportRecord decode(byte[] record, Charset charset) {
        if (record.length != LENGTH) {
            throw new IllegalArgumentException("EXPORT-RECORD must be " + LENGTH + " bytes, got " + record.length);
        }
        FieldCursor c = new FieldCursor(record, 0, charset);
        String recType = c.alnum(1);
        String timestamp = c.alnum(26);
        long seq = c.binaryUnsigned(9).longValueExact();
        String branch = c.alnum(4);
        String region = c.alnum(5);
        byte[] data = c.bytes(RECORD_DATA_LENGTH);
        return new ExportRecord(recType, timestamp, seq, branch, region, data, charset);
    }

    /** Builds a 500-byte record from header fields and an already-encoded EXPORT-RECORD-DATA. */
    public static byte[] encode(String recType, String timestamp, long sequenceNum, String branchId,
                                String regionCode, byte[] recordData, Charset charset) {
        FieldEncoder e = new FieldEncoder(charset)
                .alnum(recType, 1)
                .alnum(timestamp, 26)
                .binaryUnsigned(BigInteger.valueOf(sequenceNum), 9)
                .alnum(branchId, 4)
                .alnum(regionCode, 5);
        byte[] data = new byte[RECORD_DATA_LENGTH];
        System.arraycopy(recordData, 0, data, 0, Math.min(recordData.length, RECORD_DATA_LENGTH));
        if (recordData.length < RECORD_DATA_LENGTH) {
            byte space = " ".getBytes(charset)[0];
            for (int i = recordData.length; i < RECORD_DATA_LENGTH; i++) {
                data[i] = space;
            }
        }
        return e.raw(data).toBytes();
    }

    /** EXPORT-TIMESTAMP-R REDEFINES EXPORT-TIMESTAMP: EXPORT-DATE X(10). */
    public String exportDate() {
        return timestamp.substring(0, 10);
    }

    /** EXPORT-DATE-TIME-SEP X(1). */
    public String exportDateTimeSep() {
        return timestamp.substring(10, 11);
    }

    /** EXPORT-TIME X(15). */
    public String exportTime() {
        return timestamp.substring(11, 26);
    }

    private FieldCursor data() {
        return new FieldCursor(recordData, 0, charset);
    }

    /** EXPORT-CUSTOMER-DATA REDEFINES EXPORT-RECORD-DATA. */
    public CustomerData customerData() {
        FieldCursor c = data();
        return new CustomerData(
                c.binaryUnsigned(9),
                c.alnum(25), c.alnum(25), c.alnum(25),
                List.of(c.alnum(50), c.alnum(50), c.alnum(50)),
                c.alnum(2), c.alnum(3), c.alnum(10),
                List.of(c.alnum(15), c.alnum(15)),
                c.zonedUnsigned(9),
                c.alnum(20), c.alnum(10), c.alnum(10), c.alnum(1),
                c.packed(3, 0, false).toBigIntegerExact());
    }

    /** EXPORT-ACCOUNT-DATA REDEFINES EXPORT-RECORD-DATA. */
    public AccountData accountData() {
        FieldCursor c = data();
        return new AccountData(
                c.zonedUnsigned(11),
                c.alnum(1),
                c.packed(10, 2, true),
                c.zonedSigned(10, 2),
                c.packed(10, 2, true),
                c.alnum(10), c.alnum(10), c.alnum(10),
                c.zonedSigned(10, 2),
                c.binarySigned(10, 2),
                c.alnum(10), c.alnum(10));
    }

    /** EXPORT-TRANSACTION-DATA REDEFINES EXPORT-RECORD-DATA. */
    public TransactionData transactionData() {
        FieldCursor c = data();
        return new TransactionData(
                c.alnum(16), c.alnum(2),
                c.zonedUnsigned(4),
                c.alnum(10), c.alnum(100),
                c.packed(9, 2, true),
                c.binaryUnsigned(9),
                c.alnum(50), c.alnum(50), c.alnum(10), c.alnum(16), c.alnum(26), c.alnum(26));
    }

    /** EXPORT-CARD-XREF-DATA REDEFINES EXPORT-RECORD-DATA. */
    public CardXrefData cardXrefData() {
        FieldCursor c = data();
        return new CardXrefData(c.alnum(16), c.zonedUnsigned(9), c.binaryUnsigned(11));
    }

    /** EXPORT-CARD-DATA REDEFINES EXPORT-RECORD-DATA. */
    public CardData cardData() {
        FieldCursor c = data();
        return new CardData(c.alnum(16), c.binaryUnsigned(11), c.binaryUnsigned(3),
                c.alnum(50), c.alnum(10), c.alnum(1));
    }

    /** EXPORT-CUSTOMER-DATA (FILLER X(134) dropped). */
    public record CustomerData(
            BigInteger custId,                 // EXP-CUST-ID PIC 9(09) COMP
            String firstName,                  // EXP-CUST-FIRST-NAME X(25)
            String middleName,                 // EXP-CUST-MIDDLE-NAME X(25)
            String lastName,                   // EXP-CUST-LAST-NAME X(25)
            List<String> addrLines,            // EXP-CUST-ADDR-LINE X(50) OCCURS 3
            String addrStateCd,                // EXP-CUST-ADDR-STATE-CD X(02)
            String addrCountryCd,              // EXP-CUST-ADDR-COUNTRY-CD X(03)
            String addrZip,                    // EXP-CUST-ADDR-ZIP X(10)
            List<String> phoneNums,            // EXP-CUST-PHONE-NUM X(15) OCCURS 2
            BigInteger ssn,                    // EXP-CUST-SSN 9(09)
            String govtIssuedId,               // EXP-CUST-GOVT-ISSUED-ID X(20)
            String dobYyyyMmDd,                // EXP-CUST-DOB-YYYY-MM-DD X(10)
            String eftAccountId,               // EXP-CUST-EFT-ACCOUNT-ID X(10)
            String priCardHolderInd,           // EXP-CUST-PRI-CARD-HOLDER-IND X(01)
            BigInteger ficoCreditScore) {      // EXP-CUST-FICO-CREDIT-SCORE 9(03) COMP-3

        public static final int ADDR_LINE_OCCURS = 3;
        public static final int PHONE_NUM_OCCURS = 2;

        /** Inverse of {@link ExportRecord#customerData()}, for building export fixtures. */
        public byte[] encode(Charset cs) {
            FieldEncoder e = new FieldEncoder(cs).binaryUnsigned(custId, 9)
                    .alnum(firstName, 25).alnum(middleName, 25).alnum(lastName, 25);
            for (int i = 0; i < ADDR_LINE_OCCURS; i++) {
                e.alnum(addrLines.get(i), 50);
            }
            e.alnum(addrStateCd, 2).alnum(addrCountryCd, 3).alnum(addrZip, 10);
            for (int i = 0; i < PHONE_NUM_OCCURS; i++) {
                e.alnum(phoneNums.get(i), 15);
            }
            return e.zonedUnsigned(ssn.longValue(), 9).alnum(govtIssuedId, 20).alnum(dobYyyyMmDd, 10)
                    .alnum(eftAccountId, 10).alnum(priCardHolderInd, 1)
                    .packed(new BigDecimal(ficoCreditScore), 3, 0, false).spaces(134).toBytes();
        }
    }

    /** EXPORT-ACCOUNT-DATA (FILLER X(352) dropped). */
    public record AccountData(
            BigInteger acctId,                 // EXP-ACCT-ID 9(11)
            String activeStatus,               // EXP-ACCT-ACTIVE-STATUS X(01)
            BigDecimal currBal,                // EXP-ACCT-CURR-BAL S9(10)V99 COMP-3
            BigDecimal creditLimit,            // EXP-ACCT-CREDIT-LIMIT S9(10)V99
            BigDecimal cashCreditLimit,        // EXP-ACCT-CASH-CREDIT-LIMIT S9(10)V99 COMP-3
            String openDate,                   // EXP-ACCT-OPEN-DATE X(10)
            String expirationDate,             // EXP-ACCT-EXPIRAION-DATE X(10)
            String reissueDate,                // EXP-ACCT-REISSUE-DATE X(10)
            BigDecimal currCycCredit,          // EXP-ACCT-CURR-CYC-CREDIT S9(10)V99
            BigDecimal currCycDebit,           // EXP-ACCT-CURR-CYC-DEBIT S9(10)V99 COMP
            String addrZip,                    // EXP-ACCT-ADDR-ZIP X(10)
            String groupId) {                  // EXP-ACCT-GROUP-ID X(10)

        public byte[] encode(Charset cs) {
            return new FieldEncoder(cs).zonedUnsigned(acctId.longValue(), 11).alnum(activeStatus, 1)
                    .packed(currBal, 10, 2, true).zonedSigned(creditLimit, 10, 2)
                    .packed(cashCreditLimit, 10, 2, true)
                    .alnum(openDate, 10).alnum(expirationDate, 10).alnum(reissueDate, 10)
                    .zonedSigned(currCycCredit, 10, 2).binarySigned(currCycDebit, 10, 2)
                    .alnum(addrZip, 10).alnum(groupId, 10).spaces(352).toBytes();
        }
    }

    /** EXPORT-TRANSACTION-DATA (FILLER X(140) dropped). */
    public record TransactionData(
            String tranId,                     // EXP-TRAN-ID X(16)
            String typeCd,                     // EXP-TRAN-TYPE-CD X(02)
            BigInteger catCd,                  // EXP-TRAN-CAT-CD 9(04)
            String source,                     // EXP-TRAN-SOURCE X(10)
            String desc,                       // EXP-TRAN-DESC X(100)
            BigDecimal amt,                    // EXP-TRAN-AMT S9(09)V99 COMP-3
            BigInteger merchantId,             // EXP-TRAN-MERCHANT-ID 9(09) COMP
            String merchantName,               // EXP-TRAN-MERCHANT-NAME X(50)
            String merchantCity,               // EXP-TRAN-MERCHANT-CITY X(50)
            String merchantZip,                // EXP-TRAN-MERCHANT-ZIP X(10)
            String cardNum,                    // EXP-TRAN-CARD-NUM X(16)
            String origTs,                     // EXP-TRAN-ORIG-TS X(26)
            String procTs) {                   // EXP-TRAN-PROC-TS X(26)

        public byte[] encode(Charset cs) {
            return new FieldEncoder(cs).alnum(tranId, 16).alnum(typeCd, 2).zonedUnsigned(catCd.longValue(), 4)
                    .alnum(source, 10).alnum(desc, 100).packed(amt, 9, 2, true)
                    .binaryUnsigned(merchantId, 9).alnum(merchantName, 50).alnum(merchantCity, 50)
                    .alnum(merchantZip, 10).alnum(cardNum, 16).alnum(origTs, 26).alnum(procTs, 26)
                    .spaces(140).toBytes();
        }
    }

    /** EXPORT-CARD-XREF-DATA (FILLER X(427) dropped). */
    public record CardXrefData(
            String cardNum,                    // EXP-XREF-CARD-NUM X(16)
            BigInteger custId,                 // EXP-XREF-CUST-ID 9(09)
            BigInteger acctId) {               // EXP-XREF-ACCT-ID 9(11) COMP

        public byte[] encode(Charset cs) {
            return new FieldEncoder(cs).alnum(cardNum, 16).zonedUnsigned(custId.longValue(), 9)
                    .binaryUnsigned(acctId, 11).spaces(427).toBytes();
        }
    }

    /** EXPORT-CARD-DATA (FILLER X(373) dropped). */
    public record CardData(
            String cardNum,                    // EXP-CARD-NUM X(16)
            BigInteger acctId,                 // EXP-CARD-ACCT-ID 9(11) COMP
            BigInteger cvvCd,                  // EXP-CARD-CVV-CD 9(03) COMP
            String embossedName,               // EXP-CARD-EMBOSSED-NAME X(50)
            String expirationDate,             // EXP-CARD-EXPIRAION-DATE X(10)
            String activeStatus) {             // EXP-CARD-ACTIVE-STATUS X(01)

        public byte[] encode(Charset cs) {
            return new FieldEncoder(cs).alnum(cardNum, 16).binaryUnsigned(acctId, 11).binaryUnsigned(cvvCd, 3)
                    .alnum(embossedName, 50).alnum(expirationDate, 10).alnum(activeStatus, 1)
                    .spaces(373).toBytes();
        }
    }
}
