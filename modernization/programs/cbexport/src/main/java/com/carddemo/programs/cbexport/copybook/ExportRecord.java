package com.carddemo.programs.cbexport.copybook;

import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getBinary;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getPacked;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getSignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getUnsignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putBinary;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putPacked;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putSignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putUnsignedZoned;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.List;

/**
 * CVEXPORT - 01 EXPORT-RECORD (500 bytes). Backed by the raw record image so the REDEFINES
 * views (timestamp and the five EXPORT-RECORD-DATA layouts) overlay the same storage, exactly as
 * in COBOL. Byte offsets below are 0-based.
 *
 * <pre>
 *   0   EXPORT-REC-TYPE        X(1)
 *   1   EXPORT-TIMESTAMP       X(26)  (REDEFINES: DATE X(10), SEP X(1), TIME X(15))
 *   27  EXPORT-SEQUENCE-NUM    9(9) COMP (4 bytes)
 *   31  EXPORT-BRANCH-ID       X(4)
 *   35  EXPORT-REGION-CODE     X(5)
 *   40  EXPORT-RECORD-DATA     X(460)
 * </pre>
 */
public final class ExportRecord {

    public static final int LENGTH = 500;
    public static final int REC_TYPE_OFFSET = 0;
    public static final int TIMESTAMP_OFFSET = 1;
    public static final int TIMESTAMP_LENGTH = 26;
    public static final int SEQUENCE_NUM_OFFSET = 27;
    public static final int SEQUENCE_NUM_DIGITS = 9;
    public static final int SEQUENCE_NUM_LENGTH = 4;
    public static final int BRANCH_ID_OFFSET = 31;
    public static final int REGION_CODE_OFFSET = 35;
    public static final int DATA_OFFSET = 40;
    public static final int DATA_LENGTH = 460;

    private static final int D = DATA_OFFSET;

    private final byte[] data;

    public ExportRecord() {
        this.data = new byte[LENGTH];
        initialize();
    }

    private ExportRecord(byte[] image) {
        this.data = image;
    }

    public static ExportRecord fromBytes(byte[] image) {
        if (image.length != LENGTH) {
            throw new IllegalArgumentException("EXPORT-RECORD must be " + LENGTH + " bytes, got " + image.length);
        }
        return new ExportRecord(image.clone());
    }

    /**
     * INITIALIZE EXPORT-RECORD: alphanumeric items to spaces, numeric items to zero. REDEFINES
     * entries are skipped, so the data area is initialised as EXPORT-RECORD-DATA PIC X(460)
     * (all spaces) and the binary sequence number becomes zero.
     */
    public void initialize() {
        Arrays.fill(data, CobolFieldCodec.SPACE);
        setSequenceNum(0);
    }

    public byte[] toBytes() {
        return data.clone();
    }

    // ---- common header --------------------------------------------------------------------

    public char recType() {
        return (char) (data[REC_TYPE_OFFSET] & 0xFF);
    }

    public void setRecType(char type) {
        putAlnum(data, REC_TYPE_OFFSET, 1, String.valueOf(type));
    }

    public ExportRecordType recordType() {
        return ExportRecordType.fromCode(recType());
    }

    public String timestamp() {
        return getAlnum(data, TIMESTAMP_OFFSET, TIMESTAMP_LENGTH);
    }

    public void setTimestamp(String ts) {
        putAlnum(data, TIMESTAMP_OFFSET, TIMESTAMP_LENGTH, ts);
    }

    /** EXPORT-DATE (EXPORT-TIMESTAMP-R). */
    public String exportDate() {
        return getAlnum(data, TIMESTAMP_OFFSET, 10);
    }

    /** EXPORT-DATE-TIME-SEP (EXPORT-TIMESTAMP-R). */
    public String exportDateTimeSep() {
        return getAlnum(data, TIMESTAMP_OFFSET + 10, 1);
    }

    /** EXPORT-TIME (EXPORT-TIMESTAMP-R). */
    public String exportTime() {
        return getAlnum(data, TIMESTAMP_OFFSET + 11, 15);
    }

    public long sequenceNum() {
        return getBinary(data, SEQUENCE_NUM_OFFSET, SEQUENCE_NUM_DIGITS, false);
    }

    public void setSequenceNum(long seq) {
        putBinary(data, SEQUENCE_NUM_OFFSET, SEQUENCE_NUM_DIGITS, false, seq);
    }

    public String branchId() {
        return getAlnum(data, BRANCH_ID_OFFSET, 4);
    }

    public void setBranchId(String branchId) {
        putAlnum(data, BRANCH_ID_OFFSET, 4, branchId);
    }

    public String regionCode() {
        return getAlnum(data, REGION_CODE_OFFSET, 5);
    }

    public void setRegionCode(String regionCode) {
        putAlnum(data, REGION_CODE_OFFSET, 5, regionCode);
    }

    /** EXPORT-RECORD-DATA PIC X(460). */
    public String recordData() {
        return getAlnum(data, DATA_OFFSET, DATA_LENGTH);
    }

    // ---- EXPORT-CUSTOMER-DATA ----------------------------------------------------------------

    public void setCustomerData(ExportCustomerData c) {
        putBinary(data, D, 9, false, c.custId());
        putAlnum(data, D + 4, 25, c.firstName());
        putAlnum(data, D + 29, 25, c.middleName());
        putAlnum(data, D + 54, 25, c.lastName());
        for (int i = 0; i < ExportCustomerData.ADDR_LINE_OCCURS; i++) {
            putAlnum(data, D + 79 + i * 50, 50, c.addrLines().get(i));
        }
        putAlnum(data, D + 229, 2, c.addrStateCd());
        putAlnum(data, D + 231, 3, c.addrCountryCd());
        putAlnum(data, D + 234, 10, c.addrZip());
        for (int i = 0; i < ExportCustomerData.PHONE_NUM_OCCURS; i++) {
            putAlnum(data, D + 244 + i * 15, 15, c.phoneNums().get(i));
        }
        putUnsignedZoned(data, D + 274, 9, c.ssn());
        putAlnum(data, D + 283, 20, c.govtIssuedId());
        putAlnum(data, D + 303, 10, c.dobYyyyMmDd());
        putAlnum(data, D + 313, 10, c.eftAccountId());
        putAlnum(data, D + 323, 1, c.priCardHolderInd());
        putPacked(data, D + 324, 3, 0, false, BigDecimal.valueOf(c.ficoCreditScore()));
        // FILLER X(134) at D+326 is left as initialised.
    }

    public ExportCustomerData customerData() {
        return new ExportCustomerData(
                getBinary(data, D, 9, false),
                getAlnum(data, D + 4, 25),
                getAlnum(data, D + 29, 25),
                getAlnum(data, D + 54, 25),
                List.of(getAlnum(data, D + 79, 50), getAlnum(data, D + 129, 50), getAlnum(data, D + 179, 50)),
                getAlnum(data, D + 229, 2),
                getAlnum(data, D + 231, 3),
                getAlnum(data, D + 234, 10),
                List.of(getAlnum(data, D + 244, 15), getAlnum(data, D + 259, 15)),
                getUnsignedZoned(data, D + 274, 9),
                getAlnum(data, D + 283, 20),
                getAlnum(data, D + 303, 10),
                getAlnum(data, D + 313, 10),
                getAlnum(data, D + 323, 1),
                getPacked(data, D + 324, 3, 0).intValueExact());
    }

    // ---- EXPORT-ACCOUNT-DATA -----------------------------------------------------------------

    public void setAccountData(ExportAccountData a) {
        putUnsignedZoned(data, D, 11, a.acctId());
        putAlnum(data, D + 11, 1, a.activeStatus());
        putPacked(data, D + 12, 12, 2, true, a.currBal());
        putSignedZoned(data, D + 19, 12, 2, a.creditLimit());
        putPacked(data, D + 31, 12, 2, true, a.cashCreditLimit());
        putAlnum(data, D + 38, 10, a.openDate());
        putAlnum(data, D + 48, 10, a.expiraionDate());
        putAlnum(data, D + 58, 10, a.reissueDate());
        putSignedZoned(data, D + 68, 12, 2, a.currCycCredit());
        putBinary(data, D + 80, 12, true,
                a.currCycDebit().setScale(2, RoundingMode.DOWN).unscaledValue().longValueExact());
        putAlnum(data, D + 88, 10, a.addrZip());
        putAlnum(data, D + 98, 10, a.groupId());
        // FILLER X(352) at D+108 is left as initialised.
    }

    public ExportAccountData accountData() {
        return new ExportAccountData(
                getUnsignedZoned(data, D, 11),
                getAlnum(data, D + 11, 1),
                getPacked(data, D + 12, 12, 2),
                getSignedZoned(data, D + 19, 12, 2),
                getPacked(data, D + 31, 12, 2),
                getAlnum(data, D + 38, 10),
                getAlnum(data, D + 48, 10),
                getAlnum(data, D + 58, 10),
                getSignedZoned(data, D + 68, 12, 2),
                BigDecimal.valueOf(getBinary(data, D + 80, 12, true), 2),
                getAlnum(data, D + 88, 10),
                getAlnum(data, D + 98, 10));
    }

    // ---- EXPORT-TRANSACTION-DATA -------------------------------------------------------------

    public void setTransactionData(ExportTransactionData t) {
        putAlnum(data, D, 16, t.tranId());
        putAlnum(data, D + 16, 2, t.typeCd());
        putUnsignedZoned(data, D + 18, 4, t.catCd());
        putAlnum(data, D + 22, 10, t.source());
        putAlnum(data, D + 32, 100, t.desc());
        putPacked(data, D + 132, 11, 2, true, t.amt());
        putBinary(data, D + 138, 9, false, t.merchantId());
        putAlnum(data, D + 142, 50, t.merchantName());
        putAlnum(data, D + 192, 50, t.merchantCity());
        putAlnum(data, D + 242, 10, t.merchantZip());
        putAlnum(data, D + 252, 16, t.cardNum());
        putAlnum(data, D + 268, 26, t.origTs());
        putAlnum(data, D + 294, 26, t.procTs());
        // FILLER X(140) at D+320 is left as initialised.
    }

    public ExportTransactionData transactionData() {
        return new ExportTransactionData(
                getAlnum(data, D, 16),
                getAlnum(data, D + 16, 2),
                (int) getUnsignedZoned(data, D + 18, 4),
                getAlnum(data, D + 22, 10),
                getAlnum(data, D + 32, 100),
                getPacked(data, D + 132, 11, 2),
                getBinary(data, D + 138, 9, false),
                getAlnum(data, D + 142, 50),
                getAlnum(data, D + 192, 50),
                getAlnum(data, D + 242, 10),
                getAlnum(data, D + 252, 16),
                getAlnum(data, D + 268, 26),
                getAlnum(data, D + 294, 26));
    }

    // ---- EXPORT-CARD-XREF-DATA ---------------------------------------------------------------

    public void setCardXrefData(ExportCardXrefData x) {
        putAlnum(data, D, 16, x.cardNum());
        putUnsignedZoned(data, D + 16, 9, x.custId());
        putBinary(data, D + 25, 11, false, x.acctId());
        // FILLER X(427) at D+33 is left as initialised.
    }

    public ExportCardXrefData cardXrefData() {
        return new ExportCardXrefData(
                getAlnum(data, D, 16),
                getUnsignedZoned(data, D + 16, 9),
                getBinary(data, D + 25, 11, false));
    }

    // ---- EXPORT-CARD-DATA --------------------------------------------------------------------

    public void setCardData(ExportCardData c) {
        putAlnum(data, D, 16, c.cardNum());
        putBinary(data, D + 16, 11, false, c.acctId());
        putBinary(data, D + 24, 3, false, c.cvvCd());
        putAlnum(data, D + 26, 50, c.embossedName());
        putAlnum(data, D + 76, 10, c.expiraionDate());
        putAlnum(data, D + 86, 1, c.activeStatus());
        // FILLER X(373) at D+87 is left as initialised.
    }

    public ExportCardData cardData() {
        return new ExportCardData(
                getAlnum(data, D, 16),
                getBinary(data, D + 16, 11, false),
                (int) getBinary(data, D + 24, 3, false),
                getAlnum(data, D + 26, 50),
                getAlnum(data, D + 76, 10),
                getAlnum(data, D + 86, 1));
    }
}
