package com.carddemo.programs.csutldtc.copybook;

import java.nio.ByteBuffer;
import java.util.Arrays;

/**
 * FEEDBACK-CODE (12 bytes) returned by CEEDAYS:
 * <pre>
 *   02 FEEDBACK-TOKEN-VALUE.
 *      03 CASE-1-CONDITION-ID.
 *         04 SEVERITY     PIC S9(4) BINARY.
 *         04 MSG-NO       PIC S9(4) BINARY.
 *      03 CASE-2-CONDITION-ID REDEFINES CASE-1-CONDITION-ID.
 *         04 CLASS-CODE   PIC S9(4) BINARY.
 *         04 CAUSE-CODE   PIC S9(4) BINARY.
 *      03 CASE-SEV-CTL    PIC X.
 *      03 FACILITY-ID     PIC XXX.
 *   02 I-S-INFO           PIC S9(9) BINARY.
 * </pre>
 * Raw bytes are kept (FACILITY-ID is EBCDIC 'CEE' = X'C3C5C5') so 88-level comparisons are byte exact.
 */
public final class FeedbackCode {
    public static final int LENGTH = 12;
    public static final byte CEE_CASE_SEV_CTL = 0x59;
    public static final byte[] CEE_FACILITY_EBCDIC = {(byte) 0xC3, (byte) 0xC5, (byte) 0xC5};

    private final byte[] bytes;

    private FeedbackCode(byte[] bytes) {
        this.bytes = bytes;
    }

    public static FeedbackCode fromBytes(byte[] raw) {
        if (raw.length != LENGTH) {
            throw new IllegalArgumentException("FEEDBACK-CODE must be 12 bytes");
        }
        return new FeedbackCode(raw.clone());
    }

    /** CEE000: condition token of all LOW-VALUES. */
    public static FeedbackCode success() {
        return new FeedbackCode(new byte[LENGTH]);
    }

    /** Case-1 CEE condition token with CASE-SEV-CTL X'59', FACILITY-ID 'CEE', I-S-INFO 0. */
    public static FeedbackCode ofCondition(int severity, int msgNo) {
        ByteBuffer b = ByteBuffer.allocate(LENGTH);
        b.putShort((short) severity).putShort((short) msgNo).put(CEE_CASE_SEV_CTL).put(CEE_FACILITY_EBCDIC).putInt(0);
        return new FeedbackCode(b.array());
    }

    public short severity() {
        return ByteBuffer.wrap(bytes).getShort(0);
    }

    public short msgNo() {
        return ByteBuffer.wrap(bytes).getShort(2);
    }

    /** CLASS-CODE REDEFINES SEVERITY. */
    public short classCode() {
        return severity();
    }

    /** CAUSE-CODE REDEFINES MSG-NO. */
    public short causeCode() {
        return msgNo();
    }

    public byte caseSevCtl() {
        return bytes[4];
    }

    public byte[] facilityId() {
        return Arrays.copyOfRange(bytes, 5, 8);
    }

    public int iSInfo() {
        return ByteBuffer.wrap(bytes).getInt(8);
    }

    /** FEEDBACK-TOKEN-VALUE: first 8 bytes. */
    public byte[] tokenValue() {
        return Arrays.copyOf(bytes, 8);
    }

    public byte[] toBytes() {
        return bytes.clone();
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof FeedbackCode f && Arrays.equals(bytes, f.bytes);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(bytes);
    }

    @Override
    public String toString() {
        return "FeedbackCode[severity=" + severity() + ", msgNo=" + msgNo() + "]";
    }
}
