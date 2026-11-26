package com.carddemo.programs.paudbunl;

import com.carddemo.programs.paudbunl.copybook.PendingAuthDetails;
import com.carddemo.programs.paudbunl.support.CobolData;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Arrays;

/**
 * FD OPFILE2 record area (LRECL 206):
 * <pre>
 * 01 OPFIL2-REC.
 *    05 ROOT-SEG-KEY   PIC S9(11) COMP-3.   6 bytes
 *    05 CHILD-SEG-REC  PIC X(200).
 * </pre>
 */
public final class Opfil2Rec {

    public static final int ROOT_SEG_KEY_DIGITS = 11;
    public static final int ROOT_SEG_KEY_LENGTH = CobolData.packedLength(ROOT_SEG_KEY_DIGITS);
    public static final int CHILD_SEG_REC_LENGTH = PendingAuthDetails.LENGTH;
    public static final int LENGTH = ROOT_SEG_KEY_LENGTH + CHILD_SEG_REC_LENGTH;

    private final byte[] area = new byte[LENGTH];
    private final Charset charset;

    public Opfil2Rec(Charset charset) {
        this.charset = charset;
    }

    /** INITIALIZE ROOT-SEG-KEY: packed zero (X'00000000000C'). */
    public void initializeRootSegKey() {
        CobolData.encodePacked(BigDecimal.ZERO, ROOT_SEG_KEY_DIGITS, 0, area, 0);
    }

    /** INITIALIZE CHILD-SEG-REC: spaces. */
    public void initializeChildSegRec() {
        CobolData.encodeAlpha("", area, ROOT_SEG_KEY_LENGTH, CHILD_SEG_REC_LENGTH, charset);
    }

    /** MOVE PA-ACCT-ID TO ROOT-SEG-KEY: same PIC and USAGE, so the 6 packed bytes are copied. */
    public void moveRootSegKey(byte[] summarySegment, int offset) {
        System.arraycopy(summarySegment, offset, area, 0, ROOT_SEG_KEY_LENGTH);
    }

    /** MOVE PENDING-AUTH-DETAILS TO CHILD-SEG-REC (group move, 200 bytes). */
    public void moveChildSegRec(byte[] detailsSegment) {
        System.arraycopy(detailsSegment, 0, area, ROOT_SEG_KEY_LENGTH, CHILD_SEG_REC_LENGTH);
    }

    public byte[] rootSegKey() {
        return Arrays.copyOfRange(area, 0, ROOT_SEG_KEY_LENGTH);
    }

    public byte[] childSegRec() {
        return Arrays.copyOfRange(area, ROOT_SEG_KEY_LENGTH, LENGTH);
    }

    public byte[] toBytes() {
        return area.clone();
    }

    /** Splits a 206-byte OUTFIL2 record back into its two fields. */
    public static BigDecimal rootSegKeyOf(byte[] record) {
        return CobolData.decodePacked(record, 0, ROOT_SEG_KEY_LENGTH, 0);
    }

    public static byte[] childSegRecOf(byte[] record) {
        return Arrays.copyOfRange(record, ROOT_SEG_KEY_LENGTH, LENGTH);
    }
}
