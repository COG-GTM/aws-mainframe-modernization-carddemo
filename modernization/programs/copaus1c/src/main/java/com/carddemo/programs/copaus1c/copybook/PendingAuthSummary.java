package com.carddemo.programs.copaus1c.copybook;

import java.math.BigDecimal;
import java.util.Arrays;

/** CIPAUSMY - IMS root segment PAUTSUM0 (PENDING-AUTH-SUMMARY), 100 bytes. */
public final class PendingAuthSummary {

    public static final int LENGTH = 100;

    private long acctId;                 // PA-ACCT-ID          PIC S9(11) COMP-3
    private long custId;                 // PA-CUST-ID          PIC 9(09)
    private char authStatus;             // PA-AUTH-STATUS      PIC X(01)
    private String[] accountStatus;      // PA-ACCOUNT-STATUS   PIC X(02) OCCURS 5
    private BigDecimal creditLimit;      // PA-CREDIT-LIMIT     PIC S9(09)V99 COMP-3
    private BigDecimal cashLimit;        // PA-CASH-LIMIT       PIC S9(09)V99 COMP-3
    private BigDecimal creditBalance;    // PA-CREDIT-BALANCE   PIC S9(09)V99 COMP-3
    private BigDecimal cashBalance;      // PA-CASH-BALANCE     PIC S9(09)V99 COMP-3
    private int approvedAuthCnt;         // PA-APPROVED-AUTH-CNT PIC S9(04) COMP
    private int declinedAuthCnt;         // PA-DECLINED-AUTH-CNT PIC S9(04) COMP
    private BigDecimal approvedAuthAmt;  // PA-APPROVED-AUTH-AMT PIC S9(09)V99 COMP-3
    private BigDecimal declinedAuthAmt;  // PA-DECLINED-AUTH-AMT PIC S9(09)V99 COMP-3
    private byte[] filler;               // FILLER PIC X(34)

    private PendingAuthSummary() {
    }

    public static PendingAuthSummary of(long acctId, long custId) {
        PendingAuthSummary s = fromEbcdic(blankImage());
        s.acctId = acctId;
        s.custId = custId;
        return s;
    }

    private static byte[] blankImage() {
        byte[] rec = new byte[LENGTH];
        PendingAuthSummary s = new PendingAuthSummary();
        s.authStatus = ' ';
        s.accountStatus = new String[] {"  ", "  ", "  ", "  ", "  "};
        s.creditLimit = s.cashLimit = s.creditBalance = s.cashBalance = BigDecimal.ZERO.setScale(2);
        s.approvedAuthAmt = s.declinedAuthAmt = BigDecimal.ZERO.setScale(2);
        s.filler = Cobol.spaces(34).getBytes(Cobol.EBCDIC);
        s.encodeInto(rec);
        return rec;
    }

    public static PendingAuthSummary fromEbcdic(byte[] rec) {
        if (rec.length != LENGTH) {
            throw new IllegalArgumentException("PAUTSUM0 segment must be 100 bytes, got " + rec.length);
        }
        PendingAuthSummary s = new PendingAuthSummary();
        s.acctId = Cobol.readPacked(rec, 0, 6, 0).longValue();
        s.custId = Cobol.displayValue(Cobol.readX(rec, 6, 9));
        s.authStatus = Cobol.readX(rec, 15, 1).charAt(0);
        s.accountStatus = new String[5];
        for (int i = 0; i < 5; i++) {
            s.accountStatus[i] = Cobol.readX(rec, 16 + i * 2, 2);
        }
        s.creditLimit = Cobol.readPacked(rec, 26, 6, 2);
        s.cashLimit = Cobol.readPacked(rec, 32, 6, 2);
        s.creditBalance = Cobol.readPacked(rec, 38, 6, 2);
        s.cashBalance = Cobol.readPacked(rec, 44, 6, 2);
        s.approvedAuthCnt = Cobol.readBinary(rec, 50, 2);
        s.declinedAuthCnt = Cobol.readBinary(rec, 52, 2);
        s.approvedAuthAmt = Cobol.readPacked(rec, 54, 6, 2);
        s.declinedAuthAmt = Cobol.readPacked(rec, 60, 6, 2);
        s.filler = Arrays.copyOfRange(rec, 66, 100);
        return s;
    }

    public byte[] toEbcdic() {
        byte[] rec = new byte[LENGTH];
        encodeInto(rec);
        return rec;
    }

    private void encodeInto(byte[] rec) {
        Cobol.writePacked(rec, 0, 6, 0, BigDecimal.valueOf(acctId));
        Cobol.writeX(rec, 6, 9, Cobol.nine(custId, 9));
        Cobol.writeX(rec, 15, 1, String.valueOf(authStatus));
        for (int i = 0; i < 5; i++) {
            Cobol.writeX(rec, 16 + i * 2, 2, accountStatus[i]);
        }
        Cobol.writePacked(rec, 26, 6, 2, creditLimit);
        Cobol.writePacked(rec, 32, 6, 2, cashLimit);
        Cobol.writePacked(rec, 38, 6, 2, creditBalance);
        Cobol.writePacked(rec, 44, 6, 2, cashBalance);
        Cobol.writeBinary(rec, 50, 2, approvedAuthCnt);
        Cobol.writeBinary(rec, 52, 2, declinedAuthCnt);
        Cobol.writePacked(rec, 54, 6, 2, approvedAuthAmt);
        Cobol.writePacked(rec, 60, 6, 2, declinedAuthAmt);
        System.arraycopy(filler, 0, rec, 66, 34);
    }

    public PendingAuthSummary copy() {
        return fromEbcdic(toEbcdic());
    }

    public long getAcctId() { return acctId; }
    public void setAcctId(long v) { acctId = v; }
    public long getCustId() { return custId; }
    public void setCustId(long v) { custId = v; }
    public char getAuthStatus() { return authStatus; }
    public void setAuthStatus(char v) { authStatus = v; }
    public String getAccountStatus(int index1) { return accountStatus[index1 - 1]; }
    public void setAccountStatus(int index1, String v) { accountStatus[index1 - 1] = Cobol.x(v, 2); }
    public BigDecimal getCreditLimit() { return creditLimit; }
    public void setCreditLimit(BigDecimal v) { creditLimit = v.setScale(2, java.math.RoundingMode.DOWN); }
    public BigDecimal getCashLimit() { return cashLimit; }
    public void setCashLimit(BigDecimal v) { cashLimit = v.setScale(2, java.math.RoundingMode.DOWN); }
    public BigDecimal getCreditBalance() { return creditBalance; }
    public void setCreditBalance(BigDecimal v) { creditBalance = v.setScale(2, java.math.RoundingMode.DOWN); }
    public BigDecimal getCashBalance() { return cashBalance; }
    public void setCashBalance(BigDecimal v) { cashBalance = v.setScale(2, java.math.RoundingMode.DOWN); }
    public int getApprovedAuthCnt() { return approvedAuthCnt; }
    public void setApprovedAuthCnt(int v) { approvedAuthCnt = v % 10000; }
    public int getDeclinedAuthCnt() { return declinedAuthCnt; }
    public void setDeclinedAuthCnt(int v) { declinedAuthCnt = v % 10000; }
    public BigDecimal getApprovedAuthAmt() { return approvedAuthAmt; }
    public void setApprovedAuthAmt(BigDecimal v) { approvedAuthAmt = v.setScale(2, java.math.RoundingMode.DOWN); }
    public BigDecimal getDeclinedAuthAmt() { return declinedAuthAmt; }
    public void setDeclinedAuthAmt(BigDecimal v) { declinedAuthAmt = v.setScale(2, java.math.RoundingMode.DOWN); }
}
