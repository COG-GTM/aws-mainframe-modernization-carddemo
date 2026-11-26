package com.carddemo.programs.cbpaup0c.copybook;

import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.decodeHalfword;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.decodePacked;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.decodePackedLong;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.decodeText;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.encodeHalfword;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.encodePacked;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.encodeText;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.fit;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Arrays;

/**
 * CIPAUSMY - IMS segment PAUTSUM0, pending authorization summary (root, 100 bytes).
 * Mutable because CBPAUP0C adjusts the counters/amounts in its working-storage copy.
 */
public final class PendingAuthSummary {

    public static final String SEGMENT_NAME = "PAUTSUM0";
    public static final int LENGTH = 100;
    public static final int ACCOUNT_STATUS_OCCURS = 5;

    private long accountId;                 // PA-ACCT-ID            S9(11) COMP-3   @0  (6)
    private String customerId = fit("", 9); // PA-CUST-ID            9(09)           @6  (9)
    private String authStatus = " ";        // PA-AUTH-STATUS        X(01)           @15 (1)
    private final String[] accountStatus = {"  ", "  ", "  ", "  ", "  "}; // PA-ACCOUNT-STATUS X(02) OCCURS 5 @16 (10)
    private BigDecimal creditLimit = BigDecimal.ZERO.setScale(2);    // PA-CREDIT-LIMIT   S9(09)V99 COMP-3 @26 (6)
    private BigDecimal cashLimit = BigDecimal.ZERO.setScale(2);      // PA-CASH-LIMIT     S9(09)V99 COMP-3 @32 (6)
    private BigDecimal creditBalance = BigDecimal.ZERO.setScale(2);  // PA-CREDIT-BALANCE S9(09)V99 COMP-3 @38 (6)
    private BigDecimal cashBalance = BigDecimal.ZERO.setScale(2);    // PA-CASH-BALANCE   S9(09)V99 COMP-3 @44 (6)
    private int approvedAuthCount;          // PA-APPROVED-AUTH-CNT  S9(04) COMP     @50 (2)
    private int declinedAuthCount;          // PA-DECLINED-AUTH-CNT  S9(04) COMP     @52 (2)
    private BigDecimal approvedAuthAmount = BigDecimal.ZERO.setScale(2); // PA-APPROVED-AUTH-AMT S9(09)V99 COMP-3 @54 (6)
    private BigDecimal declinedAuthAmount = BigDecimal.ZERO.setScale(2); // PA-DECLINED-AUTH-AMT S9(09)V99 COMP-3 @60 (6)
    private String filler = fit("", 34);    // FILLER                X(34)           @66 (34)

    public static PendingAuthSummary fromBytes(byte[] buf, Charset charset) {
        if (buf.length < LENGTH) {
            throw new IllegalArgumentException("PAUTSUM0 segment must be " + LENGTH + " bytes, got " + buf.length);
        }
        PendingAuthSummary s = new PendingAuthSummary();
        s.accountId = decodePackedLong(buf, 0, 6);
        s.customerId = decodeText(buf, 6, 9, charset);
        s.authStatus = decodeText(buf, 15, 1, charset);
        for (int i = 0; i < ACCOUNT_STATUS_OCCURS; i++) {
            s.accountStatus[i] = decodeText(buf, 16 + i * 2, 2, charset);
        }
        s.creditLimit = decodePacked(buf, 26, 6, 2);
        s.cashLimit = decodePacked(buf, 32, 6, 2);
        s.creditBalance = decodePacked(buf, 38, 6, 2);
        s.cashBalance = decodePacked(buf, 44, 6, 2);
        s.approvedAuthCount = decodeHalfword(buf, 50);
        s.declinedAuthCount = decodeHalfword(buf, 52);
        s.approvedAuthAmount = decodePacked(buf, 54, 6, 2);
        s.declinedAuthAmount = decodePacked(buf, 60, 6, 2);
        s.filler = decodeText(buf, 66, 34, charset);
        return s;
    }

    public byte[] toBytes(Charset charset) {
        byte[] buf = new byte[LENGTH];
        encodePacked(accountId, buf, 0, 6);
        encodeText(customerId, buf, 6, 9, charset);
        encodeText(authStatus, buf, 15, 1, charset);
        for (int i = 0; i < ACCOUNT_STATUS_OCCURS; i++) {
            encodeText(accountStatus[i], buf, 16 + i * 2, 2, charset);
        }
        encodePacked(creditLimit, 2, buf, 26, 6);
        encodePacked(cashLimit, 2, buf, 32, 6);
        encodePacked(creditBalance, 2, buf, 38, 6);
        encodePacked(cashBalance, 2, buf, 44, 6);
        encodeHalfword(approvedAuthCount, buf, 50);
        encodeHalfword(declinedAuthCount, buf, 52);
        encodePacked(approvedAuthAmount, 2, buf, 54, 6);
        encodePacked(declinedAuthAmount, 2, buf, 60, 6);
        encodeText(filler, buf, 66, 34, charset);
        return buf;
    }

    public PendingAuthSummary copy() {
        PendingAuthSummary c = new PendingAuthSummary();
        c.accountId = accountId;
        c.customerId = customerId;
        c.authStatus = authStatus;
        System.arraycopy(accountStatus, 0, c.accountStatus, 0, ACCOUNT_STATUS_OCCURS);
        c.creditLimit = creditLimit;
        c.cashLimit = cashLimit;
        c.creditBalance = creditBalance;
        c.cashBalance = cashBalance;
        c.approvedAuthCount = approvedAuthCount;
        c.declinedAuthCount = declinedAuthCount;
        c.approvedAuthAmount = approvedAuthAmount;
        c.declinedAuthAmount = declinedAuthAmount;
        c.filler = filler;
        return c;
    }

    public long accountId() { return accountId; }
    public PendingAuthSummary accountId(long v) { accountId = v; return this; }
    public String customerId() { return customerId; }
    public PendingAuthSummary customerId(String v) { customerId = fit(v, 9); return this; }
    public String authStatus() { return authStatus; }
    public PendingAuthSummary authStatus(String v) { authStatus = fit(v, 1); return this; }
    public String accountStatus(int index) { return accountStatus[index]; }
    public PendingAuthSummary accountStatus(int index, String v) { accountStatus[index] = fit(v, 2); return this; }
    public String[] accountStatuses() { return Arrays.copyOf(accountStatus, ACCOUNT_STATUS_OCCURS); }
    public BigDecimal creditLimit() { return creditLimit; }
    public PendingAuthSummary creditLimit(BigDecimal v) { creditLimit = v; return this; }
    public BigDecimal cashLimit() { return cashLimit; }
    public PendingAuthSummary cashLimit(BigDecimal v) { cashLimit = v; return this; }
    public BigDecimal creditBalance() { return creditBalance; }
    public PendingAuthSummary creditBalance(BigDecimal v) { creditBalance = v; return this; }
    public BigDecimal cashBalance() { return cashBalance; }
    public PendingAuthSummary cashBalance(BigDecimal v) { cashBalance = v; return this; }
    public int approvedAuthCount() { return approvedAuthCount; }
    public PendingAuthSummary approvedAuthCount(int v) { approvedAuthCount = v; return this; }
    public int declinedAuthCount() { return declinedAuthCount; }
    public PendingAuthSummary declinedAuthCount(int v) { declinedAuthCount = v; return this; }
    public BigDecimal approvedAuthAmount() { return approvedAuthAmount; }
    public PendingAuthSummary approvedAuthAmount(BigDecimal v) { approvedAuthAmount = v; return this; }
    public BigDecimal declinedAuthAmount() { return declinedAuthAmount; }
    public PendingAuthSummary declinedAuthAmount(BigDecimal v) { declinedAuthAmount = v; return this; }
    public String filler() { return filler; }
}
