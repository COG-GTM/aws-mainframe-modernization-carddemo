package com.carddemo.interest.domain;

import com.carddemo.recordio.layout.Transaction;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * CBACT04C 1300-B-WRITE-TX (lines 473-517): builds the system-generated interest transaction.
 * <ul>
 *   <li>TRAN-ID = PARM-DATE (10 chars) + a 6-digit run-wide sequence starting at 000001</li>
 *   <li>type '01', category 0005, source 'System', description 'Int. for a/c ' + account id</li>
 *   <li>merchant id 0, merchant name/city/zip spaces, card = first card of the account (XREF AIX)</li>
 *   <li>original and processing timestamps both = now, DB2 format</li>
 * </ul>
 */
public final class InterestTransactionFactory {

    public static final String TYPE_CODE = "01";
    public static final int CATEGORY_CODE = 5;
    public static final String SOURCE = "System";
    private static final DateTimeFormatter DB2_TS = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH.mm.ss.SS'0000'");

    private final String runDate;
    private final Clock clock;
    private int sequence;

    public InterestTransactionFactory(String runDate, Clock clock) {
        if (runDate.length() != 10) {
            throw new IllegalArgumentException("PARM-DATE must be exactly 10 characters, got '" + runDate + "'");
        }
        this.runDate = runDate;
        this.clock = clock;
    }

    public Transaction next(String accountId, String cardNumber, BigDecimal monthlyInterest) {
        sequence++;
        String now = LocalDateTime.now(clock).format(DB2_TS);
        return new Transaction(
                runDate + String.format("%06d", sequence % 1_000_000),
                TYPE_CODE, CATEGORY_CODE, SOURCE,
                "Int. for a/c " + accountId,
                monthlyInterest, 0L, "", "", "",
                cardNumber, now, now);
    }
}
