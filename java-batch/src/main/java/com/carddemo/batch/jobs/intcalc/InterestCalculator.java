package com.carddemo.batch.jobs.intcalc;

import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.model.AccountRecord;
import com.carddemo.batch.model.CardXrefRecord;
import com.carddemo.batch.model.DisclosureGroupRecord;
import com.carddemo.batch.model.DisclosureGroupRecord.DisGroupKey;
import com.carddemo.batch.model.TranCatBalRecord;
import com.carddemo.batch.model.TransactionRecord;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * Business logic of {@code CBACT04C} (JCL {@code INTCALC}): one call per TCATBALF record, in key order.
 * Paragraph names in the comments refer to the COBOL source.
 */
public class InterestCalculator {

    static final String DEFAULT_GROUP = "DEFAULT";
    private static final BigDecimal MONTHLY_RATE_DIVISOR = new BigDecimal("1200");
    private static final BigDecimal ZERO = BigDecimal.ZERO.setScale(2);
    private static final DateTimeFormatter DB2_TS = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH.mm.ss.SS");

    private final KsdsFile<Long, AccountRecord> accounts;
    private final Map<Long, CardXrefRecord> xrefByAcctId = new HashMap<>();
    private final KsdsFile<DisGroupKey, DisclosureGroupRecord> disclosureGroups;
    private final String parmDate;
    private final Clock clock;

    private Long lastAcctId;
    private AccountRecord account;
    private CardXrefRecord xref;
    private BigDecimal totalInterest = ZERO;
    private int tranIdSuffix;
    private int recordCount;

    public InterestCalculator(KsdsFile<Long, AccountRecord> accounts, KsdsFile<String, CardXrefRecord> xrefs,
                              KsdsFile<DisGroupKey, DisclosureGroupRecord> disclosureGroups, String parmDate,
                              Clock clock) {
        this.accounts = accounts;
        // XREFFILE alternate key FD-XREF-ACCT-ID: first card in primary-key order wins.
        xrefs.values().forEach(x -> xrefByAcctId.putIfAbsent(x.xrefAcctId(), x));
        this.disclosureGroups = disclosureGroups;
        this.parmDate = parmDate;
        this.clock = clock;
    }

    /** Main loop body for one TRAN-CAT-BAL-RECORD; returns the interest transaction, if any. */
    public Optional<TransactionRecord> process(TranCatBalRecord tcatBal) {
        recordCount++;
        long acctId = tcatBal.tranCatKey().trancatAcctId();
        if (lastAcctId == null || lastAcctId != acctId) {
            if (lastAcctId != null) {
                updateAccount();
            }
            totalInterest = ZERO;
            lastAcctId = acctId;
            account = accounts.read(acctId)
                    .orElseThrow(() -> new IntcalcAbendException("ACCOUNT NOT FOUND: " + acctId));
            xref = Optional.ofNullable(xrefByAcctId.get(acctId))
                    .orElseThrow(() -> new IntcalcAbendException("XREF NOT FOUND FOR ACCOUNT: " + acctId));
        }
        BigDecimal rate = interestRate(account.acctGroupId(), tcatBal.tranCatKey());
        if (rate.signum() == 0) {
            return Optional.empty();
        }
        // 1300-COMPUTE-INTEREST: COMPUTE without ROUNDED truncates into WS-MONTHLY-INT PIC S9(09)V99.
        BigDecimal monthly = fit(tcatBal.tranCatBal().multiply(rate)
                .divide(MONTHLY_RATE_DIVISOR, 2, RoundingMode.DOWN), 9, 2);
        totalInterest = fit(totalInterest.add(monthly), 9, 2);
        return Optional.of(interestTransaction(monthly));
        // 1400-COMPUTE-FEES is an empty paragraph in the COBOL.
    }

    /**
     * End of file. The COBOL main loop is {@code PERFORM UNTIL END-OF-FILE = 'Y'} and the EOF read happens
     * inside the {@code IF END-OF-FILE = 'N'} branch, so the {@code ELSE PERFORM 1050-UPDATE-ACCOUNT} branch
     * is never reached: the last account's interest is written to TRANSACT but never added to ACCTFILE.
     * Kept as-is for parity (see the edge golden output, account 104).
     */
    public void finish() {
    }

    public int recordCount() {
        return recordCount;
    }

    public int transactionCount() {
        return tranIdSuffix;
    }

    /** 1050-UPDATE-ACCOUNT. */
    private void updateAccount() {
        account = account.withAcctCurrBal(fit(account.acctCurrBal().add(totalInterest), 10, 2))
                .withAcctCurrCycCredit(ZERO)
                .withAcctCurrCycDebit(ZERO);
        accounts.rewrite(account);
    }

    /** 1200-GET-INTEREST-RATE with the 1200-A-GET-DEFAULT-INT-RATE fallback. */
    private BigDecimal interestRate(String groupId, TranCatBalRecord.TranCatKey key) {
        DisGroupKey lookup = new DisGroupKey(groupId, key.trancatTypeCd(), key.trancatCd());
        Optional<DisclosureGroupRecord> group = disclosureGroups.read(lookup);
        if (group.isEmpty()) {
            DisGroupKey fallback = new DisGroupKey(DEFAULT_GROUP, key.trancatTypeCd(), key.trancatCd());
            group = Optional.of(disclosureGroups.read(fallback)
                    .orElseThrow(() -> new IntcalcAbendException("ERROR READING DEFAULT DISCLOSURE GROUP " + fallback)));
        }
        return group.get().disIntRate();
    }

    /** 1300-B-WRITE-TX. */
    private TransactionRecord interestTransaction(BigDecimal monthly) {
        tranIdSuffix++;
        String tranId = parmDate + String.format("%06d", tranIdSuffix % 1_000_000);
        String ts = db2Timestamp();
        return new TransactionRecord(tranId, "01", 5, "System",
                "Int. for a/c " + String.format("%011d", account.acctId()), monthly, 0L, "", "", "",
                xref.xrefCardNum(), ts, ts, "");
    }

    /** Z-GET-DB2-FORMAT-TIMESTAMP: YYYY-MM-DD-HH.MM.SS.hh0000 from the injected clock. */
    private String db2Timestamp() {
        return LocalDateTime.now(clock).format(DB2_TS) + "0000";
    }

    /** Store into a PIC S9(intDigits)V9(scale) field without ON SIZE ERROR: truncate both ends. */
    static BigDecimal fit(BigDecimal value, int intDigits, int scale) {
        return value.setScale(scale, RoundingMode.DOWN).remainder(BigDecimal.TEN.pow(intDigits));
    }
}
