package com.carddemo.batch;

import com.carddemo.common.domain.Account;
import com.carddemo.common.domain.AccountRepository;
import com.carddemo.common.domain.CardTransaction;
import com.carddemo.common.domain.CardTransactionRepository;
import com.carddemo.common.domain.CardXref;
import com.carddemo.common.domain.CardXrefRepository;
import com.carddemo.common.domain.DisclosureGroup;
import com.carddemo.common.domain.DisclosureGroupId;
import com.carddemo.common.domain.DisclosureGroupRepository;
import com.carddemo.common.domain.TranCatBalance;
import com.carddemo.common.domain.TranCatBalanceRepository;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Optional;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** INTCALC / CBACT04C: monthly interest per transaction category balance, posted as system transactions. */
@Service
public class InterestCalculationService {

    static final String DEFAULT_GROUP = "DEFAULT";
    static final BigDecimal DIVISOR = new BigDecimal("1200");

    private final TranCatBalanceRepository balances;
    private final AccountRepository accounts;
    private final CardXrefRepository xrefs;
    private final DisclosureGroupRepository groups;
    private final CardTransactionRepository transactions;
    private final Clock clock;

    public InterestCalculationService(TranCatBalanceRepository balances, AccountRepository accounts,
                                      CardXrefRepository xrefs, DisclosureGroupRepository groups,
                                      CardTransactionRepository transactions, Optional<Clock> clock) {
        this.balances = balances;
        this.accounts = accounts;
        this.xrefs = xrefs;
        this.groups = groups;
        this.transactions = transactions;
        this.clock = clock.orElse(Clock.systemDefaultZone());
    }

    /** COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200 into PIC S9(09)V99 (truncating). */
    static BigDecimal monthlyInterest(BigDecimal balance, BigDecimal annualRate) {
        return balance.multiply(annualRate).divide(DIVISOR, 2, RoundingMode.DOWN);
    }

    /** @param runDate the JCL PARM date; it prefixes generated transaction ids exactly as PARM-DATE does. */
    @Transactional
    public JobResult run(LocalDate runDate) {
        String parmDate = runDate.toString();
        if (transactions.existsByTranIdStartingWithAndSource(parmDate, "System")) {
            // Interest ids are PARM-DATE + sequence, so a second run for the same date would double-charge.
            return JobResult.of("INTCALC", 8, "INTCALC already ran for " + parmDate, "runDate", parmDate,
                    "interestTransactions", 0);
        }
        LocalDateTime now = LocalDateTime.now(clock).withNano(0);
        int recordCount = 0;
        int suffix = 0;
        int accountsUpdated = 0;
        BigDecimal grandTotal = BigDecimal.ZERO;
        Account current = null;
        CardXref currentXref = null;
        BigDecimal accountInterest = BigDecimal.ZERO;
        for (TranCatBalance bal : balances.findAllByOrderByAcctIdAscTypeCdAscCatCdAsc()) {
            recordCount++;
            if (current == null || !current.getAcctId().equals(bal.getAcctId())) {
                if (current != null) {
                    updateAccount(current, accountInterest);
                    accountsUpdated++;
                }
                accountInterest = BigDecimal.ZERO;
                current = accounts.findById(bal.getAcctId()).orElse(null);
                currentXref = xrefs.findFirstByAcctIdOrderByCardNumAsc(bal.getAcctId()).orElse(null);
                if (current == null) {
                    continue;
                }
            }
            BigDecimal rate = rate(current.getGroupId(), bal);
            if (rate.signum() != 0) {
                BigDecimal interest = monthlyInterest(bal.getBalance(), rate);
                accountInterest = accountInterest.add(interest);
                grandTotal = grandTotal.add(interest);
                suffix++;
                writeInterest(parmDate, suffix, current, currentXref, interest, now);
            }
        }
        if (current != null) {
            updateAccount(current, accountInterest);
            accountsUpdated++;
        }
        return JobResult.of("INTCALC", 0, null, "runDate", parmDate, "categoryBalancesRead", recordCount,
                "interestTransactions", suffix, "accountsUpdated", accountsUpdated, "totalInterest", grandTotal);
    }

    /** 1200-GET-INTEREST-RATE with the 1200-A-GET-DEFAULT-INT-RATE fallback. */
    private BigDecimal rate(String groupId, TranCatBalance bal) {
        String group = groupId == null || groupId.isBlank() ? "" : groupId;
        return groups.findById(new DisclosureGroupId(group, bal.getTypeCd(), bal.getCatCd()))
                .or(() -> groups.findById(new DisclosureGroupId(DEFAULT_GROUP, bal.getTypeCd(), bal.getCatCd())))
                .map(DisclosureGroup::getIntRate)
                .orElse(BigDecimal.ZERO);
    }

    /** 1050-UPDATE-ACCOUNT: add interest, reset the cycle credit/debit accumulators. */
    private static void updateAccount(Account a, BigDecimal interest) {
        a.setCurrBal(a.getCurrBal().add(interest));
        a.setCurrCycCredit(BigDecimal.ZERO);
        a.setCurrCycDebit(BigDecimal.ZERO);
    }

    /** 1300-B-WRITE-TX. */
    private void writeInterest(String parmDate, int suffix, Account a, CardXref xref, BigDecimal interest,
                               LocalDateTime now) {
        CardTransaction t = new CardTransaction();
        t.setTranId(parmDate + String.format("%06d", suffix));
        t.setTypeCd("01");
        t.setCatCd(5);
        t.setSource("System");
        t.setDescription("Int. for a/c " + String.format("%011d", a.getAcctId()));
        t.setAmount(interest);
        t.setMerchantId(0L);
        t.setMerchantName("");
        t.setMerchantCity("");
        t.setMerchantZip("");
        t.setCardNum(xref == null ? null : xref.getCardNum());
        t.setOrigTs(now);
        t.setProcTs(now);
        transactions.save(t);
    }
}
