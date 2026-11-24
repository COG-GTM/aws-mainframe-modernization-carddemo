package com.carddemo.batch;

import com.carddemo.common.domain.Account;
import com.carddemo.common.domain.AccountRepository;
import com.carddemo.common.domain.CardTransaction;
import com.carddemo.common.domain.CardTransactionRepository;
import com.carddemo.common.domain.CardXref;
import com.carddemo.common.domain.CardXrefRepository;
import com.carddemo.common.domain.DailyTransaction;
import com.carddemo.common.domain.DailyTransactionReject;
import com.carddemo.common.domain.DailyTransactionRejectRepository;
import com.carddemo.common.domain.DailyTransactionRepository;
import com.carddemo.common.domain.TranCatBalance;
import com.carddemo.common.domain.TranCatBalanceId;
import com.carddemo.common.domain.TranCatBalanceRepository;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * POSTTRAN / CBTRN02C: validates and posts the day's transactions (DALYTRAN) to TRANSACT, TCATBALF and ACCTDAT,
 * writing failures to DALYREJS with the same reason codes.
 */
@Service
public class PostTransactionsService {

    public static final String PENDING = "PENDING";
    public static final String POSTED = "POSTED";
    public static final String REJECTED = "REJECTED";

    private final DailyTransactionRepository daily;
    private final DailyTransactionRejectRepository rejects;
    private final CardXrefRepository xrefs;
    private final AccountRepository accounts;
    private final TranCatBalanceRepository balances;
    private final CardTransactionRepository transactions;
    private final Clock clock;

    public PostTransactionsService(DailyTransactionRepository daily, DailyTransactionRejectRepository rejects,
                                   CardXrefRepository xrefs, AccountRepository accounts,
                                   TranCatBalanceRepository balances, CardTransactionRepository transactions,
                                   Optional<Clock> clock) {
        this.daily = daily;
        this.rejects = rejects;
        this.xrefs = xrefs;
        this.accounts = accounts;
        this.balances = balances;
        this.transactions = transactions;
        this.clock = clock.orElse(Clock.systemDefaultZone());
    }

    record Validation(int code, String reason) {
        static final Validation OK = new Validation(0, "");
    }

    @Transactional
    public JobResult run() {
        String batchId = UUID.randomUUID().toString();
        LocalDateTime now = LocalDateTime.now(clock).withNano(0);
        int processed = 0;
        int rejected = 0;
        for (DailyTransaction t : daily.findByStatusOrderByTranIdAsc(PENDING)) {
            processed++;
            t.setBatchId(batchId);
            Optional<CardXref> xref = xrefs.findById(t.getCardNum());
            Optional<Account> account = xref.flatMap(x -> accounts.findById(x.getAcctId()));
            Validation v = validate(xref, account, t);
            if (v.code() == 0) {
                post(t, xref.get(), account.get(), now);
                t.setStatus(POSTED);
            } else {
                rejected++;
                rejects.save(new DailyTransactionReject(t.getTranId(), t.getCardNum(), t.getAmount(), v.code(),
                        v.reason(), batchId, now));
                t.setStatus(REJECTED);
            }
        }
        // CBTRN02C sets RETURN-CODE 4 when any record was rejected.
        int rc = rejected > 0 ? 4 : 0;
        return JobResult.of("POSTTRAN", rc, null, "batchId", batchId, "transactionsProcessed", processed,
                "transactionsRejected", rejected);
    }

    /**
     * 1500-VALIDATE-TRAN. The over-limit and expiration checks both run and the later one overwrites the reason,
     * so an over-limit transaction on an expired account is reported as 103.
     */
    static Validation validate(Optional<CardXref> xref, Optional<Account> account, DailyTransaction t) {
        if (xref.isEmpty()) {
            return new Validation(100, "INVALID CARD NUMBER FOUND");
        }
        if (account.isEmpty()) {
            return new Validation(101, "ACCOUNT RECORD NOT FOUND");
        }
        Account a = account.get();
        Validation v = Validation.OK;
        BigDecimal projected = a.getCurrCycCredit().subtract(a.getCurrCycDebit()).add(t.getAmount());
        if (a.getCreditLimit().compareTo(projected) < 0) {
            v = new Validation(102, "OVERLIMIT TRANSACTION");
        }
        if (a.getExpirationDate() != null && a.getExpirationDate().isBefore(t.getOrigTs().toLocalDate())) {
            v = new Validation(103, "TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
        }
        return v;
    }

    /** 2000-POST-TRANSACTION: 2700-UPDATE-TCATBAL, 2800-UPDATE-ACCOUNT-REC, 2900-WRITE-TRANSACTION-FILE. */
    private void post(DailyTransaction t, CardXref xref, Account account, LocalDateTime now) {
        TranCatBalanceId key = new TranCatBalanceId(xref.getAcctId(), t.getTypeCd(), t.getCatCd());
        TranCatBalance bal = balances.findById(key).orElseGet(() -> {
            TranCatBalance b = new TranCatBalance();
            b.setAcctId(key.getAcctId());
            b.setTypeCd(t.getTypeCd());
            b.setCatCd(t.getCatCd());
            b.setBalance(BigDecimal.ZERO);
            return b;
        });
        bal.setBalance(bal.getBalance().add(t.getAmount()));
        balances.save(bal);

        account.setCurrBal(account.getCurrBal().add(t.getAmount()));
        if (t.getAmount().signum() >= 0) {
            account.setCurrCycCredit(account.getCurrCycCredit().add(t.getAmount()));
        } else {
            account.setCurrCycDebit(account.getCurrCycDebit().add(t.getAmount()));
        }

        CardTransaction posted = new CardTransaction();
        posted.setTranId(t.getTranId());
        posted.setTypeCd(t.getTypeCd());
        posted.setCatCd(t.getCatCd());
        posted.setSource(t.getSource());
        posted.setDescription(t.getDescription());
        posted.setAmount(t.getAmount());
        posted.setMerchantId(t.getMerchantId());
        posted.setMerchantName(t.getMerchantName());
        posted.setMerchantCity(t.getMerchantCity());
        posted.setMerchantZip(t.getMerchantZip());
        posted.setCardNum(t.getCardNum());
        posted.setOrigTs(t.getOrigTs());
        posted.setProcTs(now);
        transactions.save(posted);
    }
}
