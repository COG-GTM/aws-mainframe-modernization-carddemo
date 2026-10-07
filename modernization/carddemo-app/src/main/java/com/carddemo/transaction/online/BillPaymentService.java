package com.carddemo.transaction.online;

import com.carddemo.account.Account;
import com.carddemo.account.AccountRepository;
import com.carddemo.card.CardXref;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.AbendException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.Versions;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.TransactionRecord;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.function.Supplier;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * COBIL00C (CB00): show the balance, then on confirm pay it in full. The payment is one database transaction: the
 * account row is locked ({@code READ UPDATE} = {@code SELECT ... FOR UPDATE}), its version re-checked against the
 * balance the user confirmed, the bill-payment transaction written and the balance zeroed; any failure rolls both
 * back (COBOL codes no SYNCPOINT ROLLBACK, R-17).
 */
@Component
public class BillPaymentService {

    private static final Logger log = LoggerFactory.getLogger(BillPaymentService.class);

    public static final String ACCOUNT_FIELD = "accountId";
    public static final String CONFIRM_FIELD = "confirm";
    public static final String VERSION_FIELD = "version";
    public static final String MSG_ACCOUNT_EMPTY = "Acct ID can NOT be empty...";
    public static final String MSG_INVALID_CONFIRM = "Invalid value. Valid values are (Y/N)...";
    public static final String MSG_NOT_FOUND = "Account ID NOT found...";
    public static final String MSG_LOOKUP_FAILED = "Unable to lookup Account...";
    public static final String MSG_NOTHING_TO_PAY = "You have nothing to pay...";
    public static final String MSG_CONFIRM = "Confirm to make a bill payment...";
    public static final String MSG_XREF_FAILED = "Unable to lookup XREF AIX file...";
    public static final String MSG_ADD_FAILED = "Unable to Add Bill pay Transaction...";
    public static final String MSG_UPDATE_FAILED = "Unable to Update Account...";
    public static final String MSG_VERSION_REQUIRED =
            "version must be supplied with confirm=Y: send the version returned with the balance shown";
    static final String MSG_PAID = "Payment successful.  Your Transaction ID is %s.";

    /** R-12: the fixed fields of the bill-payment record. */
    public static final String TYPE_CODE = "02";
    public static final int CATEGORY_CODE = 2;
    public static final String SOURCE = "POS TERM";
    public static final String DESCRIPTION = "BILL PAYMENT - ONLINE";
    public static final int MERCHANT_ID = 999_999_999;
    public static final String MERCHANT_NAME = "BILL PAYMENT";
    public static final String MERCHANT_CITY = "N/A";
    public static final String MERCHANT_ZIP = "N/A";

    /** R-13: {@code YYYY-MM-DD HH:MM:SS.000000}. */
    static final DateTimeFormatter TIMESTAMP = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss'.000000'");

    public enum State {
        /** CONFIRM blank: balance shown, awaiting Y (R-11). */
        SHOW,
        /** CONFIRM N: screen cleared, nothing read or written (R-8). */
        CLEARED,
        /** CONFIRM Y: transaction written and balance zeroed (R-12, R-20). */
        PAID
    }

    /**
     * @param account     the account read (null when {@link State#CLEARED})
     * @param transaction the bill-payment record written, null unless {@link State#PAID}
     */
    public record Outcome(State state, Account account, Transaction transaction, String message) {
    }

    private final AccountRepository accounts;
    private final CardXrefRepository xrefs;
    private final TransactionIds ids;
    private final Clock clock;

    public BillPaymentService(AccountRepository accounts, CardXrefRepository xrefs, TransactionIds ids, Clock clock) {
        this.accounts = accounts;
        this.xrefs = xrefs;
        this.ids = ids;
        this.clock = clock;
    }

    public static String paid(String tranId) {
        return String.format(MSG_PAID, tranId);
    }

    @Transactional
    public Outcome pay(String accountId, String confirm, Long version) {
        if (ScreenInput.isSpacesOrLowValues(accountId)) {
            throw new InvalidRequestException(ACCOUNT_FIELD, MSG_ACCOUNT_EMPTY);
        }
        char answer = TransactionAddService.confirmation(confirm);
        boolean blank = ScreenInput.isSpacesOrLowValues(confirm);
        if (answer == '?') {
            throw new InvalidRequestException(CONFIRM_FIELD, MSG_INVALID_CONFIRM);
        }
        if (answer == 'N' && !blank) {
            return new Outcome(State.CLEARED, null, null, "");
        }
        long acctId = accountKey(accountId);
        if (blank) {
            Account account = read(() -> accounts.findById(acctId), MSG_LOOKUP_FAILED)
                    .orElseThrow(() -> new RecordNotFoundException(MSG_NOT_FOUND));
            requireSomethingToPay(account);
            return new Outcome(State.SHOW, account, null, MSG_CONFIRM);
        }
        if (version == null) {
            throw new InvalidRequestException(VERSION_FIELD, MSG_VERSION_REQUIRED);
        }
        long locked = read(() -> accounts.lockVersion(acctId), MSG_LOOKUP_FAILED)
                .orElseThrow(() -> new RecordNotFoundException(MSG_NOT_FOUND));
        Versions.requireCurrent(Account.class, acctId, version, locked);
        Account account = read(() -> accounts.findById(acctId), MSG_LOOKUP_FAILED)
                .orElseThrow(() -> new RecordNotFoundException(MSG_NOT_FOUND));
        requireSomethingToPay(account);
        CardXref xref = read(() -> xrefs.findFirstByAcctIdOrderByCardNumAsc(acctId), MSG_XREF_FAILED)
                .orElseThrow(() -> new RecordNotFoundException(MSG_NOT_FOUND));

        String now = LocalDateTime.now(clock).format(TIMESTAMP);
        BigDecimal amount = account.getCurrBal();
        Transaction written = ids.write(new TransactionRecord(ids.next(), TYPE_CODE, CATEGORY_CODE, SOURCE,
                DESCRIPTION, amount, MERCHANT_ID, MERCHANT_NAME, MERCHANT_CITY, MERCHANT_ZIP, xref.getCardNum(), now,
                now), MSG_ADD_FAILED);
        account.subtractFromBalance(amount);
        try {
            accounts.saveAndFlush(account);
        } catch (ObjectOptimisticLockingFailureException e) {
            throw e;
        } catch (DataAccessException e) {
            throw AbendException.carddemo(MSG_UPDATE_FAILED, e);
        }
        log.info("COBIL00C bill payment transaction {} account {} amount {}", written.getTranId(), acctId, amount);
        return new Outcome(State.PAID, account, written, paid(written.getTranId()));
    }

    /**
     * R-8: COBOL moves {@code ACTIDINI} into the key as typed; a key that is not up to 11 digits cannot match an
     * account, so it is NOTFND like the READ would answer.
     */
    static long accountKey(String accountId) {
        String typed = accountId.strip();
        if (!TransactionAddEdits.isNumeric(typed, 11)) {
            throw new RecordNotFoundException(MSG_NOT_FOUND);
        }
        return Long.parseLong(typed);
    }

    /** R-10: {@code ACCT-CURR-BAL <= ZEROS} → {@code You have nothing to pay...}. */
    private static void requireSomethingToPay(Account account) {
        if (account.getCurrBal().signum() <= 0) {
            throw new InvalidRequestException(ACCOUNT_FIELD, MSG_NOTHING_TO_PAY);
        }
    }

    private static <T> Optional<T> read(Supplier<Optional<T>> read, String failure) {
        try {
            return read.get();
        } catch (DataAccessException e) {
            throw AbendException.carddemo(failure, e);
        }
    }
}
