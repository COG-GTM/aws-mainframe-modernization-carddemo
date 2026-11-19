package com.carddemo.transaction.service;

import com.carddemo.account.domain.Account;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.card.domain.CardXref;
import com.carddemo.card.repository.CardXrefRepository;
import com.carddemo.common.cobol.CobolDecimal;
import com.carddemo.transaction.domain.DailyTransaction;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Optional;

/** {@code CBTRN02C 1500-VALIDATE-TRAN} (cbl:370-420): FR-POSTTRAN-02..05, PR-CBTRN02C-03..07. */
@Service
public class DailyTransactionValidator {

    /** {@code WS-TEMP-BAL PIC S9(09)V99} (cbl:187). */
    static final int TEMP_BAL_INTEGER_DIGITS = 9;
    /** {@code ACCT-EXPIRAION-DATE PIC X(10)} (CVACT01Y). */
    static final int EXPIRATION_DATE_LENGTH = 10;

    private final CardXrefRepository xrefs;
    private final AccountRepository accounts;

    public DailyTransactionValidator(CardXrefRepository xrefs, AccountRepository accounts) {
        this.xrefs = xrefs;
        this.accounts = accounts;
    }

    /**
     * Reason starts at 0 / spaces for every record (cbl:208-209). {@code 1500-A-LOOKUP-XREF} runs first; the account
     * checks of {@code 1500-B-LOOKUP-ACCT} run only when the card was found (cbl:372-376).
     */
    public ValidationResult validate(DailyTransaction transaction) {
        Optional<CardXref> xref = xrefs.findById(transaction.cardNum().stripTrailing());
        if (xref.isEmpty()) {
            return new ValidationResult(ValidationResult.INVALID_CARD, "INVALID CARD NUMBER FOUND", 0L);
        }
        return validateAccount(transaction, xref.get().getAcctId());
    }

    /**
     * {@code 1500-B-LOOKUP-ACCT} (cbl:393-420). Not found: 101. Otherwise the over-limit check (102) and then the expiry
     * check (103) both run, so 103 overwrites 102 (quirk Q-02).
     */
    ValidationResult validateAccount(DailyTransaction transaction, long xrefAcctId) {
        Optional<Account> found = accounts.findById(xrefAcctId);
        if (found.isEmpty()) {
            return new ValidationResult(ValidationResult.ACCOUNT_NOT_FOUND, "ACCOUNT RECORD NOT FOUND", xrefAcctId);
        }
        Account account = found.get();
        ValidationResult result = ValidationResult.valid(xrefAcctId);
        if (isOverLimit(account, transaction.amount())) {
            result = new ValidationResult(ValidationResult.OVERLIMIT, "OVERLIMIT TRANSACTION", xrefAcctId);
        }
        if (isAfterExpiration(account.getExpirationDate(), transaction.origTs())) {
            result = new ValidationResult(ValidationResult.AFTER_EXPIRATION, "TRANSACTION RECEIVED AFTER ACCT EXPIRATION",
                    xrefAcctId);
        }
        return result;
    }

    /**
     * cbl:403-412: {@code COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT}, then over limit
     * unless {@code ACCT-CREDIT-LIMIT >= WS-TEMP-BAL}. The COMPUTE has no {@code ON SIZE ERROR}: the result keeps only the
     * nine low-order integer digits of {@code PIC S9(09)V99}.
     */
    static boolean isOverLimit(Account account, BigDecimal amount) {
        BigDecimal tempBal = CobolDecimal.store(account.getCurrCycCredit().subtract(account.getCurrCycDebit()).add(amount),
                TEMP_BAL_INTEGER_DIGITS, 2);
        return account.getCreditLimit().compareTo(tempBal) < 0;
    }

    /**
     * cbl:414-419: {@code ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)} is an alphanumeric comparison of two
     * {@code X(10)} values (D-004), so it is a character comparison of the space-padded texts.
     */
    static boolean isAfterExpiration(String expirationDate, String origTs) {
        String expiration = padRight(expirationDate, EXPIRATION_DATE_LENGTH);
        return expiration.compareTo(origTs.substring(0, EXPIRATION_DATE_LENGTH)) < 0;
    }

    private static String padRight(String value, int length) {
        return value.length() >= length ? value : value + " ".repeat(length - value.length());
    }
}
