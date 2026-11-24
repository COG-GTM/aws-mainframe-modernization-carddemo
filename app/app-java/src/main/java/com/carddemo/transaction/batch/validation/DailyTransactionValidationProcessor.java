package com.carddemo.transaction.batch.validation;

import com.carddemo.transaction.cobol.Pic;
import com.carddemo.transaction.domain.Account;
import com.carddemo.transaction.domain.CardXref;
import com.carddemo.transaction.domain.DailyTransaction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.stereotype.Component;

import java.util.Optional;

/**
 * Body of the CBTRN01C MAIN-PARA loop for one DALYTRAN record.
 */
@Component
public class DailyTransactionValidationProcessor implements ItemProcessor<DailyTransaction, ValidationResult> {

    private static final Logger log = LoggerFactory.getLogger(DailyTransactionValidationProcessor.class);

    private final XrefAccountLookup lookup;

    public DailyTransactionValidationProcessor(XrefAccountLookup lookup) {
        this.lookup = lookup;
    }

    @Override
    public ValidationResult process(DailyTransaction tran) {
        log.debug("{}", tran.rawRecord());   // DISPLAY DALYTRAN-RECORD
        Optional<CardXref> xref = lookupXref(tran.cardNum());
        if (xref.isEmpty()) {
            log.warn("CARD NUMBER {} COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-{}", tran.cardNum(), tran.id());
            return new ValidationResult(tran, ValidationResult.Outcome.CARD_NOT_VERIFIED);
        }
        long acctId = xref.get().getAcctId();
        if (readAccount(acctId).isEmpty()) {
            log.warn("ACCOUNT {} NOT FOUND", Pic.zeroFill(acctId, 11));
            return new ValidationResult(tran, ValidationResult.Outcome.ACCOUNT_NOT_FOUND);
        }
        return new ValidationResult(tran, ValidationResult.Outcome.VERIFIED);
    }

    /** 2000-LOOKUP-XREF. */
    Optional<CardXref> lookupXref(String cardNum) {
        Optional<CardXref> xref = lookup.readXref(cardNum);
        if (xref.isEmpty()) {
            log.info("INVALID CARD NUMBER FOR XREF");
        } else {
            log.info("SUCCESSFUL READ OF XREF");
            log.info("CARD NUMBER: {}", xref.get().getCardNum());
            log.info("ACCOUNT ID : {}", Pic.zeroFill(xref.get().getAcctId(), 11));
            log.info("CUSTOMER ID: {}", Pic.zeroFill(xref.get().getCustId(), 9));
        }
        return xref;
    }

    /** 3000-READ-ACCOUNT. */
    Optional<Account> readAccount(long acctId) {
        Optional<Account> account = lookup.readAccount(acctId);
        log.info(account.isEmpty() ? "INVALID ACCOUNT NUMBER FOUND" : "SUCCESSFUL READ OF ACCOUNT FILE");
        return account;
    }
}
