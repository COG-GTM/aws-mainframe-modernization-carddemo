package com.carddemo.transaction.batch.validation;

import com.carddemo.transaction.cobol.VsamIo;
import com.carddemo.transaction.domain.Account;
import com.carddemo.transaction.domain.CardXref;
import com.carddemo.transaction.repository.AccountRepository;
import com.carddemo.transaction.repository.CardXrefRepository;
import org.springframework.stereotype.Component;

import java.util.Optional;

/**
 * Keyed reads shared by CBTRN01C and CBTRN02C. {@code INVALID KEY} (status 23) is {@link Optional#empty()};
 * any other persistence failure becomes a {@link com.carddemo.transaction.cobol.CobolFileException}.
 */
@Component
public class XrefAccountLookup {

    private final CardXrefRepository xrefs;
    private final AccountRepository accounts;

    public XrefAccountLookup(CardXrefRepository xrefs, AccountRepository accounts) {
        this.xrefs = xrefs;
        this.accounts = accounts;
    }

    /** {@code READ XREF-FILE KEY IS FD-XREF-CARD-NUM}. */
    public Optional<CardXref> readXref(String cardNum) {
        return VsamIo.perform("XREFFILE", "ERROR READING CROSS REF FILE", () -> xrefs.findById(cardNum));
    }

    /** {@code READ ACCOUNT-FILE KEY IS FD-ACCT-ID}. */
    public Optional<Account> readAccount(long acctId) {
        return VsamIo.perform("ACCTFILE", "ERROR READING ACCOUNT FILE", () -> accounts.findById(acctId));
    }
}
