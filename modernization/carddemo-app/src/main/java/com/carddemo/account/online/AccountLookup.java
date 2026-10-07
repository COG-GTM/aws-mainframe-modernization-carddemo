package com.carddemo.account.online;

import com.carddemo.account.Account;
import com.carddemo.account.AccountRepository;
import com.carddemo.card.CardXref;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.AbendException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.online.CicsFileErrors;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.customer.Customer;
import com.carddemo.customer.CustomerRepository;
import java.util.List;
import java.util.function.Supplier;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * {@code 9000-READ-ACCT} shared by {@code COACTVWC} and {@code COACTUPC}: {@code CXACAIX} (account → customer and card),
 * then {@code ACCTDAT}, then {@code CUSTDAT}, each {@code NOTFND} with the program's message.
 */
@Service
public class AccountLookup {

    /** Field name of the account id ({@code ACCTSID}) in error bodies. */
    public static final String ACCOUNT_ID_FIELD = "acctId";

    public static final String MSG_NO_INPUT = "No input received";
    /** {@code COACTVWC} {@code 2210-EDIT-ACCOUNT} (two spaces after "must", as in the source). */
    public static final String MSG_VIEW_ACCOUNT_INVALID = "Account Filter must  be a non-zero 11 digit number";
    /** {@code COACTUPC} {@code 1210-EDIT-ACCOUNT}. */
    public static final String MSG_UPDATE_ACCOUNT_INVALID =
            "Account Number if supplied must be a 11 digit Non-Zero Number";

    /** {@code DFHRESP(NOTFND)} and the {@code RESP2} of a keyed {@code READ} that finds no record. */
    static final int RESP_NOTFND = 13;
    static final int RESP2_NOTFND = 80;
    static final int RESP_IOERR = 17;
    public static final String CXACAIX = "CXACAIX";
    public static final String ACCTDAT = "ACCTDAT";
    public static final String CUSTDAT = "CUSTDAT";
    /** {@code WS-RETURN-MSG PIC X(75)}. */
    private static final int RETURN_MSG_LENGTH = 75;

    private final CardXrefRepository xrefs;
    private final AccountRepository accounts;
    private final CustomerRepository customers;

    public AccountLookup(CardXrefRepository xrefs, AccountRepository accounts, CustomerRepository customers) {
        this.xrefs = xrefs;
        this.accounts = accounts;
        this.customers = customers;
    }

    /**
     * COACTVWC {@code 2200-EDIT-MAP-INPUTS} with {@code 2210-EDIT-ACCOUNT} / COACTUPC {@code 1210-EDIT-ACCOUNT}: {@code
     * *} or spaces is "no input"; otherwise up to 11
     * digits, not all zeros ({@code CC-ACCT-ID PIC X(11)} tested {@code NUMERIC}; shorter input is the number
     * right-justified with leading zeros, as {@code CDEMO-ACCT-ID PIC 9(11)} holds it).
     */
    public static long accountId(String input, String invalidMessage) {
        String id = input == null ? "" : input.strip();
        if (id.isEmpty() || id.equals("*")) {
            throw new InvalidRequestException(ACCOUNT_ID_FIELD, MSG_NO_INPUT);
        }
        if (id.length() > 11 || !id.chars().allMatch(c -> c >= '0' && c <= '9') || Long.parseLong(id) == 0) {
            throw new InvalidRequestException(ACCOUNT_ID_FIELD, invalidMessage);
        }
        return Long.parseLong(id);
    }

    @Transactional(readOnly = true)
    public AccountDetails read(long acctId) {
        return find(acctId);
    }

    /** {@link #read(long)} inside the caller's transaction (the entities stay managed). */
    AccountDetails find(long acctId) {
        CardXref xref = read(CXACAIX, () -> xrefs.findFirstByAcctIdOrderByCardNumAsc(acctId))
                .orElseThrow(() -> new RecordNotFoundException(xrefNotFound(acctId)));
        Account account = read(ACCTDAT, () -> accounts.findById(acctId))
                .orElseThrow(() -> new RecordNotFoundException(accountNotFound(acctId)));
        Customer customer = read(CUSTDAT, () -> customers.findById(xref.getCustId()))
                .orElseThrow(() -> new RecordNotFoundException(customerNotFound(xref.getCustId())));
        List<String> cards = read(CXACAIX, () -> xrefs.findByAcctIdOrderByCardNumAsc(acctId)).stream()
                .map(CardXref::getCardNum).toList();
        return new AccountDetails(account, customer, xref.getCardNum(), cards);
    }

    /** A read that fails other than {@code NOTFND} ends the request with {@code WS-FILE-ERROR-MESSAGE}. */
    private static <T> T read(String dataset, Supplier<T> read) {
        try {
            return read.get();
        } catch (DataAccessException e) {
            throw new AbendException(AbendException.CARDDEMO_ABEND_CODE, fileError("READ", dataset), e);
        }
    }

    /**
     * {@code WS-FILE-ERROR-MESSAGE} moved to {@code WS-RETURN-MSG}; a database failure is reported as
     * {@code DFHRESP(IOERR)} with {@code RESP2} 0.
     */
    public static String fileError(String operation, String dataset) {
        return CicsFileErrors.message(operation, dataset);
    }

    /** {@code 9200-GETCARDXREF-BYACCT} {@code NOTFND}. */
    public static String xrefNotFound(long acctId) {
        return returnMessage("Account:" + id11(acctId) + " not found in" + " Cross ref file.  Resp:"
                + errorResp(RESP_NOTFND) + " Reas:" + errorResp(RESP2_NOTFND));
    }

    /** {@code 9300-GETACCTDATA-BYACCT} {@code NOTFND}. */
    public static String accountNotFound(long acctId) {
        return returnMessage("Account:" + id11(acctId) + " not found in" + " Acct Master file.Resp:"
                + errorResp(RESP_NOTFND) + " Reas:" + errorResp(RESP2_NOTFND));
    }

    /** {@code 9400-GETCUSTDATA-BYCUST} {@code NOTFND}. */
    public static String customerNotFound(int custId) {
        return returnMessage("CustId:" + String.format("%09d", custId) + " not found"
                + " in customer master.Resp: " + errorResp(RESP_NOTFND) + " REAS:" + errorResp(RESP2_NOTFND));
    }

    public static String id11(long acctId) {
        return String.format("%011d", acctId);
    }

    /** {@code MOVE WS-RESP-CD (S9(9) COMP) TO ERROR-RESP (X(10))}: nine unsigned digits and a space. */
    private static String errorResp(int value) {
        return String.format("%09d ", Math.abs(value));
    }

    /** {@code STRING ... DELIMITED BY SIZE INTO WS-RETURN-MSG}: cut at 75 characters, shown right-trimmed. */
    private static String returnMessage(String text) {
        return ScreenInput.rightTrim(text.length() > RETURN_MSG_LENGTH ? text.substring(0, RETURN_MSG_LENGTH) : text);
    }
}
