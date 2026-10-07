package com.carddemo.transaction.online;

import com.carddemo.common.AbendException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.codec.NumvalC;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import java.math.BigDecimal;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * COTRN02C (CT02) PROCESS-ENTER-KEY / COPY-LAST-TRAN-DATA / ADD-TRANSACTION: key edits, data edits, the CONFIRM
 * field, then id assignment and WRITE to the shared {@code transaction} table in one database transaction.
 */
@Component
public class TransactionAddService {

    private static final Logger log = LoggerFactory.getLogger(TransactionAddService.class);

    public static final String CONFIRM_FIELD = "confirm";
    public static final String MSG_CONFIRM = "Confirm to add this transaction...";
    public static final String MSG_INVALID_CONFIRM = "Invalid value. Valid values are (Y/N)...";
    public static final String MSG_ADD_FAILED = "Unable to Add Transaction...";
    static final String MSG_ADDED = "Transaction added successfully.  Your Tran ID is %s.";

    /** COTRN2A field lengths that a copied record is moved into (COPY-LAST-TRAN-DATA). */
    static final int SOURCE_LENGTH = 10;
    static final int DESCRIPTION_LENGTH = 60;
    static final int MERCHANT_NAME_LENGTH = 30;
    static final int MERCHANT_CITY_LENGTH = 25;
    static final int MERCHANT_ZIP_LENGTH = 10;

    public enum State {
        /** CONFIRM blank or N: everything edited, nothing written (R-27b). */
        VALIDATED,
        /** CONFIRM Y: written (R-30). */
        ADDED
    }

    /**
     * @param form        the input fields after the edits (keys resolved, amount re-edited)
     * @param transaction the record written, null unless {@link State#ADDED}
     * @param message     {@code ERRMSG}
     */
    public record Outcome(State state, TransactionForm form, Transaction transaction, String message) {
    }

    private final TransactionAddEdits edits;
    private final TransactionIds ids;
    private final TransactionRepository transactions;

    public TransactionAddService(TransactionAddEdits edits, TransactionIds ids, TransactionRepository transactions) {
        this.edits = edits;
        this.ids = ids;
        this.transactions = transactions;
    }

    public static String added(String tranId) {
        return String.format(MSG_ADDED, tranId);
    }

    /**
     * ENTER ({@code copyLast=false}) or PF5 ({@code copyLast=true}, R-32: the keys are edited first, then the
     * last transaction's data replaces the typed data and ENTER processing runs on it).
     */
    @Transactional
    public Outcome add(TransactionForm typed, String confirm, boolean copyLast) {
        TransactionForm form = edits.validateInputKeyFields(typed);
        if (copyLast) {
            form = copyLastTranData(form);
        }
        form = edits.validateInputDataFields(form);
        switch (confirmation(confirm)) {
            case 'Y' -> {
                Transaction written = addTransaction(form);
                log.info("COTRN02C WRITE transaction {} account {}", written.getTranId(), form.accountId());
                return new Outcome(State.ADDED, form, written, added(written.getTranId()));
            }
            case 'N' -> {
                return new Outcome(State.VALIDATED, form, null, MSG_CONFIRM);
            }
            default -> throw new InvalidRequestException(CONFIRM_FIELD, MSG_INVALID_CONFIRM);
        }
    }

    /** R-27: {@code Y}/{@code y} → add; {@code N}/{@code n}/blank → ask; anything else → invalid. */
    static char confirmation(String confirm) {
        if (ScreenInput.isSpacesOrLowValues(confirm)) {
            return 'N';
        }
        return switch (confirm.strip()) {
            case "Y", "y" -> 'Y';
            case "N", "n" -> 'N';
            default -> '?';
        };
    }

    /** R-28..R-31: next id under the id lock, record built from the edited fields, WRITE. */
    private Transaction addTransaction(TransactionForm form) {
        String tranId = ids.next();
        BigDecimal amount = NumvalC.parse(form.amount()).orElseThrow();
        TransactionRecord record = new TransactionRecord(tranId, TransactionAddEdits.typeCode(form.typeCode()),
                TransactionAddEdits.categoryCode(form.categoryCode()), ScreenInput.rightTrim(form.source()),
                ScreenInput.rightTrim(form.description()), amount,
                Integer.parseInt(ScreenInput.rightTrim(form.merchantId())), ScreenInput.rightTrim(form.merchantName()),
                ScreenInput.rightTrim(form.merchantCity()), ScreenInput.rightTrim(form.merchantZip()),
                form.cardNumber(), ScreenInput.rightTrim(form.origDate()), ScreenInput.rightTrim(form.procDate()));
        return ids.write(record, MSG_ADD_FAILED);
    }

    /** R-32: the last record (HIGH-VALUES READPREV) copied into the data fields; an empty file is NOTFND. */
    private TransactionForm copyLastTranData(TransactionForm form) {
        Optional<Transaction> found;
        try {
            found = transactions.findFirstByOrderByTranIdDesc();
        } catch (DataAccessException e) {
            throw AbendException.carddemo(TransactionLookup.MSG_LOOKUP_FAILED, e);
        }
        Transaction last = found.orElseThrow(() -> new RecordNotFoundException(TransactionLookup.MSG_NOT_FOUND));
        return new TransactionForm(form.accountId(), form.cardNumber(), last.getTranTypeCd(),
                String.format("%04d", last.getTranCatCd()), cut(last.getSource(), SOURCE_LENGTH),
                cut(last.getDescription(), DESCRIPTION_LENGTH), TransactionFormat.amount(last.getAmount()),
                TransactionFormat.date(last.getOrigTs()), TransactionFormat.date(last.getProcTs()),
                String.format("%09d", last.getMerchantId()), cut(last.getMerchantName(), MERCHANT_NAME_LENGTH),
                cut(last.getMerchantCity(), MERCHANT_CITY_LENGTH), cut(last.getMerchantZip(), MERCHANT_ZIP_LENGTH));
    }

    private static String cut(String value, int length) {
        String text = value == null ? "" : value;
        return text.length() <= length ? text : text.substring(0, length);
    }
}
