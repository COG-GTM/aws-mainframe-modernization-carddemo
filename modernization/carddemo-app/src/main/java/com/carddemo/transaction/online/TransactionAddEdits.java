package com.carddemo.transaction.online;

import com.carddemo.card.CardXref;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.AbendException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.codec.NumvalC;
import com.carddemo.common.date.Csutldtc;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.transaction.TransactionCategoryId;
import com.carddemo.transaction.TransactionCategoryRepository;
import com.carddemo.transaction.TransactionTypeRepository;
import java.math.BigDecimal;
import java.util.function.Supplier;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Component;

/**
 * COTRN02C edits, one method per paragraph in source order: {@code VALIDATE-INPUT-KEY-FIELDS} (R-8..R-14) and
 * {@code VALIDATE-INPUT-DATA-FIELDS} (R-15..R-26). The first failing check ends the request, as each COBOL check
 * sends the screen and stops.
 */
@Component
public class TransactionAddEdits {

    public static final String ACCOUNT_FIELD = "accountId";
    public static final String CARD_FIELD = "cardNumber";
    public static final String TYPE_FIELD = "typeCode";
    public static final String CATEGORY_FIELD = "categoryCode";
    public static final String SOURCE_FIELD = "source";
    public static final String DESCRIPTION_FIELD = "description";
    public static final String AMOUNT_FIELD = "amount";
    public static final String ORIG_DATE_FIELD = "origDate";
    public static final String PROC_DATE_FIELD = "procDate";
    public static final String MERCHANT_ID_FIELD = "merchantId";
    public static final String MERCHANT_NAME_FIELD = "merchantName";
    public static final String MERCHANT_CITY_FIELD = "merchantCity";
    public static final String MERCHANT_ZIP_FIELD = "merchantZip";

    public static final String MSG_ACCOUNT_NOT_NUMERIC = "Account ID must be Numeric...";
    public static final String MSG_CARD_NOT_NUMERIC = "Card Number must be Numeric...";
    public static final String MSG_KEY_REQUIRED = "Account or Card Number must be entered...";
    public static final String MSG_ACCOUNT_NOT_FOUND = "Account ID NOT found...";
    public static final String MSG_ACCOUNT_LOOKUP_FAILED = "Unable to lookup Acct in XREF AIX file...";
    public static final String MSG_CARD_NOT_FOUND = "Card Number NOT found...";
    public static final String MSG_CARD_LOOKUP_FAILED = "Unable to lookup Card # in XREF file...";
    public static final String MSG_TYPE_EMPTY = "Type CD can NOT be empty...";
    public static final String MSG_CATEGORY_EMPTY = "Category CD can NOT be empty...";
    public static final String MSG_SOURCE_EMPTY = "Source can NOT be empty...";
    public static final String MSG_DESCRIPTION_EMPTY = "Description can NOT be empty...";
    public static final String MSG_AMOUNT_EMPTY = "Amount can NOT be empty...";
    public static final String MSG_ORIG_DATE_EMPTY = "Orig Date can NOT be empty...";
    public static final String MSG_PROC_DATE_EMPTY = "Proc Date can NOT be empty...";
    public static final String MSG_MERCHANT_ID_EMPTY = "Merchant ID can NOT be empty...";
    public static final String MSG_MERCHANT_NAME_EMPTY = "Merchant Name can NOT be empty...";
    public static final String MSG_MERCHANT_CITY_EMPTY = "Merchant City can NOT be empty...";
    public static final String MSG_MERCHANT_ZIP_EMPTY = "Merchant Zip can NOT be empty...";
    public static final String MSG_TYPE_NOT_NUMERIC = "Type CD must be Numeric...";
    public static final String MSG_CATEGORY_NOT_NUMERIC = "Category CD must be Numeric...";
    public static final String MSG_AMOUNT_FORMAT = "Amount should be in format -99999999.99";
    public static final String MSG_ORIG_DATE_FORMAT = "Orig Date should be in format YYYY-MM-DD";
    public static final String MSG_PROC_DATE_FORMAT = "Proc Date should be in format YYYY-MM-DD";
    public static final String MSG_ORIG_DATE_INVALID = "Orig Date - Not a valid date...";
    public static final String MSG_PROC_DATE_INVALID = "Proc Date - Not a valid date...";
    public static final String MSG_MERCHANT_ID_NOT_NUMERIC = "Merchant ID must be Numeric...";
    /** Added check (s5.4): CBTRN03C abends on a type or category it cannot find, so the port refuses them here. */
    public static final String MSG_TYPE_UNKNOWN = "Type CD not found in TRANTYPE...";
    public static final String MSG_CATEGORY_UNKNOWN = "Category CD not found in TRANCATG for this Type CD...";
    public static final String MSG_REFERENCE_LOOKUP_FAILED = "Unable to lookup TRANTYPE/TRANCATG...";

    static final String DATE_MASK = "YYYY-MM-DD";
    /** CEEDAYS "insufficient data" feedback, which COTRN02C accepts (R-26). */
    static final String CEEDAYS_INSUFFICIENT_DATA = "2513";

    private final CardXrefRepository xrefs;
    private final TransactionTypeRepository types;
    private final TransactionCategoryRepository categories;

    public TransactionAddEdits(CardXrefRepository xrefs, TransactionTypeRepository types,
            TransactionCategoryRepository categories) {
        this.xrefs = xrefs;
        this.types = types;
        this.categories = categories;
    }

    /**
     * {@code VALIDATE-INPUT-KEY-FIELDS}: the account (when entered) wins over the card; the other key is filled from
     * CARDXREF. Returns the form with {@code accountId} as 11 digits and {@code cardNumber} as 16.
     */
    public TransactionForm validateInputKeyFields(TransactionForm form) {
        if (!ScreenInput.isSpacesOrLowValues(form.accountId())) {
            String typed = ScreenInput.rightTrim(form.accountId());
            if (!isNumeric(typed, 11)) {
                throw new InvalidRequestException(ACCOUNT_FIELD, MSG_ACCOUNT_NOT_NUMERIC);
            }
            long acctId = Long.parseLong(typed);
            CardXref xref = read(() -> xrefs.findFirstByAcctIdOrderByCardNumAsc(acctId), MSG_ACCOUNT_LOOKUP_FAILED)
                    .orElseThrow(() -> new RecordNotFoundException(MSG_ACCOUNT_NOT_FOUND));
            return form.withKeys(String.format("%011d", acctId), xref.getCardNum());
        }
        if (!ScreenInput.isSpacesOrLowValues(form.cardNumber())) {
            String typed = ScreenInput.rightTrim(form.cardNumber());
            if (!isNumeric(typed, 16)) {
                throw new InvalidRequestException(CARD_FIELD, MSG_CARD_NOT_NUMERIC);
            }
            String cardNum = String.format("%016d", Long.parseLong(typed));
            CardXref xref = read(() -> xrefs.findById(cardNum), MSG_CARD_LOOKUP_FAILED)
                    .orElseThrow(() -> new RecordNotFoundException(MSG_CARD_NOT_FOUND));
            return form.withKeys(String.format("%011d", xref.getAcctId()), cardNum);
        }
        throw new InvalidRequestException(ACCOUNT_FIELD, MSG_KEY_REQUIRED);
    }

    /**
     * {@code VALIDATE-INPUT-DATA-FIELDS}: presence in screen order (R-15..R-22), then the format checks (R-23..R-26),
     * then the TRANTYPE/TRANCATG references (added). Returns the form with the amount re-edited.
     */
    public TransactionForm validateInputDataFields(TransactionForm form) {
        required(form.typeCode(), TYPE_FIELD, MSG_TYPE_EMPTY);
        required(form.categoryCode(), CATEGORY_FIELD, MSG_CATEGORY_EMPTY);
        required(form.source(), SOURCE_FIELD, MSG_SOURCE_EMPTY);
        required(form.description(), DESCRIPTION_FIELD, MSG_DESCRIPTION_EMPTY);
        required(form.amount(), AMOUNT_FIELD, MSG_AMOUNT_EMPTY);
        required(form.origDate(), ORIG_DATE_FIELD, MSG_ORIG_DATE_EMPTY);
        required(form.procDate(), PROC_DATE_FIELD, MSG_PROC_DATE_EMPTY);
        required(form.merchantId(), MERCHANT_ID_FIELD, MSG_MERCHANT_ID_EMPTY);
        required(form.merchantName(), MERCHANT_NAME_FIELD, MSG_MERCHANT_NAME_EMPTY);
        required(form.merchantCity(), MERCHANT_CITY_FIELD, MSG_MERCHANT_CITY_EMPTY);
        required(form.merchantZip(), MERCHANT_ZIP_FIELD, MSG_MERCHANT_ZIP_EMPTY);

        if (!isNumeric(ScreenInput.rightTrim(form.typeCode()), 2)) {
            throw new InvalidRequestException(TYPE_FIELD, MSG_TYPE_NOT_NUMERIC);
        }
        if (!isNumeric(ScreenInput.rightTrim(form.categoryCode()), 4)) {
            throw new InvalidRequestException(CATEGORY_FIELD, MSG_CATEGORY_NOT_NUMERIC);
        }
        if (!amountShape(form.amount())) {
            throw new InvalidRequestException(AMOUNT_FIELD, MSG_AMOUNT_FORMAT);
        }
        if (!dateShape(form.origDate())) {
            throw new InvalidRequestException(ORIG_DATE_FIELD, MSG_ORIG_DATE_FORMAT);
        }
        if (!dateShape(form.procDate())) {
            throw new InvalidRequestException(PROC_DATE_FIELD, MSG_PROC_DATE_FORMAT);
        }
        BigDecimal amount = NumvalC.parse(form.amount()).orElseThrow(
                () -> new InvalidRequestException(AMOUNT_FIELD, MSG_AMOUNT_FORMAT));
        if (!validDate(form.origDate())) {
            throw new InvalidRequestException(ORIG_DATE_FIELD, MSG_ORIG_DATE_INVALID);
        }
        if (!validDate(form.procDate())) {
            throw new InvalidRequestException(PROC_DATE_FIELD, MSG_PROC_DATE_INVALID);
        }
        if (!isNumeric(ScreenInput.rightTrim(form.merchantId()), 9)) {
            throw new InvalidRequestException(MERCHANT_ID_FIELD, MSG_MERCHANT_ID_NOT_NUMERIC);
        }
        String type = typeCode(form.typeCode());
        int category = categoryCode(form.categoryCode());
        if (!read(() -> types.existsById(type), MSG_REFERENCE_LOOKUP_FAILED)) {
            throw new InvalidRequestException(TYPE_FIELD, MSG_TYPE_UNKNOWN);
        }
        if (!read(() -> categories.existsById(new TransactionCategoryId(type, category)),
                MSG_REFERENCE_LOOKUP_FAILED)) {
            throw new InvalidRequestException(CATEGORY_FIELD, MSG_CATEGORY_UNKNOWN);
        }
        return form.withAmount(TransactionFormat.amount(amount));
    }

    /** {@code TRAN-TYPE-CD ← TTYPCDI}, a one-digit entry zero-padded to the two digits TRANTYPE keys have. */
    static String typeCode(String typed) {
        return String.format("%02d", Integer.parseInt(ScreenInput.rightTrim(typed)));
    }

    /** {@code TRAN-CAT-CD ← TCATCDI} (PIC 9(4)). */
    static int categoryCode(String typed) {
        return Integer.parseInt(ScreenInput.rightTrim(typed));
    }

    /** R-24: exactly {@code [+-]99999999.99}. */
    static boolean amountShape(String typed) {
        String a = TransactionFormat.pad(typed, 12);
        if (typed.length() > 12 || (a.charAt(0) != '-' && a.charAt(0) != '+') || a.charAt(9) != '.') {
            return false;
        }
        return digits(a, 1, 9) && digits(a, 10, 12);
    }

    /** R-25: {@code 9999-99-99}. */
    static boolean dateShape(String typed) {
        String d = TransactionFormat.pad(typed, 10);
        return typed.length() <= 10 && digits(d, 0, 4) && d.charAt(4) == '-' && digits(d, 5, 7) && d.charAt(7) == '-'
                && digits(d, 8, 10);
    }

    /** R-26: {@code CSUTLDTC} severity 0000, or message 2513 (insufficient data) which COTRN02C lets through. */
    static boolean validDate(String date) {
        Csutldtc.Result result = Csutldtc.validate(date, DATE_MASK);
        return "0000".equals(result.severityCode()) || CEEDAYS_INSUFFICIENT_DATA.equals(result.messageCode());
    }

    private static void required(String value, String field, String message) {
        if (ScreenInput.isSpacesOrLowValues(value)) {
            throw new InvalidRequestException(field, message);
        }
    }

    /** {@code IS NUMERIC} of the entered characters (trailing spaces are the unentered part of the BMS field). */
    static boolean isNumeric(String typed, int maxLength) {
        return !typed.isEmpty() && typed.length() <= maxLength && digits(typed, 0, typed.length());
    }

    private static boolean digits(String text, int from, int to) {
        for (int i = from; i < to; i++) {
            char c = text.charAt(i);
            if (c < '0' || c > '9') {
                return false;
            }
        }
        return true;
    }

    private static <T> T read(Supplier<T> read, String failure) {
        try {
            return read.get();
        } catch (DataAccessException e) {
            throw AbendException.carddemo(failure, e);
        }
    }
}
