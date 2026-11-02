package com.carddemo.transaction.service;

import com.carddemo.common.api.ApiError.FieldProblem;
import com.carddemo.common.error.FieldValidationException;
import com.carddemo.transaction.api.dto.TransactionCreateRequest;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Pattern;
import org.springframework.stereotype.Component;

/**
 * COTRN02C {@code VALIDATE-INPUT-KEY-FIELDS} and {@code VALIDATE-INPUT-DATA-FIELDS}, in the order the
 * COBOL runs them and with its messages. The COBOL stops at the first failure; this reports every
 * failing field, first the one the COBOL would have shown, at most one problem per field.
 */
@Component
public class TransactionAddValidator {

    private static final Pattern ACCOUNT_ID = Pattern.compile("\\d{1,11}");
    private static final Pattern CARD_NUMBER = Pattern.compile("\\d{1,16}");
    private static final Pattern TYPE_CODE = Pattern.compile("\\d{1,2}");
    private static final Pattern CATEGORY_CODE = Pattern.compile("\\d{1,4}");
    private static final Pattern MERCHANT_ID = Pattern.compile("\\d{1,9}");
    /** {@code TRNAMTI} edit: sign, 8 integer digits, point, 2 decimals ({@code -99999999.99}); sign and leading zeros optional. */
    private static final Pattern AMOUNT = Pattern.compile("[+-]?\\d{1,8}(\\.\\d{1,2})?");
    /** {@code TORIGDTI} / {@code TPROCDTI} edit: {@code (1:4)} numeric, {@code (5:1) = '-'}, {@code (6:2)} numeric, ... */
    private static final Pattern DATE = Pattern.compile("\\d{4}-\\d{2}-\\d{2}");

    public void validate(TransactionCreateRequest request) {
        Problems problems = new Problems();

        // VALIDATE-INPUT-KEY-FIELDS
        if (!blank(request.accountId())) {
            problems.unless("accountId", ACCOUNT_ID.matcher(request.accountId().trim()).matches(),
                    "Account ID must be Numeric...");
        } else if (!blank(request.cardNumber())) {
            problems.unless("cardNumber", CARD_NUMBER.matcher(request.cardNumber().trim()).matches(),
                    "Card Number must be Numeric...");
        } else {
            problems.add("accountId", "Account or Card Number must be entered...");
        }

        // VALIDATE-INPUT-DATA-FIELDS: empty fields, in map order
        problems.notBlank("typeCode", request.typeCode(), "Type CD can NOT be empty...");
        problems.notBlank("categoryCode", request.categoryCode(), "Category CD can NOT be empty...");
        problems.notBlank("source", request.source(), "Source can NOT be empty...");
        problems.notBlank("description", request.description(), "Description can NOT be empty...");
        problems.notBlank("amount", request.amount(), "Amount can NOT be empty...");
        problems.notBlank("originDate", request.originDate(), "Orig Date can NOT be empty...");
        problems.notBlank("processDate", request.processDate(), "Proc Date can NOT be empty...");
        problems.notBlank("merchantId", request.merchantId(), "Merchant ID can NOT be empty...");
        problems.notBlank("merchantName", request.merchantName(), "Merchant Name can NOT be empty...");
        problems.notBlank("merchantCity", request.merchantCity(), "Merchant City can NOT be empty...");
        problems.notBlank("merchantZip", request.merchantZip(), "Merchant Zip can NOT be empty...");

        problems.matches("typeCode", request.typeCode(), TYPE_CODE, "Type CD must be Numeric...");
        problems.matches("categoryCode", request.categoryCode(), CATEGORY_CODE, "Category CD must be Numeric...");
        problems.matches("amount", request.amount(), AMOUNT, "Amount should be in format -99999999.99");
        problems.matches("originDate", request.originDate(), DATE, "Orig Date should be in format YYYY-MM-DD");
        problems.matches("processDate", request.processDate(), DATE, "Proc Date should be in format YYYY-MM-DD");
        problems.check("originDate", request.originDate(), CsutldtcDate::isValid, "Orig Date - Not a valid date...");
        problems.check("processDate", request.processDate(), CsutldtcDate::isValid, "Proc Date - Not a valid date...");
        problems.matches("merchantId", request.merchantId(), MERCHANT_ID, "Merchant ID must be Numeric...");

        // Column widths of CVTRA05Y; the 3270 map cut longer input off at the field length.
        problems.maxLength("source", request.source(), 10);
        problems.maxLength("description", request.description(), 100);
        problems.maxLength("merchantName", request.merchantName(), 50);
        problems.maxLength("merchantCity", request.merchantCity(), 50);
        problems.maxLength("merchantZip", request.merchantZip(), 10);

        if (!problems.list.isEmpty()) {
            throw new FieldValidationException(problems.list);
        }
    }

    /** {@code COMPUTE WS-ACCT-ID-N = FUNCTION NUMVAL(ACTIDINI)}, a PIC 9(11). */
    static long accountId(TransactionCreateRequest request) {
        return Long.parseLong(request.accountId().trim());
    }

    /** {@code COMPUTE WS-CARD-NUM-N = FUNCTION NUMVAL(CARDNINI)} into PIC 9(16): zero padded. */
    static String cardNumber(TransactionCreateRequest request) {
        return String.format("%016d", Long.parseLong(request.cardNumber().trim()));
    }

    /** TRAN-TYPE-CD is X(02); the map field is two characters, type codes are {@code 01}..{@code 07}. */
    static String typeCode(TransactionCreateRequest request) {
        return String.format("%02d", Integer.parseInt(request.typeCode().trim()));
    }

    static int categoryCode(TransactionCreateRequest request) {
        return Integer.parseInt(request.categoryCode().trim());
    }

    /** {@code FUNCTION NUMVAL-C(TRNAMTI)} moved to TRAN-AMT S9(09)V99. */
    static BigDecimal amount(TransactionCreateRequest request) {
        return new BigDecimal(request.amount().trim()).setScale(2);
    }

    static LocalDate date(String value) {
        return LocalDate.parse(value.trim());
    }

    static long merchantId(TransactionCreateRequest request) {
        return Long.parseLong(request.merchantId().trim());
    }

    private static boolean blank(String value) {
        return value == null || value.isBlank();
    }

    private static final class Problems {

        private final List<FieldProblem> list = new ArrayList<>();
        private final Set<String> failed = new HashSet<>();

        void add(String field, String message) {
            if (failed.add(field)) {
                list.add(new FieldProblem(field, message));
            }
        }

        void unless(String field, boolean ok, String message) {
            if (!ok) {
                add(field, message);
            }
        }

        void notBlank(String field, String value, String message) {
            unless(field, !blank(value), message);
        }

        void matches(String field, String value, Pattern pattern, String message) {
            check(field, value, v -> pattern.matcher(v).matches(), message);
        }

        void check(String field, String value, Function<String, Boolean> rule, String message) {
            if (!failed.contains(field) && !blank(value) && !rule.apply(value.trim())) {
                add(field, message);
            }
        }

        void maxLength(String field, String value, int max) {
            check(field, value, v -> v.length() <= max, "must be at most " + max + " characters");
        }
    }
}
