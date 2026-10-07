package com.carddemo.card.online;

import com.carddemo.common.FieldEditException;
import java.util.ArrayList;
import java.util.List;

/**
 * The account/card key edits of the three card programs: {@code COCRDLIC 2210/2220} (optional list filters) and
 * {@code COCRDSLC 2210/2220} = {@code COCRDUPC 1210/1220} (required search keys).
 *
 * @param acctId  {@code CDEMO-ACCT-ID}, {@code null} when blank (no filter)
 * @param cardNum {@code CDEMO-CARD-NUM} as 16 digits, {@code null} when blank (no filter)
 */
public record CardKeys(Long acctId, String cardNum) {

    public static final String ACCOUNT_FIELD = "accountId";
    public static final String CARD_FIELD = "cardNumber";

    public static final String MSG_ACCOUNT_INVALID = "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER";
    public static final String MSG_CARD_INVALID = "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER";
    public static final String MSG_ACCOUNT_NOT_PROVIDED = "Account number not provided";
    public static final String MSG_CARD_NOT_PROVIDED = "Card number not provided";
    public static final String MSG_NO_INPUT = "No input received";

    private enum Edit { BLANK, INVALID, VALID }

    private record Parsed(Edit edit, long value) {
    }

    /**
     * COCRDLIC {@code 2210-EDIT-ACCOUNT}/{@code 2220-EDIT-CARD}: low-values, spaces or zero = no filter; anything
     * else must be numeric ({@code CC-ACCT-ID PIC X(11)}, {@code CC-CARD-NUM PIC X(16)}). The account message wins
     * when both fail (R-15, R-16).
     */
    public static CardKeys listFilters(String account, String card) {
        Parsed a = parse(account, 11, false);
        Parsed c = parse(card, 16, false);
        List<String> fields = new ArrayList<>();
        String message = null;
        if (a.edit() == Edit.INVALID) {
            fields.add(ACCOUNT_FIELD);
            message = MSG_ACCOUNT_INVALID;
        }
        if (c.edit() == Edit.INVALID) {
            fields.add(CARD_FIELD);
            message = message == null ? MSG_CARD_INVALID : message;
        }
        if (!fields.isEmpty()) {
            throw new FieldEditException(fields.get(0), message, fields);
        }
        return new CardKeys(a.edit() == Edit.VALID ? a.value() : null,
                c.edit() == Edit.VALID ? cardNum(c.value()) : null);
    }

    /**
     * COCRDSLC {@code 2200-EDIT-MAP-INPUTS} / COCRDUPC {@code 1200-EDIT-MAP-INPUTS} search phase: {@code *} or
     * spaces = low-values (R-10); both keys are required; the first message wins, except that two blank keys give
     * {@code No input received} (COCRDSLC R-11..R-15, COCRDUPC R-10..R-12). The key edits are COCRDSLC
     * {@code 2210-EDIT-ACCOUNT}, COCRDSLC {@code 2220-EDIT-CARD}, COCRDUPC {@code 1210-EDIT-ACCOUNT}, COCRDUPC
     * {@code 1220-EDIT-CARD}.
     */
    public static CardKeys searchKeys(String account, String card) {
        Parsed a = parse(account, 11, true);
        Parsed c = parse(card, 16, true);
        List<String> fields = new ArrayList<>();
        String message = null;
        if (a.edit() != Edit.VALID) {
            fields.add(ACCOUNT_FIELD);
            message = a.edit() == Edit.BLANK ? MSG_ACCOUNT_NOT_PROVIDED : MSG_ACCOUNT_INVALID;
        }
        if (c.edit() != Edit.VALID) {
            fields.add(CARD_FIELD);
            String cardMessage = c.edit() == Edit.BLANK ? MSG_CARD_NOT_PROVIDED : MSG_CARD_INVALID;
            message = message == null ? cardMessage : message;
        }
        if (a.edit() == Edit.BLANK && c.edit() == Edit.BLANK) {
            message = MSG_NO_INPUT;
        }
        if (!fields.isEmpty()) {
            throw new FieldEditException(fields.get(0), message, fields);
        }
        return new CardKeys(a.value(), cardNum(c.value()));
    }

    /**
     * COCRDSLC {@code 2210-EDIT-ACCOUNT} alone, for the CARDAIX path ({@code 9150-GETCARD-BYACCT}): the account is
     * required (R-11, R-12).
     */
    public static long accountKey(String account) {
        Parsed a = parse(account, 11, true);
        if (a.edit() != Edit.VALID) {
            throw new FieldEditException(ACCOUNT_FIELD,
                    a.edit() == Edit.BLANK ? MSG_ACCOUNT_NOT_PROVIDED : MSG_ACCOUNT_INVALID, List.of(ACCOUNT_FIELD));
        }
        return a.value();
    }

    /** {@code CDEMO-CARD-NUM PIC 9(16)}: the card number right-justified with leading zeros. */
    public static String cardNum(long value) {
        return String.format("%016d", value);
    }

    private static Parsed parse(String input, int width, boolean asteriskIsBlank) {
        String v = input == null ? "" : input.strip();
        if (v.isEmpty() || (asteriskIsBlank && v.equals("*"))) {
            return new Parsed(Edit.BLANK, 0);
        }
        if (v.length() > width || !v.chars().allMatch(ch -> ch >= '0' && ch <= '9')) {
            return new Parsed(Edit.INVALID, 0);
        }
        long value = Long.parseLong(v);
        return value == 0 ? new Parsed(Edit.BLANK, 0) : new Parsed(Edit.VALID, value);
    }
}
