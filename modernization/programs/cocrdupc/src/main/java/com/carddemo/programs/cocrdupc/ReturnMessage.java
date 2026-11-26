package com.carddemo.programs.cocrdupc;

/** WS-RETURN-MSG PIC X(75) 88-levels plus the inline literals moved to it (exact COBOL text). */
public enum ReturnMessage {
    WS_EXIT_MESSAGE("PF03 pressed.Exiting              "),
    WS_PROMPT_FOR_ACCT("Account number not provided"),
    WS_PROMPT_FOR_CARD("Card number not provided"),
    WS_PROMPT_FOR_NAME("Card name not provided"),
    WS_NAME_MUST_BE_ALPHA("Card name can only contain alphabets and spaces"),
    NO_SEARCH_CRITERIA_RECEIVED("No input received"),
    NO_CHANGES_DETECTED("No change detected with respect to values fetched."),
    SEARCHED_ACCT_ZEROES("Account number must be a non zero 11 digit number"),
    SEARCHED_ACCT_NOT_NUMERIC("Account number must be a non zero 11 digit number"),
    SEARCHED_CARD_NOT_NUMERIC("Card number if supplied must be a 16 digit number"),
    CARD_STATUS_MUST_BE_YES_NO("Card Active Status must be Y or N"),
    CARD_EXPIRY_MONTH_NOT_VALID("Card expiry month must be between 1 and 12"),
    CARD_EXPIRY_YEAR_NOT_VALID("Invalid card expiry year"),
    DID_NOT_FIND_ACCT_IN_CARDXREF("Did not find this account in cards database"),
    DID_NOT_FIND_ACCTCARD_COMBO("Did not find cards for this search condition"),
    COULD_NOT_LOCK_FOR_UPDATE("Could not lock record for update"),
    DATA_WAS_CHANGED_BEFORE_UPDATE("Record changed by some one else. Please review"),
    LOCKED_BUT_UPDATE_FAILED("Update of record failed"),
    XREF_READ_ERROR("Error reading Card Data File"),
    CODING_TO_BE_DONE("Looks Good.... so far"),
    /** Inline literal in 1210-EDIT-ACCOUNT. */
    ACCOUNT_FILTER_NOT_NUMERIC("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"),
    /** Inline literal in 1220-EDIT-CARD. */
    CARD_FILTER_NOT_NUMERIC("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER");

    public static final int LENGTH = 75;

    private final String text;

    ReturnMessage(String text) {
        this.text = text;
    }

    public String text() {
        return text;
    }

    public String value() {
        return Cobol.alnum(text, LENGTH);
    }
}
