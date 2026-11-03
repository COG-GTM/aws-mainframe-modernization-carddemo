package com.carddemo.card.service;

/** Screen messages of COCRDLIC, COCRDSLC and COCRDUPC, verbatim from their WORKING-STORAGE. */
public final class CardMessages {

    public static final String ACCOUNT_FILTER_NOT_NUMERIC = "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER";
    public static final String CARD_FILTER_NOT_NUMERIC = "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER";
    public static final String CARD_NOT_PROVIDED = "Card number not provided";
    public static final String NOT_FOUND = "Did not find cards for this search condition";
    public static final String NAME_NOT_PROVIDED = "Card name not provided";
    public static final String NAME_MUST_BE_ALPHA = "Card name can only contain alphabets and spaces";
    public static final String STATUS_MUST_BE_YES_NO = "Card Active Status must be Y or N";
    public static final String EXPIRY_MONTH_NOT_VALID = "Card expiry month must be between 1 and 12";
    public static final String EXPIRY_YEAR_NOT_VALID = "Invalid card expiry year";
    public static final String NO_CHANGES_DETECTED = "No change detected with respect to values fetched.";

    private CardMessages() {
    }
}
