package com.carddemo.programs.cocrdupc;

/** WS-INFO-MSG PIC X(40) 88-levels (exact COBOL literals). */
public enum InfoMessage {
    FOUND_CARDS_FOR_ACCOUNT("Details of selected card shown above"),
    PROMPT_FOR_SEARCH_KEYS("Please enter Account and Card Number"),
    PROMPT_FOR_CHANGES("Update card details presented above."),
    PROMPT_FOR_CONFIRMATION("Changes validated.Press F5 to save"),
    CONFIRM_UPDATE_SUCCESS("Changes committed to database"),
    INFORM_FAILURE("Changes unsuccessful. Please try again");

    public static final int LENGTH = 40;

    private final String text;

    InfoMessage(String text) {
        this.text = text;
    }

    public String text() {
        return text;
    }

    /** Value as stored in the PIC X(40) field. */
    public String value() {
        return Cobol.alnum(text, LENGTH);
    }
}
