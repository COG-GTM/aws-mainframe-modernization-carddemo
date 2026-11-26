package com.carddemo.programs.coactupc;

/** 88-level values of WS-INFO-MSG PIC X(40) and WS-RETURN-MSG PIC X(75). */
public final class Messages {

    private Messages() {
    }

    // WS-INFO-MSG
    public static final String FOUND_ACCOUNT_DATA = "Details of selected account shown above";
    public static final String PROMPT_FOR_SEARCH_KEYS = "Enter or update id of account to update";
    public static final String PROMPT_FOR_CHANGES = "Update account details presented above.";
    public static final String PROMPT_FOR_CONFIRMATION = "Changes validated.Press F5 to save";
    public static final String CONFIRM_UPDATE_SUCCESS = "Changes committed to database";
    public static final String INFORM_FAILURE = "Changes unsuccessful. Please try again";

    // WS-RETURN-MSG
    public static final String WS_EXIT_MESSAGE = "PF03 pressed.Exiting              ";
    public static final String WS_PROMPT_FOR_ACCT = "Account number not provided";
    public static final String WS_PROMPT_FOR_LASTNAME = "Last name not provided";
    public static final String WS_NAME_MUST_BE_ALPHA = "Name can only contain alphabets and spaces";
    public static final String NO_SEARCH_CRITERIA_RECEIVED = "No input received";
    public static final String NO_CHANGES_DETECTED = "No change detected with respect to values fetched.";
    public static final String SEARCHED_ACCT_ZEROES = "Account number must be a non zero 11 digit number";
    public static final String SEARCHED_ACCT_NOT_NUMERIC = "Account number must be a non zero 11 digit number";
    /** First of the two DID-NOT-FIND-ACCT-IN-CARDXREF 88s (the first VALUE wins on SET). */
    public static final String DID_NOT_FIND_ACCT_IN_CARDXREF = "Did not find this account in account card xref file";
    public static final String DID_NOT_FIND_ACCT_IN_ACCTDAT = "Did not find this account in account master file";
    public static final String DID_NOT_FIND_CUST_IN_CUSTDAT = "Did not find associated customer in master file";
    public static final String ACCT_STATUS_MUST_BE_YES_NO = "Account Active Status must be Y or N";
    public static final String CRED_LIMIT_IS_BLANK = "Credit Limit must be supplied";
    public static final String CRED_LIMIT_IS_NOT_VALID = "Credit Limit is not valid";
    public static final String THIS_MONTH_NOT_VALID = "Card expiry month must be between 1 and 12";
    public static final String THIS_YEAR_NOT_VALID = "Invalid card expiry year";
    public static final String DID_NOT_FIND_ACCTCARD_COMBO = "Did not find cards for this search condition";
    public static final String COULD_NOT_LOCK_ACCT_FOR_UPDATE = "Could not lock account record for update";
    public static final String COULD_NOT_LOCK_CUST_FOR_UPDATE = "Could not lock customer record for update";
    public static final String DATA_WAS_CHANGED_BEFORE_UPDATE = "Record changed by some one else. Please review";
    public static final String LOCKED_BUT_UPDATE_FAILED = "Update of record failed";
    public static final String XREF_READ_ERROR = "Error reading Card Data File";
    public static final String CODING_TO_BE_DONE = "Looks Good.... so far";

    // Messages built with STRING in the procedure division
    public static final String ACCOUNT_NUMBER_INVALID = "Account Number if supplied must be a 11 digit Non-Zero Number";
}
