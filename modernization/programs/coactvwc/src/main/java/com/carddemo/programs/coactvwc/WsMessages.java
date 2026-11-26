package com.carddemo.programs.coactvwc;

/** 88-level message values of WS-INFO-MSG (X(40)) and WS-RETURN-MSG (X(75)) in COACTVWC working storage. */
public final class WsMessages {
    // WS-INFO-MSG
    public static final String WS_PROMPT_FOR_INPUT = "Enter or update id of account to display";
    public static final String WS_INFORM_OUTPUT = "Displaying details of given Account";

    // WS-RETURN-MSG
    public static final String WS_EXIT_MESSAGE = "PF03 pressed.Exiting              ";
    public static final String WS_PROMPT_FOR_ACCT = "Account number not provided";
    public static final String NO_SEARCH_CRITERIA_RECEIVED = "No input received";
    public static final String SEARCHED_ACCT_ZEROES = "Account number must be a non zero 11 digit number";
    public static final String SEARCHED_ACCT_NOT_NUMERIC = "Account number must be a non zero 11 digit number";
    public static final String DID_NOT_FIND_ACCT_IN_CARDXREF = "Did not find this account in account card xref file";
    public static final String DID_NOT_FIND_ACCT_IN_ACCTDAT = "Did not find this account in account master file";
    public static final String DID_NOT_FIND_CUST_IN_CUSTDAT = "Did not find associated customer in master file";
    public static final String XREF_READ_ERROR = "Error reading account card xref File";
    public static final String CODING_TO_BE_DONE = "Looks Good.... so far";

    /** Literal MOVEd in 2210-EDIT-ACCOUNT (note the double space, kept from the source). */
    public static final String ACCOUNT_FILTER_NOT_NUMERIC = "Account Filter must  be a non-zero 11 digit number";
    public static final String UNEXPECTED_DATA_SCENARIO = "UNEXPECTED DATA SCENARIO";
    public static final String UNEXPECTED_ABEND_OCCURRED = "UNEXPECTED ABEND OCCURRED.";

    private WsMessages() {
    }
}
