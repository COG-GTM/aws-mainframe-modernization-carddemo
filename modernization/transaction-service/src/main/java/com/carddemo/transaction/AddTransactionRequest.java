package com.carddemo.transaction;

/** CTRN2A map fields as entered (strings, so COBOL-style edits can be reproduced exactly). */
public record AddTransactionRequest(String acctId, String cardNum, String typeCd, String catCd, String source,
                                    String description, String amount, String origDate, String procDate,
                                    String merchantId, String merchantName, String merchantCity,
                                    String merchantZip, String confirm) {
}
