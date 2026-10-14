package com.carddemo.online.web;

import java.util.Map;

/**
 * One terminal interaction.
 *
 * @param aid      attention key (ENTER, PF1..PF24, CLEAR, PA1..PA3); default ENTER
 * @param commarea commarea from the previous RETURN TRANSID / XCTL, base64 (ISO-8859-1 bytes); absent or
 *                 empty means EIBCALEN = 0 (first entry)
 * @param fields   map field name -> typed text for fields the terminal transmits (modified/FSET);
 *                 absent or empty means no data (RECEIVE MAP gets MAPFAIL)
 * @param termid   terminal id (EIBTRMID), optional
 */
public record CicsTransactionRequest(String aid, String commarea, Map<String, String> fields, String termid) {
}
