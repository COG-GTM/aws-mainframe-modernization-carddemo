package com.carddemo.online.web;

import com.carddemo.runtime.bms.BmsScreen;
import java.util.List;
import java.util.Map;

/**
 * Result of one task.
 *
 * @param outcome         RETURN, RETURN_TRANSID, XCTL or ABEND
 * @param nextTransid     RETURN TRANSID target (the next interaction starts this transaction)
 * @param nextProgram     XCTL target (a not-yet-migrated program is returned here, not executed)
 * @param commarea        commarea passed on, base64
 * @param commareaFields  commarea decoded with the program's copybook layout
 * @param screen          last SEND MAP (null when none)
 * @param screenLines     last SEND MAP rendered as 24 lines (null when none)
 * @param text            last SEND TEXT (null when none)
 */
public record CicsTransactionResponse(String program, String outcome, String nextTransid, String nextProgram,
                                      String commarea, Map<String, Object> commareaFields, BmsScreen screen,
                                      List<String> screenLines, String text, int eibresp, int eibresp2,
                                      String abendCode) {
}
