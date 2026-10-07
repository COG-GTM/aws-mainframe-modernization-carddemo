package com.carddemo.card.online;

import com.carddemo.common.FieldEditException;
import java.util.ArrayList;
import java.util.List;

/**
 * COCRDLIC {@code 2250-EDIT-ARRAY}: the selection codes typed in {@code CRDSEL1..7}. More than one {@code S}/{@code U}
 * is an error with every selected row flagged; any other non-blank code (lower case included) is
 * {@code INVALID ACTION CODE}; otherwise the last row holding {@code S}/{@code U} is selected (R-17).
 *
 * @param row    the selected row index (0-based), -1 when nothing is selected (R-13)
 * @param action {@code S} (detail, COCRDSLC, R-11) or {@code U} (update, COCRDUPC, R-12); null when none
 */
public record CardSelection(int row, String action) {

    public static final String MSG_ONLY_ONE = "PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE";
    public static final String MSG_INVALID_ACTION = "INVALID ACTION CODE";
    public static final String VIEW = "S";
    public static final String UPDATE = "U";

    public static CardSelection of(List<String> codes) {
        List<String> fields = new ArrayList<>();
        String message = null;
        long selected = codes.stream().filter(CardSelection::isSelect).count();
        if (selected > 1) {
            message = MSG_ONLY_ONE;
        }
        int row = -1;
        for (int i = 0; i < codes.size(); i++) {
            String code = codes.get(i);
            if (isSelect(code)) {
                row = i;
                if (selected > 1) {
                    fields.add(field(i));
                }
            } else if (!isBlank(code)) {
                fields.add(field(i));
                message = message == null ? MSG_INVALID_ACTION : message;
            }
        }
        if (message != null) {
            throw new FieldEditException(fields.get(0), message, fields);
        }
        return new CardSelection(row, row < 0 ? null : codes.get(row));
    }

    public boolean none() {
        return row < 0;
    }

    private static String field(int i) {
        return "rows[" + i + "].action";
    }

    private static boolean isSelect(String code) {
        return VIEW.equals(code) || UPDATE.equals(code);
    }

    /** {@code SELECT-BLANK}: spaces or low-values. */
    private static boolean isBlank(String code) {
        return code == null || code.isBlank() || code.chars().allMatch(c -> c == 0);
    }
}
