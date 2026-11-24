package com.carddemo.common.web;

import com.carddemo.common.error.ApiException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

/** Shared PF7/PF8 browse logic over a string-keyed table. */
public final class KeysetBrowser {

    public static final String TOP_MESSAGE = "You are already at the top of the page...";
    public static final String BOTTOM_MESSAGE = "You are already at the bottom of the page...";

    private KeysetBrowser() {
    }

    /**
     * @param forward  fetches up to n rows with key >= start in ascending order
     * @param backward fetches up to n rows with key < start in descending order
     * @param after    if set, PF8: next page after this key (exclusive)
     * @param before   if set, PF7: previous page before this key (exclusive)
     * @param start    initial positioning key typed by the user (inclusive), may be null
     */
    public static <T> KeysetPage<T> browse(int pageSize, int pageNumber, String start, String after, String before,
                                           BiFunction<String, Integer, List<T>> forward,
                                           BiFunction<String, Integer, List<T>> backward,
                                           Function<T, String> keyOf) {
        if (before != null) {
            List<T> rows = new ArrayList<>(backward.apply(before, pageSize + 1));
            boolean more = rows.size() > pageSize;
            if (rows.isEmpty()) {
                throw ApiException.badRequest("TOP_OF_PAGE", TOP_MESSAGE);
            }
            rows = new ArrayList<>(rows.subList(0, Math.min(pageSize, rows.size())));
            Collections.reverse(rows);
            int page = Math.max(1, pageNumber - 1);
            return page(rows, page, true, more, null, keyOf);
        }
        String from = after != null ? after + "\u0000" : (start == null ? "" : start);
        List<T> rows = forward.apply(from, pageSize + 1);
        if (rows.isEmpty() && after != null) {
            throw ApiException.badRequest("BOTTOM_OF_PAGE", BOTTOM_MESSAGE);
        }
        boolean more = rows.size() > pageSize;
        rows = rows.subList(0, Math.min(pageSize, rows.size()));
        int page = after != null ? pageNumber + 1 : 1;
        return page(rows, page, more, page > 1, null, keyOf);
    }

    private static <T> KeysetPage<T> page(List<T> rows, int page, boolean hasNext, boolean hasPrevious,
                                          String message, Function<T, String> keyOf) {
        String first = rows.isEmpty() ? null : keyOf.apply(rows.get(0));
        String last = rows.isEmpty() ? null : keyOf.apply(rows.get(rows.size() - 1));
        return new KeysetPage<>(List.copyOf(rows), page, first, last, hasNext, hasPrevious, message);
    }
}
