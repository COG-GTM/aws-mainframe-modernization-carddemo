package com.carddemo.common.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import org.springframework.data.domain.Limit;

/**
 * One screen of a CICS browse (ADR-0011). The online programs read one record past the screen to decide whether
 * PF8/PF7 can page further ({@code READNEXT}/{@code READPREV} after the last row); {@code more} is that look-ahead.
 *
 * @param rows the rows in key (display) order, at most the screen size
 * @param more whether another record exists beyond the screen in the browse direction
 */
public record KeysetPage<T>(List<T> rows, boolean more) {

    public KeysetPage {
        rows = List.copyOf(rows);
    }

    /** {@code STARTBR} + {@code READNEXT} x screen: {@code query} must return ascending key order. */
    public static <T> KeysetPage<T> forward(Function<Limit, List<T>> query, int screenRows) {
        List<T> found = query.apply(Limit.of(screenRows + 1));
        boolean more = found.size() > screenRows;
        return new KeysetPage<>(more ? found.subList(0, screenRows) : found, more);
    }

    /**
     * {@code STARTBR} + {@code READPREV} x screen: {@code query} must return descending key order; the page is
     * reversed into display (ascending) order, as the programs fill the screen bottom-up.
     */
    public static <T> KeysetPage<T> backward(Function<Limit, List<T>> query, int screenRows) {
        List<T> found = query.apply(Limit.of(screenRows + 1));
        boolean more = found.size() > screenRows;
        List<T> page = new ArrayList<>(more ? found.subList(0, screenRows) : found);
        Collections.reverse(page);
        return new KeysetPage<>(page, more);
    }

    public T first() {
        return rows.get(0);
    }

    public T last() {
        return rows.get(rows.size() - 1);
    }

    public boolean isEmpty() {
        return rows.isEmpty();
    }
}
