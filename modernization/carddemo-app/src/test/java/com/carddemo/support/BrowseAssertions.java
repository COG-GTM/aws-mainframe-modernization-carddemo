package com.carddemo.support;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.data.KeysetPage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

/**
 * Walks a whole file screen by screen with PF8 then back with PF7 and checks the result against the expected key
 * order: every screen is full except the last, the look-ahead flag is set on all but the final screen, and the
 * concatenated screens reproduce {@code expectedKeys} exactly (no row skipped or repeated at a page boundary).
 */
public final class BrowseAssertions {

    private BrowseAssertions() {
    }

    public static <T> void assertFullBrowse(List<String> expectedKeys, int screenRows, Function<T, String> key,
                                            Function<String, KeysetPage<T>> browseFrom,
                                            Function<String, KeysetPage<T>> nextPage,
                                            Function<String, KeysetPage<T>> previousPage) {
        int screens = (expectedKeys.size() + screenRows - 1) / screenRows;
        List<String> forward = new ArrayList<>();
        List<List<String>> pages = new ArrayList<>();
        KeysetPage<T> page = browseFrom.apply("");
        for (int i = 1; i <= screens; i++) {
            List<String> keys = page.rows().stream().map(key).toList();
            pages.add(keys);
            forward.addAll(keys);
            boolean last = i == screens;
            assertThat(page.more()).as("screen %d look-ahead", i).isEqualTo(!last);
            assertThat(keys).as("screen %d rows", i).hasSize(last ? expectedKeys.size() - (screens - 1) * screenRows
                    : screenRows);
            if (!last) {
                page = nextPage.apply(keys.get(keys.size() - 1));
            }
        }
        assertThat(forward).containsExactlyElementsOf(expectedKeys);
        assertThat(nextPage.apply(expectedKeys.get(expectedKeys.size() - 1)).rows()).as("PF8 past end").isEmpty();

        List<String> backward = new ArrayList<>(pages.get(pages.size() - 1));
        String first = backward.get(0);
        for (int i = screens - 1; i >= 1; i--) {
            KeysetPage<T> prev = previousPage.apply(first);
            List<String> keys = prev.rows().stream().map(key).toList();
            assertThat(keys).as("PF7 to screen %d", i).containsExactlyElementsOf(pages.get(i - 1));
            assertThat(prev.more()).as("PF7 look-ahead at screen %d", i).isEqualTo(i > 1);
            List<String> merged = new ArrayList<>(keys);
            merged.addAll(backward);
            backward = merged;
            first = keys.get(0);
        }
        assertThat(previousPage.apply(expectedKeys.get(0)).rows()).as("PF7 before start").isEmpty();
        assertThat(backward).containsExactlyElementsOf(expectedKeys);
    }

    public static List<String> sorted(List<String> keys) {
        List<String> copy = new ArrayList<>(keys);
        Collections.sort(copy);
        return copy;
    }
}
