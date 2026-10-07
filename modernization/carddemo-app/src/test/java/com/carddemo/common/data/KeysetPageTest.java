package com.carddemo.common.data;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.stream.IntStream;
import org.junit.jupiter.api.Test;

class KeysetPageTest {

    static List<Integer> upTo(int n) {
        return IntStream.rangeClosed(1, n).boxed().toList();
    }

    @Test
    void forwardReadsOneRowAheadToSetTheMoreFlag() {
        int[] requested = new int[1];
        KeysetPage<Integer> page = KeysetPage.forward(limit -> {
            requested[0] = limit.max();
            return upTo(8);
        }, 7);
        assertThat(requested[0]).isEqualTo(8);
        assertThat(page.rows()).containsExactly(1, 2, 3, 4, 5, 6, 7);
        assertThat(page.more()).isTrue();
        assertThat(page.first()).isEqualTo(1);
        assertThat(page.last()).isEqualTo(7);
    }

    @Test
    void forwardShortPageHasNoMore() {
        KeysetPage<Integer> page = KeysetPage.forward(limit -> upTo(7), 7);
        assertThat(page.rows()).hasSize(7);
        assertThat(page.more()).isFalse();
        assertThat(KeysetPage.forward(limit -> List.<Integer>of(), 10).isEmpty()).isTrue();
    }

    @Test
    void backwardReversesDescendingRowsIntoDisplayOrder() {
        KeysetPage<Integer> page = KeysetPage.backward(limit -> List.of(30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20),
                10);
        assertThat(page.rows()).containsExactly(21, 22, 23, 24, 25, 26, 27, 28, 29, 30);
        assertThat(page.more()).isTrue();
        assertThat(KeysetPage.backward(limit -> List.of(3, 2, 1), 10).rows()).containsExactly(1, 2, 3);
    }
}
