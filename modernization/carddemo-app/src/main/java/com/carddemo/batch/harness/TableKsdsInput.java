package com.carddemo.batch.harness;

import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.data.KeysetPage;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import org.springframework.dao.DataAccessException;
import org.springframework.data.domain.Limit;

final class TableKsdsInput<E, K> implements KsdsInput {

    private final String ddname;
    private final K lowValues;
    private final BiFunction<K, Limit, List<E>> after;
    private final Function<E, K> key;
    private final Function<E, FixedWidthRecord> toRecord;
    private final int pageSize;
    private final Deque<E> buffer = new ArrayDeque<>();
    private boolean open;
    private boolean more;
    private K lastKey;

    TableKsdsInput(String ddname, K lowValues, BiFunction<K, Limit, List<E>> after, Function<E, K> key,
                   Function<E, FixedWidthRecord> toRecord, int pageSize) {
        if (pageSize < 1) {
            throw new IllegalArgumentException("pageSize must be positive");
        }
        this.ddname = ddname;
        this.lowValues = lowValues;
        this.after = after;
        this.key = key;
        this.toRecord = toRecord;
        this.pageSize = pageSize;
    }

    @Override
    public String ddname() {
        return ddname;
    }

    @Override
    public void open() {
        if (open) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.ALREADY_OPEN);
        }
        open = true;
        more = true;
        lastKey = lowValues;
        buffer.clear();
    }

    @Override
    public Optional<FixedWidthRecord> readNext() {
        if (!open) {
            throw new FileStatusException(ddname, "READ", FileStatus.NOT_OPEN_INPUT);
        }
        if (buffer.isEmpty() && more) {
            KeysetPage<E> page;
            try {
                K from = lastKey;
                page = KeysetPage.forward(limit -> after.apply(from, limit), pageSize);
            } catch (DataAccessException e) {
                throw new FileStatusException(ddname, "READ", FileStatus.VSAM_OTHER_ERROR, e);
            }
            buffer.addAll(page.rows());
            more = page.more();
            if (!page.isEmpty()) {
                lastKey = key.apply(page.last());
            }
        }
        E row = buffer.poll();
        return row == null ? Optional.empty() : Optional.of(toRecord.apply(row));
    }

    @Override
    public void close() {
        if (!open) {
            throw new FileStatusException(ddname, "CLOSE", FileStatus.NOT_OPEN);
        }
        open = false;
        buffer.clear();
    }
}
