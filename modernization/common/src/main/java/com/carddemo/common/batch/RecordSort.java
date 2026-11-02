package com.carddemo.common.batch;

import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.Field;
import com.carddemo.common.record.RecordLayout;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.function.Predicate;

/**
 * An in-job DFSORT step: {@code SORT FIELDS=(p,l,CH,A|D,...)}, an optional {@code INCLUDE COND}
 * and an optional {@code OUTREC FIELDS=(o:p,l,...)}. Several SORTIN data sets are concatenated in
 * order. Keys compare as unsigned bytes in the records' code page (CH), so EBCDIC and ASCII files
 * sort as they do on the mainframe and under GnuCOBOL respectively. The sort is stable.
 */
public final class RecordSort {

    /** A {@code SORT FIELDS} key; {@code position} is 1-based as in the control statement. */
    public record Key(int position, int length, boolean ascending) {
        public Key {
            if (position < 1 || length < 1) {
                throw new IllegalArgumentException("Bad sort key " + position + "," + length);
            }
        }

        public static Key ascending(int position, int length) {
            return new Key(position, length, true);
        }

        public static Key descending(int position, int length) {
            return new Key(position, length, false);
        }

        public static Key ascending(Field field) {
            return ascending(field.offset() + 1, field.length());
        }
    }

    /** {@code o:p,l} of {@code OUTREC FIELDS}: copy {@code length} bytes from input {@code p} to output {@code o}. */
    public record OutrecField(int outPosition, int inPosition, int length) {
    }

    private final List<Key> keys;
    private final Predicate<byte[]> include;
    private final List<OutrecField> outrec;

    private RecordSort(List<Key> keys, Predicate<byte[]> include, List<OutrecField> outrec) {
        if (keys.isEmpty()) {
            throw new IllegalArgumentException("SORT FIELDS needs at least one key");
        }
        this.keys = List.copyOf(keys);
        this.include = include;
        this.outrec = List.copyOf(outrec);
    }

    public static RecordSort fields(Key... keys) {
        return new RecordSort(Arrays.asList(keys), r -> true, List.of());
    }

    /** {@code INCLUDE COND=(...)}: only matching input records are sorted. */
    public RecordSort include(Predicate<byte[]> condition) {
        return new RecordSort(keys, condition, outrec);
    }

    public RecordSort outrec(OutrecField... fields) {
        return new RecordSort(keys, include, Arrays.asList(fields));
    }

    /** Sorts raw records; an OUTREC is applied after sorting, gaps filled with {@code space}. */
    public List<byte[]> sort(Collection<byte[]> sortin, int outLength, byte space) {
        List<byte[]> selected = new ArrayList<>();
        for (byte[] record : sortin) {
            if (include.test(record)) {
                selected.add(record);
            }
        }
        selected.sort(comparator());
        if (outrec.isEmpty()) {
            return selected;
        }
        List<byte[]> out = new ArrayList<>(selected.size());
        for (byte[] record : selected) {
            out.add(reformat(record, outLength, space));
        }
        return out;
    }

    /** Sorts records of the SORTIN layouts into records of {@code outLayout} (the SORTOUT LRECL). */
    public List<CobolRecord> sort(List<List<CobolRecord>> sortin, RecordLayout outLayout) {
        List<byte[]> raw = new ArrayList<>();
        var codePage = sortin.stream().flatMap(List::stream).map(CobolRecord::codePage).findFirst().orElse(null);
        for (List<CobolRecord> dataset : sortin) {
            for (CobolRecord record : dataset) {
                if (record.codePage() != codePage) {
                    throw new IllegalArgumentException("SORTIN data sets mix code pages");
                }
                if (outrec.isEmpty() && record.layout().length() != outLayout.length()) {
                    throw new IllegalArgumentException("SORTIN LRECL " + record.layout().length()
                            + " differs from SORTOUT LRECL " + outLayout.length() + " without OUTREC");
                }
                raw.add(record.toBytes());
            }
        }
        if (codePage == null) {
            return List.of();
        }
        byte space = codePage.encodeText(" ")[0];
        return sort(raw, outLayout.length(), space).stream()
                .map(bytes -> CobolRecord.of(outLayout, codePage, bytes))
                .toList();
    }

    private Comparator<byte[]> comparator() {
        Comparator<byte[]> result = null;
        for (Key key : keys) {
            Comparator<byte[]> next = (a, b) -> Arrays.compareUnsigned(
                    a, key.position() - 1, key.position() - 1 + key.length(),
                    b, key.position() - 1, key.position() - 1 + key.length());
            if (!key.ascending()) {
                next = next.reversed();
            }
            result = result == null ? next : result.thenComparing(next);
        }
        return result;
    }

    private byte[] reformat(byte[] record, int outLength, byte space) {
        byte[] out = new byte[outLength];
        Arrays.fill(out, space);
        for (OutrecField field : outrec) {
            System.arraycopy(record, field.inPosition() - 1, out, field.outPosition() - 1, field.length());
        }
        return out;
    }
}
