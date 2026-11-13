package com.carddemo.common.testing;

import com.carddemo.common.cobol.CobolDataException;
import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.cobol.Hex;
import com.carddemo.common.copybook.CopybookField;
import com.carddemo.common.copybook.RecordLayout;
import com.carddemo.common.copybook.RecordTranscoder;
import com.carddemo.common.copybook.RecordView;
import org.opentest4j.AssertionFailedError;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Golden-master assertions for migration parity.
 *
 * <ul>
 *   <li>{@link #compare}/{@link #assertMatches}: record-by-record, field-by-field comparison of Java output with
 *   the expected mainframe output, both described by the same copybook (encodings may differ, e.g. EBCDIC expected
 *   vs ASCII actual). Values are compared decoded, so a zoned {@code +194.00} matches regardless of code page.</li>
 *   <li>{@link #assertMatchesSnapshot}: approval testing of text output against
 *   {@code src/test/resources/golden/<name>.golden}; run with {@code -Dgolden.update=true} to (re)approve.</li>
 * </ul>
 */
public final class GoldenMaster {

    public static final String UPDATE_PROPERTY = "golden.update";

    private final Path snapshotDir;

    public GoldenMaster(Path snapshotDir) {
        this.snapshotDir = snapshotDir;
    }

    /** Snapshots live in the module's {@code src/test/resources/golden} (Surefire runs in the module directory). */
    public static GoldenMaster forCurrentModule() {
        return new GoldenMaster(Paths.get("src", "test", "resources", "golden").toAbsolutePath());
    }

    public Path snapshotDir() {
        return snapshotDir;
    }

    public ComparisonReport compare(String label, RecordLayout layout, List<byte[]> expected,
                                    DataEncoding expectedEncoding, List<byte[]> actual, DataEncoding actualEncoding) {
        return compare(label, layout, expected, expectedEncoding, actual, actualEncoding, true);
    }

    /**
     * @param includeFiller also compare FILLER/unmapped bytes (reported as field {@code FILLER}); disable when the
     *                      target model deliberately does not keep FILLER content (e.g. data round-tripped via a database)
     */
    public ComparisonReport compare(String label, RecordLayout layout, List<byte[]> expected,
                                    DataEncoding expectedEncoding, List<byte[]> actual, DataEncoding actualEncoding,
                                    boolean includeFiller) {
        List<RecordDifference> diffs = new ArrayList<>();
        List<CopybookField> fields = layout.primaryElementaryFields();
        int n = Math.min(expected.size(), actual.size());
        for (int r = 0; r < n; r++) {
            RecordView e = RecordView.wrap(layout, expectedEncoding, expected.get(r));
            RecordView a = RecordView.wrap(layout, actualEncoding, actual.get(r));
            boolean fieldDiff = false;
            for (CopybookField f : fields) {
                for (int[] idx : indexCombinations(f)) {
                    String ev = render(e, f, idx);
                    String av = render(a, f, idx);
                    if (!ev.equals(av)) {
                        fieldDiff = true;
                        diffs.add(new RecordDifference(r + 1, f.name() + suffix(idx), RecordView.offset(f, idx), ev, av));
                    }
                }
            }
            if (!fieldDiff && includeFiller) {
                byte[] normalized = RecordTranscoder.transcode(actual.get(r), layout, actualEncoding, expectedEncoding);
                byte[] exp = Arrays.copyOf(expected.get(r), layout.length());
                if (!Arrays.equals(exp, Arrays.copyOf(normalized, layout.length()))) {
                    diffs.add(new RecordDifference(r + 1, "FILLER", -1, Hex.of(exp), Hex.of(normalized)));
                }
            }
        }
        return new ComparisonReport(label, expected.size(), actual.size(), List.copyOf(diffs));
    }

    public void assertMatches(String label, RecordLayout layout, List<byte[]> expected, DataEncoding expectedEncoding,
                              List<byte[]> actual, DataEncoding actualEncoding, KnownDifference... known) {
        assertMatches(compare(label, layout, expected, expectedEncoding, actual, actualEncoding), known);
    }

    public void assertMatches(ComparisonReport report, KnownDifference... known) {
        Set<String> declared = new HashSet<>();
        for (KnownDifference k : known) {
            declared.add(k.recordNumber() + ":" + k.field());
        }
        List<RecordDifference> unexpected = report.differences().stream()
                .filter(d -> !declared.remove(d.recordNumber() + ":" + d.field()))
                .toList();
        if (report.expectedCount() != report.actualCount() || !unexpected.isEmpty() || !declared.isEmpty()) {
            ComparisonReport failing = new ComparisonReport(report.label(), report.expectedCount(),
                    report.actualCount(), unexpected);
            String stale = declared.isEmpty() ? "" : "\n  declared known differences not observed: " + declared;
            throw new AssertionFailedError(failing.format(25) + stale);
        }
    }

    /** Approval test of text lines against {@code <snapshotDir>/<name>.golden}. */
    public void assertMatchesSnapshot(String name, List<String> actualLines) {
        Path file = snapshotDir.resolve(name + ".golden");
        String actual = String.join("\n", actualLines) + "\n";
        boolean update = Boolean.getBoolean(UPDATE_PROPERTY);
        try {
            if (update || !Files.exists(file)) {
                Files.createDirectories(file.getParent());
                Files.writeString(file, actual, StandardCharsets.UTF_8);
                if (!update) {
                    throw new AssertionFailedError("Golden snapshot " + file + " did not exist and was created; review "
                            + "and commit it (or run with -D" + UPDATE_PROPERTY + "=true to approve changes).");
                }
                return;
            }
            String expected = Files.readString(file, StandardCharsets.UTF_8);
            if (!expected.equals(actual)) {
                List<String> exp = expected.lines().toList();
                int line = 0;
                while (line < Math.min(exp.size(), actualLines.size()) && exp.get(line).equals(actualLines.get(line))) {
                    line++;
                }
                throw new AssertionFailedError("Output differs from golden snapshot " + file + " at line " + (line + 1)
                        + " (run with -D" + UPDATE_PROPERTY + "=true to approve)", expected, actual);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static String render(RecordView view, CopybookField f, int[] idx) {
        try {
            if (f.pic().isNumeric()) {
                return view.getDecimal(f, idx).toPlainString();
            }
            return view.getString(f, idx);
        } catch (CobolDataException e) {
            int offset = RecordView.offset(f, idx);
            return "INVALID x'" + Hex.of(view.bytes(), offset, f.length()) + "'";
        }
    }

    private static List<int[]> indexCombinations(CopybookField f) {
        List<int[]> result = new ArrayList<>();
        result.add(new int[0]);
        for (CopybookField o : f.occursAncestors()) {
            List<int[]> next = new ArrayList<>();
            for (int[] base : result) {
                for (int i = 0; i < o.occurs(); i++) {
                    int[] idx = Arrays.copyOf(base, base.length + 1);
                    idx[base.length] = i;
                    next.add(idx);
                }
            }
            result = next;
        }
        return result;
    }

    private static String suffix(int[] idx) {
        if (idx.length == 0) {
            return "";
        }
        StringBuilder sb = new StringBuilder("(");
        for (int i = 0; i < idx.length; i++) {
            sb.append(i == 0 ? "" : ",").append(idx[i] + 1);
        }
        return sb.append(')').toString();
    }
}
