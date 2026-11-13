package com.carddemo.common.testing;

import java.util.List;

/** Outcome of a golden-master comparison of two record sets. */
public record ComparisonReport(String label, int expectedCount, int actualCount, List<RecordDifference> differences) {

    public boolean matches() {
        return expectedCount == actualCount && differences.isEmpty();
    }

    public String format(int limit) {
        StringBuilder sb = new StringBuilder("Golden master [").append(label).append("]: ");
        if (expectedCount != actualCount) {
            sb.append("expected ").append(expectedCount).append(" records but got ").append(actualCount).append("; ");
        }
        sb.append(differences.size()).append(" field difference(s)");
        differences.stream().limit(limit).forEach(d -> sb.append("\n  ").append(d));
        if (differences.size() > limit) {
            sb.append("\n  ... ").append(differences.size() - limit).append(" more");
        }
        return sb.toString();
    }
}
