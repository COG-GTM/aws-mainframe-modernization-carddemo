package com.carddemo.migration;

import java.math.BigDecimal;
import java.util.LinkedHashMap;
import java.util.Map;

/** Record counts and control totals used for source-versus-target reconciliation (runbook step 5). */
public final class LoadReport {

    private final Map<String, Integer> counts = new LinkedHashMap<>();
    private final Map<String, BigDecimal> controlTotals = new LinkedHashMap<>();

    void count(String table, int rows) {
        counts.merge(table, rows, Integer::sum);
    }

    void total(String name, BigDecimal value) {
        controlTotals.merge(name, value, BigDecimal::add);
    }

    public Map<String, Integer> getCounts() {
        return counts;
    }

    public Map<String, BigDecimal> getControlTotals() {
        return controlTotals;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("VSAM -> RDS load report\n");
        counts.forEach((t, c) -> sb.append(String.format("  %-24s %8d rows%n", t, c)));
        controlTotals.forEach((t, v) -> sb.append(String.format("  %-24s %16s%n", t, v.toPlainString())));
        return sb.toString();
    }
}
