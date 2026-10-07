package com.carddemo.batch.parity;

import com.carddemo.batch.support.Cbact01cRun;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Java re-implementation of the 29 CBACT01C checks in test-harness/RECONCILIATION_CHECKS.md (mirrors
 * {@code reconcile_cbact01c} in reconcile.py check for check, same ids, same expected/actual values).
 */
final class Reconciliation {

    static final BigDecimal CYC_DEBIT_SUBSTITUTE = new BigDecimal("2525.00");

    private Reconciliation() {
    }

    static final class Check {
        final String id;
        final String group;
        final String description;
        final Object expected;
        final Object actual;

        Check(String id, String group, String description, Object expected, Object actual) {
            this.id = id;
            this.group = group;
            this.description = description;
            this.expected = expected;
            this.actual = actual;
        }

        boolean passed() {
            return Objects.equals(expected, actual);
        }
    }

    static final class Result {
        final List<Check> checks = new ArrayList<>();

        void check(String id, String group, String description, Object expected, Object actual) {
            checks.add(new Check(id, group, description, expected, actual));
        }

        List<String> failed() {
            return checks.stream().filter(c -> !c.passed()).map(c -> c.id).collect(Collectors.toList());
        }

        int passed() {
            return (int) checks.stream().filter(Check::passed).count();
        }

        Object actual(String id) {
            return checks.stream().filter(c -> c.id.equals(id)).findFirst().map(c -> c.actual).orElse(null);
        }

        void assertGroup(String group) {
            for (Check c : checks) {
                if (c.group.equals(group)) {
                    assertEquals(c.expected, c.actual, c.id + ": " + c.description);
                }
            }
        }

        String report() {
            StringBuilder sb = new StringBuilder();
            for (Check c : checks) {
                sb.append(String.format("  [%s] %-36s %s%n", c.passed() ? "PASS" : "FAIL", c.id, c.description));
                if (!c.passed()) {
                    sb.append("         expected: ").append(c.expected).append('\n');
                    sb.append("         actual:   ").append(c.actual).append('\n');
                }
            }
            return sb.toString();
        }
    }

    static String money(BigDecimal v) {
        return v.setScale(2).toPlainString();
    }

    static BigDecimal dec(Object v) {
        return new BigDecimal((String) v);
    }

    static BigDecimal decOrNull(Object v) {
        try {
            return new BigDecimal((String) v);
        } catch (NumberFormatException | ClassCastException e) {
            return null;
        }
    }

    static BigDecimal sum(List<Map<String, Object>> recs, Function<Map<String, Object>, Object> f) {
        BigDecimal t = BigDecimal.ZERO;
        for (Map<String, Object> r : recs) {
            t = t.add(dec(f.apply(r)));
        }
        return t;
    }

    @SuppressWarnings("unchecked")
    static Map<String, Object> occ(Map<String, Object> arr, int occurrence) {
        return ((List<Map<String, Object>>) arr.get("ARR-ACCT-BAL")).get(occurrence - 1);
    }

    static Result cbact01c(Cbact01cRun run) {
        return cbact01c(run.inputJson(), run.outfileJson(), run.arryfileJson(), run.vbrcfileJson());
    }

    static Result cbact01c(List<Map<String, Object>> acctIn, List<Map<String, Object>> outfile,
                           List<Map<String, Object>> arryfile, List<Map<String, Object>> vbrcfile) {
        Result r = new Result();
        int n = acctIn.size();
        List<Map<String, Object>> vb1 = vbrcfile.stream().filter(x -> "VBRC-REC1".equals(x.get("_record"))).collect(Collectors.toList());
        List<Map<String, Object>> vb2 = vbrcfile.stream().filter(x -> "VBRC-REC2".equals(x.get("_record"))).collect(Collectors.toList());

        // -- record counts
        r.check("CBACT01C-COUNT-01", "counts", "input accounts = OUTFILE records", n, outfile.size());
        r.check("CBACT01C-COUNT-02", "counts", "input accounts = ARRYFILE records", n, arryfile.size());
        r.check("CBACT01C-COUNT-03", "counts", "input accounts = VBRCFILE records / 2 (and the count is even)",
                Map.of("accounts", n, "even", true), Map.of("accounts", vbrcfile.size() / 2, "even", vbrcfile.size() % 2 == 0));
        boolean alternating = true;
        for (int i = 0; i < vbrcfile.size(); i++) {
            alternating &= (i % 2 == 0 ? "VBRC-REC1" : "VBRC-REC2").equals(vbrcfile.get(i).get("_record"));
        }
        r.check("CBACT01C-COUNT-04", "counts", "VBRCFILE: one 12-byte VBRC-REC1 and one 39-byte VBRC-REC2 per account, alternating",
                Map.of("rec1", n, "rec2", n, "alternating", true), Map.of("rec1", vb1.size(), "rec2", vb2.size(), "alternating", alternating));

        // -- field totals
        for (String fld : List.of("ACCT-CURR-BAL", "ACCT-CREDIT-LIMIT", "ACCT-CASH-CREDIT-LIMIT", "ACCT-CURR-CYC-CREDIT")) {
            r.check("CBACT01C-TOTAL-" + fld, "totals", "sum " + fld + " in = sum OUT-" + fld,
                    money(sum(acctIn, x -> x.get(fld))), money(sum(outfile, x -> x.get("OUT-" + fld))));
        }
        // legacy simulation: 2525.00 on a zero input, otherwise the previous record's output value;
        // rows before the first zero input are undefined (never-assigned FD storage) and excluded
        List<BigDecimal> expectedRows = new ArrayList<>();
        BigDecimal carry = null;
        for (Map<String, Object> x : acctIn) {
            if (dec(x.get("ACCT-CURR-CYC-DEBIT")).signum() == 0) {
                carry = CYC_DEBIT_SUBSTITUTE;
            }
            expectedRows.add(carry);
        }
        BigDecimal expectedTotal = BigDecimal.ZERO;
        BigDecimal definedActual = BigDecimal.ZERO;
        List<Map<String, Object>> mismatched = new ArrayList<>();
        int undefined = 0;
        for (int i = 0; i < Math.min(outfile.size(), expectedRows.size()); i++) {
            BigDecimal e = expectedRows.get(i);
            BigDecimal a = decOrNull(outfile.get(i).get("OUT-ACCT-CURR-CYC-DEBIT"));
            if (e == null) {
                undefined++;
                continue;
            }
            expectedTotal = expectedTotal.add(e);
            if (a != null) {
                definedActual = definedActual.add(a);
            }
            if (a == null || e.compareTo(a) != 0) {
                mismatched.add(Map.of("acct_id", outfile.get(i).get("OUT-ACCT-ID"), "expected", money(e),
                        "actual", String.valueOf(outfile.get(i).get("OUT-ACCT-CURR-CYC-DEBIT"))));
            }
        }
        r.check("CBACT01C-TOTAL-CYC-DEBIT", "totals",
                "sum OUT-ACCT-CURR-CYC-DEBIT = legacy simulation (2525.00 where input ACCT-CURR-CYC-DEBIT = 0, "
                        + "otherwise the previous record's output value is retained; rows before the first zero input are undefined and excluded)",
                money(expectedTotal), money(definedActual));
        for (Map.Entry<String, BigDecimal> c : arrConstants().entrySet()) {
            String[] parts = c.getKey().split("@");
            int occurrence = Integer.parseInt(parts[1]);
            String fld = parts[0];
            r.check("CBACT01C-TOTAL-ARR-" + fld + "-" + occurrence, "totals",
                    "sum " + fld + "(" + occurrence + ") = records x " + c.getValue(),
                    money(c.getValue().multiply(BigDecimal.valueOf(arryfile.size()))),
                    money(sum(arryfile, x -> occ(x, occurrence).get(fld))));
        }
        for (int occurrence : new int[] {1, 2}) {
            r.check("CBACT01C-TOTAL-ARR-ACCT-CURR-BAL-" + occurrence, "totals",
                    "sum ARR-ACCT-CURR-BAL(" + occurrence + ") = sum input ACCT-CURR-BAL",
                    money(sum(acctIn, x -> x.get("ACCT-CURR-BAL"))),
                    money(sum(arryfile, x -> occ(x, occurrence).get("ARR-ACCT-CURR-BAL"))));
        }
        for (int occurrence : new int[] {4, 5}) {
            r.check("CBACT01C-TOTAL-ARR-ZERO-" + occurrence, "totals",
                    "ARR-ACCT-BAL(" + occurrence + ") is INITIALIZEd to zero (both fields) in every record",
                    Map.of("curr_bal", "0.00", "cyc_debit", "0.00"),
                    Map.of("curr_bal", money(sum(arryfile, x -> occ(x, occurrence).get("ARR-ACCT-CURR-BAL"))),
                            "cyc_debit", money(sum(arryfile, x -> occ(x, occurrence).get("ARR-ACCT-CURR-CYC-DEBIT")))));
        }
        r.check("CBACT01C-TOTAL-VB2-CURR-BAL", "totals", "sum VB2-ACCT-CURR-BAL = sum input ACCT-CURR-BAL",
                money(sum(acctIn, x -> x.get("ACCT-CURR-BAL"))), money(sum(vb2, x -> x.get("VB2-ACCT-CURR-BAL"))));
        r.check("CBACT01C-TOTAL-VB2-CREDIT-LIMIT", "totals", "sum VB2-ACCT-CREDIT-LIMIT = sum input ACCT-CREDIT-LIMIT",
                money(sum(acctIn, x -> x.get("ACCT-CREDIT-LIMIT"))), money(sum(vb2, x -> x.get("VB2-ACCT-CREDIT-LIMIT"))));

        // -- derived fields
        Map<String, Map<String, Object>> byId = new LinkedHashMap<>();
        for (Map<String, Object> x : acctIn) {
            byId.put((String) x.get("ACCT-ID"), x);
        }
        List<Object> badDates = new ArrayList<>();
        for (Map<String, Object> x : outfile) {
            Map<String, Object> in = byId.get(x.get("OUT-ACCT-ID"));
            if (in != null && !yyyymmdd((String) in.get("ACCT-REISSUE-DATE")).equals(x.get("OUT-ACCT-REISSUE-DATE"))) {
                badDates.add(x.get("OUT-ACCT-ID"));
            }
        }
        r.check("CBACT01C-FIELD-REISSUE-DATE", "derived", "OUT-ACCT-REISSUE-DATE = COBDATFT(YYYY-MM-DD -> YYYYMMDD) + 2 spaces",
                0, badDates.size());
        r.check("CBACT01C-FIELD-CYC-DEBIT", "derived",
                "OUT-ACCT-CURR-CYC-DEBIT per record = legacy simulation (2525.00 on zero input, else carried value)",
                0, mismatched.size());
        List<Object> badYyyy = new ArrayList<>();
        for (Map<String, Object> x : vb2) {
            Map<String, Object> in = byId.get(x.get("VB2-ACCT-ID"));
            if (in != null && !((String) in.get("ACCT-REISSUE-DATE")).substring(0, 4).equals(x.get("VB2-ACCT-REISSUE-YYYY"))) {
                badYyyy.add(x.get("VB2-ACCT-ID"));
            }
        }
        r.check("CBACT01C-FIELD-VB2-YYYY", "derived", "VB2-ACCT-REISSUE-YYYY = first 4 chars of input ACCT-REISSUE-DATE", 0, badYyyy.size());
        List<Object> badStatus = new ArrayList<>();
        for (Map<String, Object> x : vb1) {
            Map<String, Object> in = byId.get(x.get("VB1-ACCT-ID"));
            if (in != null && !in.get("ACCT-ACTIVE-STATUS").equals(x.get("VB1-ACCT-ACTIVE-STATUS"))) {
                badStatus.add(x.get("VB1-ACCT-ID"));
            }
        }
        r.check("CBACT01C-FIELD-VB1-STATUS", "derived", "VB1-ACCT-ACTIVE-STATUS = input ACCT-ACTIVE-STATUS", 0, badStatus.size());

        // -- cross-reference integrity
        Map<String, Long> inIds = acctIn.stream().collect(Collectors.groupingBy(x -> (String) x.get("ACCT-ID"), TreeMap::new, Collectors.counting()));
        r.check("CBACT01C-XREF-00", "xref", "input ACCT-ID is unique (KSDS primary key)", 0,
                (int) inIds.values().stream().filter(c -> c > 1).count());
        xref(r, "CBACT01C-XREF-01", "OUT-ACCT-ID", outfile, inIds);
        xref(r, "CBACT01C-XREF-02", "ARR-ACCT-ID", arryfile, inIds);
        xref(r, "CBACT01C-XREF-03", "VB1-ACCT-ID", vb1, inIds);
        xref(r, "CBACT01C-XREF-04", "VB2-ACCT-ID", vb2, inIds);
        List<String> sortedIn = acctIn.stream().map(x -> (String) x.get("ACCT-ID"))
                .sorted(Comparator.comparing(Long::parseLong)).collect(Collectors.toList());
        List<String> outIds = outfile.stream().map(x -> (String) x.get("OUT-ACCT-ID")).collect(Collectors.toList());
        r.check("CBACT01C-XREF-05", "xref", "OUTFILE order = input key order (sequential KSDS read is ascending by ACCT-ID)",
                sortedIn.equals(outIds), true);
        return r;
    }

    private static void xref(Result r, String id, String label, List<Map<String, Object>> recs, Map<String, Long> inIds) {
        Map<String, Long> outIds = recs.stream().collect(Collectors.groupingBy(x -> (String) x.get(label), TreeMap::new, Collectors.counting()));
        List<String> unknown = outIds.keySet().stream().filter(k -> !inIds.containsKey(k)).sorted().collect(Collectors.toList());
        List<String> dup = outIds.entrySet().stream().filter(e -> e.getValue() > 1).map(Map.Entry::getKey).sorted().collect(Collectors.toList());
        List<String> missing = inIds.keySet().stream().filter(k -> !outIds.containsKey(k)).sorted().collect(Collectors.toList());
        r.check(id, "xref", "every " + label + " exists exactly once in the input and every input account appears once",
                Map.of("unknown", List.of(), "duplicated", List.of(), "missing", List.of()),
                Map.of("unknown", unknown, "duplicated", dup, "missing", missing));
    }

    /** What COBDATFT type 2 -> 2 does, then MOVE X(20) -> X(10). */
    static String yyyymmdd(String iso) {
        return String.format("%-10s", iso.substring(0, 4) + iso.substring(5, 7) + iso.substring(8, 10));
    }

    static Map<String, BigDecimal> arrConstants() {
        Map<String, BigDecimal> m = new TreeMap<>();
        m.put("ARR-ACCT-CURR-CYC-DEBIT@1", new BigDecimal("1005.00"));
        m.put("ARR-ACCT-CURR-CYC-DEBIT@2", new BigDecimal("1525.00"));
        m.put("ARR-ACCT-CURR-BAL@3", new BigDecimal("-1025.00"));
        m.put("ARR-ACCT-CURR-CYC-DEBIT@3", new BigDecimal("-2500.00"));
        return m;
    }
}
