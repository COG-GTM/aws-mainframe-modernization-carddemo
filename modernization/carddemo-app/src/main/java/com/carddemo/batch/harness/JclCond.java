package com.carddemo.batch.harness;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The {@code COND} parameter of a JCL {@code EXEC} statement: the step is <em>bypassed</em> when any test
 * {@code (code,operator[,stepname])} is true, i.e. when {@code code operator RC} holds for a previous step's RC
 * (all executed previous steps, or the named one). {@code COND=(4,LT)} bypasses the step if any earlier RC is
 * greater than 4. After an abend the step is bypassed unless it says {@code EVEN} (run anyway) or {@code ONLY}
 * (run only after an abend). No {@code COND} at all: run unless an earlier step abended.
 */
public record JclCond(List<Test> tests, AbendRule abendRule) {

    /** Runs the step unless a previous step abended. */
    public static final JclCond NONE = new JclCond(List.of(), AbendRule.NORMAL);

    public enum Operator {
        GT, GE, EQ, NE, LT, LE;

        boolean test(int code, int rc) {
            return switch (this) {
                case GT -> code > rc;
                case GE -> code >= rc;
                case EQ -> code == rc;
                case NE -> code != rc;
                case LT -> code < rc;
                case LE -> code <= rc;
            };
        }
    }

    public enum AbendRule { NORMAL, EVEN, ONLY }

    /** One {@code (code,operator[,stepname])} test; {@code stepName} null = every previous step. */
    public record Test(int code, Operator operator, String stepName) {

        public Test {
            if (code < 0 || code > 4095) {
                throw new IllegalArgumentException("COND code must be 0..4095, got " + code);
            }
        }
    }

    private static final Pattern TEST = Pattern.compile("\\(\\s*(\\d+)\\s*,\\s*([A-Z]{2})\\s*(?:,\\s*([A-Z0-9#@$.]+)\\s*)?\\)");

    public JclCond {
        tests = List.copyOf(tests);
        if (tests.size() > 8) {
            throw new IllegalArgumentException("JCL allows at most 8 COND tests, got " + tests.size());
        }
    }

    /**
     * Parses the value of {@code COND=}: {@code (4,LT)}, {@code ((0,NE,STEP05),(4,LT))}, {@code EVEN},
     * {@code ((4,LT),EVEN)}, {@code ONLY}.
     */
    public static JclCond parse(String text) {
        String s = text.strip().toUpperCase(Locale.ROOT);
        if (s.isEmpty()) {
            return NONE;
        }
        AbendRule rule = s.contains("EVEN") ? AbendRule.EVEN : s.contains("ONLY") ? AbendRule.ONLY : AbendRule.NORMAL;
        List<Test> tests = new ArrayList<>();
        Matcher m = TEST.matcher(s);
        while (m.find()) {
            tests.add(new Test(Integer.parseInt(m.group(1)), Operator.valueOf(m.group(2)), m.group(3)));
        }
        String rest = TEST.matcher(s).replaceAll("").replaceAll("EVEN|ONLY", "").replaceAll("[(),\\s]", "");
        if (!rest.isEmpty() || (tests.isEmpty() && rule == AbendRule.NORMAL)) {
            throw new IllegalArgumentException("invalid COND: " + text);
        }
        return new JclCond(tests, rule);
    }

    /**
     * Whether the step is bypassed, given the RCs of the steps that executed before it (in order) and whether one of
     * them abended.
     */
    public boolean bypass(Map<String, ReturnCode> previous, boolean abended) {
        if (abended && abendRule == AbendRule.NORMAL) {
            return true;
        }
        if (!abended && abendRule == AbendRule.ONLY) {
            return true;
        }
        for (Test test : tests) {
            if (test.stepName() != null) {
                ReturnCode rc = previous.get(test.stepName());
                if (rc != null && test.operator().test(test.code(), rc.code())) {
                    return true;
                }
            } else if (previous.values().stream().anyMatch(rc -> test.operator().test(test.code(), rc.code()))) {
                return true;
            }
        }
        return false;
    }
}
