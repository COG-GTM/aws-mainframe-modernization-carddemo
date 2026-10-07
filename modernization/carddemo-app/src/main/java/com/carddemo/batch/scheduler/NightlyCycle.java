package com.carddemo.batch.scheduler;

import java.util.List;
import java.util.Locale;
import java.util.Optional;

/**
 * The nightly batch cycle (docs/modernization/06-scheduling.md): the in-scope jobs of {@code app/scheduler/CardDemo.ca7}
 * and {@code CardDemo.controlm} in the GnuCOBOL baseline run order ({@code docs/validation/baseline/00-ORDER.md}), each
 * with the predecessors whose successful end (RC &lt;= 4, no abend) triggers it, the scheduler-condition equivalent of
 * {@code COND=(4,LT,<predecessor>)}.
 */
public final class NightlyCycle {

    public static final String NAME = "nightly-cycle";

    /** RC up to which a predecessor counts as ended OK (Control-M OUTCOND added / CA-7 trigger fired). */
    public static final int MAX_OK_RC = 4;

    /**
     * One job of the cycle: the JCL job name, the harness job or {@code JobStream} that implements it, the members
     * that must have ended OK before it runs, and the KSDS datasets it updates (unloaded as after-images on request).
     */
    public record Member(String name, String implementation, List<String> predecessors, List<String> afterImages) {

        public Member {
            name = name.toUpperCase(Locale.ROOT);
            predecessors = List.copyOf(predecessors);
            afterImages = List.copyOf(afterImages);
        }

        /** {@code COND=((4,LT,A),(4,LT,B))} for predecessors A and B; empty when the member has none. */
        public String cond() {
            if (predecessors.isEmpty()) {
                return "";
            }
            StringBuilder cond = new StringBuilder("(");
            for (String predecessor : predecessors) {
                if (cond.length() > 1) {
                    cond.append(',');
                }
                cond.append('(').append(MAX_OK_RC).append(",LT,").append(predecessor).append(')');
            }
            return cond.append(')').toString();
        }
    }

    public static final List<Member> MEMBERS = List.of(
            new Member("READACCT", "readacct", List.of(), List.of()),
            new Member("READCARD", "readcard", List.of("READACCT"), List.of()),
            new Member("READCUST", "readcust", List.of("READCARD"), List.of()),
            new Member("READXREF", "readxref", List.of("READCUST"), List.of()),
            new Member("POSTTRAN", "posttran", List.of(), List.of("TRANSACT", "ACCTDATA", "TCATBALF")),
            new Member("INTCALC", "intcalc", List.of("POSTTRAN"), List.of("ACCTDATA", "TCATBALF")),
            new Member("TRANBKP", "tranbkp", List.of("INTCALC"), List.of("TRANSACT")),
            new Member("COMBTRAN", "combtran", List.of("TRANBKP", "INTCALC"), List.of("TRANSACT")),
            new Member("TRANREPT", "tranrept", List.of("COMBTRAN"), List.of("TRANSACT")),
            new Member("CREASTMT", "creastmt", List.of("COMBTRAN"), List.of()),
            new Member("PRTCATBL", "prtcatbl", List.of("CREASTMT"), List.of()));

    private NightlyCycle() {
    }

    public static Optional<Member> member(String name) {
        return MEMBERS.stream().filter(m -> m.name().equalsIgnoreCase(name)).findFirst();
    }
}
