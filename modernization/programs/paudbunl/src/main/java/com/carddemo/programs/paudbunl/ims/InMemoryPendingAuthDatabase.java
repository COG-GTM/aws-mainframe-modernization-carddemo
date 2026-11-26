package com.carddemo.programs.paudbunl.ims;

import com.carddemo.programs.paudbunl.copybook.ImsFunctionCodes;
import com.carddemo.programs.paudbunl.copybook.PautbPcb;
import com.carddemo.programs.paudbunl.copybook.PendingAuthDetails;
import com.carddemo.programs.paudbunl.copybook.PendingAuthSummary;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

/**
 * In-memory stand-in for the HIDAM database DBPAUTP0 as seen through PSB PAUTBUNL
 * (PAUTSUM0 root, PAUTDTL1 child). Supports the GN / GNP calls with unqualified SSAs that
 * PAUDBUNL issues. Roots are returned in insertion order, which callers must supply in
 * hierarchical (key) sequence, as an HD unload file already is.
 */
public final class InMemoryPendingAuthDatabase implements DliGateway {

    public static final String ROOT_SEGMENT = "PAUTSUM0";
    public static final String CHILD_SEGMENT = "PAUTDTL1";

    private record Root(byte[] summary, List<byte[]> details) {
    }

    /** IMS keeps database position per PCB, so each PCB starts before the first root. */
    private static final class Position {
        int rootPos = -1;
        int childPos = -1;
    }

    private final List<Root> roots = new ArrayList<>();
    private final Map<PautbPcb, Position> positions = new IdentityHashMap<>();

    /** Adds a root segment (100 bytes); subsequent {@link #addChild} calls attach to it. */
    public InMemoryPendingAuthDatabase addRoot(byte[] summary) {
        requireLength(summary, PendingAuthSummary.LENGTH, ROOT_SEGMENT);
        roots.add(new Root(summary.clone(), new ArrayList<>()));
        return this;
    }

    /** Adds a child segment (200 bytes) under the most recently added root. */
    public InMemoryPendingAuthDatabase addChild(byte[] details) {
        requireLength(details, PendingAuthDetails.LENGTH, CHILD_SEGMENT);
        if (roots.isEmpty()) {
            throw new IllegalStateException("PAUTDTL1 segment has no PAUTSUM0 parent");
        }
        roots.get(roots.size() - 1).details().add(details.clone());
        return this;
    }

    public int rootCount() {
        return roots.size();
    }

    public int childCount() {
        return roots.stream().mapToInt(r -> r.details().size()).sum();
    }

    @Override
    public void cbltdli(ImsFunctionCodes function, PautbPcb pcb, byte[] ioArea, Ssa ssa) {
        switch (function) {
            case GN -> getNext(pcb, ioArea, ssa);
            case GNP -> getNextWithinParent(pcb, ioArea, ssa);
            default -> pcb.pcbStatus(ImsStatus.INVALID_CALL);
        }
    }

    private void getNext(PautbPcb pcb, byte[] ioArea, Ssa ssa) {
        Position p = positions.computeIfAbsent(pcb, k -> new Position());
        if (!ROOT_SEGMENT.equals(ssa.segmentName())) {
            pcb.pcbStatus(ImsStatus.INVALID_CALL);
            return;
        }
        if (p.rootPos + 1 >= roots.size()) {
            p.rootPos = roots.size();
            p.childPos = -1;
            pcb.pcbStatus(ImsStatus.END_OF_DB);
            return;
        }
        p.rootPos++;
        p.childPos = -1;
        byte[] seg = roots.get(p.rootPos).summary();
        System.arraycopy(seg, 0, ioArea, 0, seg.length);
        pcb.pcbStatus(ImsStatus.OK);
        pcb.segLevel("01");
        pcb.segName(ROOT_SEGMENT);
        pcb.keyFeedback(rootKey(seg));
    }

    private void getNextWithinParent(PautbPcb pcb, byte[] ioArea, Ssa ssa) {
        Position p = positions.computeIfAbsent(pcb, k -> new Position());
        if (!CHILD_SEGMENT.equals(ssa.segmentName())) {
            pcb.pcbStatus(ImsStatus.INVALID_CALL);
            return;
        }
        if (p.rootPos < 0 || p.rootPos >= roots.size()) {
            pcb.pcbStatus(ImsStatus.WRONG_PARENTAGE);
            return;
        }
        Root root = roots.get(p.rootPos);
        if (p.childPos + 1 >= root.details().size()) {
            p.childPos = root.details().size();
            pcb.pcbStatus(ImsStatus.SEGMENT_NOT_FOUND);
            return;
        }
        p.childPos++;
        byte[] seg = root.details().get(p.childPos);
        System.arraycopy(seg, 0, ioArea, 0, seg.length);
        pcb.pcbStatus(ImsStatus.OK);
        pcb.segLevel("02");
        pcb.segName(CHILD_SEGMENT);
        byte[] key = new byte[PendingAuthSummary.ACCT_ID_LENGTH + PendingAuthDetails.AUTHORIZATION_KEY_LENGTH];
        System.arraycopy(rootKey(root.summary()), 0, key, 0, PendingAuthSummary.ACCT_ID_LENGTH);
        System.arraycopy(seg, 0, key, PendingAuthSummary.ACCT_ID_LENGTH, PendingAuthDetails.AUTHORIZATION_KEY_LENGTH);
        pcb.keyFeedback(key);
    }

    /** ACCNTID: START=1, BYTES=6, TYPE=P. */
    private static byte[] rootKey(byte[] summary) {
        return Arrays.copyOfRange(summary, PendingAuthSummary.ACCT_ID_OFFSET,
                PendingAuthSummary.ACCT_ID_OFFSET + PendingAuthSummary.ACCT_ID_LENGTH);
    }

    private static void requireLength(byte[] seg, int len, String name) {
        if (seg.length != len) {
            throw new IllegalArgumentException(name + " segment must be " + len + " bytes, got " + seg.length);
        }
    }
}
