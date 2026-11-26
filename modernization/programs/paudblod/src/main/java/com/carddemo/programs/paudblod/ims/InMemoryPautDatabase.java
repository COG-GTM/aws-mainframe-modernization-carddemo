package com.carddemo.programs.paudblod.ims;

import com.carddemo.programs.paudblod.copybook.CobolFields;
import com.carddemo.programs.paudblod.copybook.ImsFuncs;
import com.carddemo.programs.paudblod.copybook.PautbPcb;
import com.carddemo.programs.paudblod.copybook.PendingAuthDetails;
import com.carddemo.programs.paudblod.copybook.PendingAuthSummary;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * In-memory stand-in for HIDAM database DBPAUTP0 (PSB PSBPAUTB): root PAUTSUM0 keyed by ACCNTID
 * (bytes 1-6, packed, unique) and child PAUTDTL1 keyed by PAUT9CTS (bytes 1-8, unique) under
 * its root. Supports the ISRT and GU calls issued by PAUDBLOD.
 */
public final class InMemoryPautDatabase implements DliGateway {

    public static final String STATUS_OK = "  ";
    public static final String STATUS_DUPLICATE = "II";
    public static final String STATUS_NOT_FOUND = "GE";
    public static final String STATUS_INVALID_CALL = "AD";

    private final Map<String, RootEntry> roots = new TreeMap<>();
    private final byte keyfbPad;
    private RootEntry currentParent;

    private record RootEntry(byte[] key, byte[] segment, Map<String, byte[]> children) {
    }

    /** @param keyfbPad byte used to pad the key feedback area (EBCDIC space 0x40, ASCII space 0x20). */
    public InMemoryPautDatabase(byte keyfbPad) {
        this.keyfbPad = keyfbPad;
    }

    @Override
    public void cbltdli(ImsFuncs function, PautbPcb pcb, byte[] ioArea, Ssa ssa) {
        switch (function) {
            case FUNC_ISRT -> {
                if (ssa instanceof Ssa.RootUnqualSsa) {
                    insertRoot(pcb, ioArea);
                } else if (ssa instanceof Ssa.ChildUnqualSsa) {
                    insertChild(pcb, ioArea);
                } else {
                    pcb.setPautPcbStatus(STATUS_INVALID_CALL);
                }
            }
            case FUNC_GU -> {
                if (ssa instanceof Ssa.RootQualSsa q) {
                    getUniqueRoot(pcb, ioArea, q);
                } else {
                    pcb.setPautPcbStatus(STATUS_INVALID_CALL);
                }
            }
            default -> pcb.setPautPcbStatus(STATUS_INVALID_CALL);
        }
    }

    private void insertRoot(PautbPcb pcb, byte[] ioArea) {
        byte[] key = Arrays.copyOf(ioArea, PendingAuthSummary.LEN_ACCT_ID);
        String k = rootKey(key, 0);
        RootEntry existing = roots.get(k);
        if (existing != null) {
            pcb.setPautPcbStatus(STATUS_DUPLICATE);
            return;
        }
        RootEntry e = new RootEntry(key, Arrays.copyOf(ioArea, PendingAuthSummary.LENGTH), new TreeMap<>());
        roots.put(k, e);
        currentParent = e;
        position(pcb, "01", Ssa.ROOT_SEGMENT, key);
        pcb.setPautPcbStatus(STATUS_OK);
    }

    private void getUniqueRoot(PautbPcb pcb, byte[] ioArea, Ssa.RootQualSsa q) {
        RootEntry e = roots.get(rootKey(q.qualSsaKeyValue()));
        if (e == null) {
            currentParent = null;
            pcb.setPautPcbStatus(STATUS_NOT_FOUND);
            return;
        }
        System.arraycopy(e.segment(), 0, ioArea, 0, Math.min(ioArea.length, e.segment().length));
        currentParent = e;
        position(pcb, "01", Ssa.ROOT_SEGMENT, e.key());
        pcb.setPautPcbStatus(STATUS_OK);
    }

    private void insertChild(PautbPcb pcb, byte[] ioArea) {
        if (currentParent == null) {
            pcb.setPautPcbStatus(STATUS_NOT_FOUND);
            return;
        }
        byte[] childKey = Arrays.copyOf(ioArea, PendingAuthDetails.KEY_LENGTH);
        String k = CobolFields.hex(childKey, 0, childKey.length);
        if (currentParent.children().containsKey(k)) {
            pcb.setPautPcbStatus(STATUS_DUPLICATE);
            return;
        }
        currentParent.children().put(k, Arrays.copyOf(ioArea, PendingAuthDetails.LENGTH));
        byte[] concatenated = new byte[currentParent.key().length + childKey.length];
        System.arraycopy(currentParent.key(), 0, concatenated, 0, currentParent.key().length);
        System.arraycopy(childKey, 0, concatenated, currentParent.key().length, childKey.length);
        position(pcb, "02", Ssa.CHILD_SEGMENT, concatenated);
        pcb.setPautPcbStatus(STATUS_OK);
    }

    private void position(PautbPcb pcb, String level, String segName, byte[] keyfb) {
        pcb.setPautSegLevel(level);
        pcb.setPautSegName(segName);
        pcb.setPautKeyfb(keyfb, keyfbPad);
    }

    private static String rootKey(byte[] key, int off) {
        if (CobolFields.isPackedNumeric(key, off, PendingAuthSummary.LEN_ACCT_ID)) {
            return rootKey(CobolFields.decodePacked(key, off, PendingAuthSummary.LEN_ACCT_ID, 0));
        }
        return "X'" + CobolFields.hex(key, off, PendingAuthSummary.LEN_ACCT_ID) + "'";
    }

    private static String rootKey(BigDecimal value) {
        return String.format("%+012d", value.toBigIntegerExact());
    }

    /** Preloads a root segment (bypassing DL/I), e.g. to exercise the 'II' path. */
    public void preloadRoot(byte[] segment) {
        byte[] key = Arrays.copyOf(segment, PendingAuthSummary.LEN_ACCT_ID);
        roots.put(rootKey(key, 0), new RootEntry(key, segment.clone(), new TreeMap<>()));
    }

    public int rootCount() {
        return roots.size();
    }

    public int childCount() {
        return roots.values().stream().mapToInt(r -> r.children().size()).sum();
    }

    /** Root segments in ACCNTID order. */
    public List<byte[]> rootSegments() {
        return roots.values().stream().map(r -> r.segment().clone()).toList();
    }

    /** Child segments per root key (packed-key hex), each in PAUT9CTS order. */
    public Map<String, List<byte[]>> childSegmentsByRoot() {
        Map<String, List<byte[]>> out = new LinkedHashMap<>();
        for (RootEntry r : roots.values()) {
            List<byte[]> kids = new ArrayList<>();
            r.children().values().forEach(c -> kids.add(c.clone()));
            out.put(CobolFields.hex(r.key(), 0, r.key().length), Collections.unmodifiableList(kids));
        }
        return out;
    }
}
