package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.CobolCodec;
import com.carddemo.programs.dbunldgs.copybook.ImsFunctionCodes;
import com.carddemo.programs.dbunldgs.copybook.PcbMask;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthDetails;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthSummary;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * In-memory stand-in for the HIDAM database DBPAUTP0 (root PAUTSUM0, child PAUTDTL1) supporting the
 * GN / GNP calls DBUNLDGS issues. Segments are stored as raw I/O-area bytes in hierarchic sequence.
 */
public final class InMemoryPautDatabase implements DliCallHandler {

    /** One database record: a root segment and its children, all as raw bytes. */
    public record DbRecord(byte[] summary, List<byte[]> details) {
    }

    private final List<DbRecord> records = new ArrayList<>();
    private final Map<Integer, String> injectedStatus = new HashMap<>();
    private final Charset charset;
    private int rootPos = -1;
    private int childPos = -1;
    private int callCount;

    public InMemoryPautDatabase(Charset charset) {
        this.charset = charset;
    }

    public InMemoryPautDatabase() {
        this(CobolCodec.EBCDIC);
    }

    public InMemoryPautDatabase addRecord(byte[] summary, List<byte[]> details) {
        if (summary.length != PendingAuthSummary.LENGTH) {
            throw new IllegalArgumentException("PAUTSUM0 must be 100 bytes");
        }
        for (byte[] d : details) {
            if (d.length != PendingAuthDetails.LENGTH) {
                throw new IllegalArgumentException("PAUTDTL1 must be 200 bytes");
            }
        }
        records.add(new DbRecord(summary.clone(), details.stream().map(byte[]::clone).toList()));
        return this;
    }

    public List<DbRecord> records() {
        return List.copyOf(records);
    }

    /** Make the n-th CBLTDLI call (1-based) against this PCB return the given status instead. */
    public InMemoryPautDatabase injectStatus(int callNumber, String status) {
        injectedStatus.put(callNumber, status);
        return this;
    }

    public int callCount() {
        return callCount;
    }

    @Override
    public void cbltdli(String function, PcbMask pcb, byte[] ioArea, String ssa) {
        callCount++;
        String injected = injectedStatus.get(callCount);
        if (injected != null) {
            pcb.setPcbStatus(injected);
            return;
        }
        String segment = ssa == null ? "" : ssa.substring(0, Math.min(8, ssa.length()));
        if (ImsFunctionCodes.FUNC_GN.equals(function) && "PAUTSUM0".equals(segment)) {
            getNextRoot(pcb, ioArea);
        } else if (ImsFunctionCodes.FUNC_GNP.equals(function) && "PAUTDTL1".equals(segment)) {
            getNextChild(pcb, ioArea);
        } else {
            pcb.setPcbStatus("AD");
        }
    }

    private void getNextRoot(PcbMask pcb, byte[] ioArea) {
        if (rootPos + 1 >= records.size()) {
            rootPos = records.size();
            childPos = -1;
            pcb.setPcbStatus("GB");
            return;
        }
        rootPos++;
        childPos = -1;
        byte[] root = records.get(rootPos).summary();
        System.arraycopy(root, 0, ioArea, 0, root.length);
        pcb.setPcbStatus("  ");
        pcb.setSegLevel("01");
        pcb.setSegName("PAUTSUM0");
        pcb.setKeyFb(rootKey(root), charset);
    }

    private void getNextChild(PcbMask pcb, byte[] ioArea) {
        if (rootPos < 0 || rootPos >= records.size()) {
            pcb.setPcbStatus("GP");
            return;
        }
        DbRecord rec = records.get(rootPos);
        if (childPos + 1 >= rec.details().size()) {
            childPos = rec.details().size();
            pcb.setPcbStatus("GE");
            return;
        }
        childPos++;
        byte[] child = rec.details().get(childPos);
        System.arraycopy(child, 0, ioArea, 0, child.length);
        pcb.setPcbStatus("  ");
        pcb.setSegLevel("02");
        pcb.setSegName("PAUTDTL1");
        byte[] key = new byte[PendingAuthSummary.ACCT_ID_LENGTH + PendingAuthDetails.KEY_LENGTH];
        System.arraycopy(rootKey(rec.summary()), 0, key, 0, PendingAuthSummary.ACCT_ID_LENGTH);
        System.arraycopy(child, 0, key, PendingAuthSummary.ACCT_ID_LENGTH, PendingAuthDetails.KEY_LENGTH);
        pcb.setKeyFb(key, charset);
    }

    private static byte[] rootKey(byte[] root) {
        byte[] key = new byte[PendingAuthSummary.ACCT_ID_LENGTH];
        System.arraycopy(root, PendingAuthSummary.ACCT_ID_OFFSET, key, 0, key.length);
        return key;
    }
}
