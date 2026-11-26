package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.ImsFunctionCodes;
import com.carddemo.programs.dbunldgs.copybook.PcbMask;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** In-memory GSAM output dataset (PROCOPT=LS): collects ISRT records in arrival order. */
public final class InMemoryGsamDataset implements DliCallHandler {

    private final int recordLength;
    private final List<byte[]> records = new ArrayList<>();
    private final Map<Integer, String> injectedStatus = new HashMap<>();
    private int callCount;

    public InMemoryGsamDataset(int recordLength) {
        this.recordLength = recordLength;
    }

    /** Make the n-th ISRT (1-based) fail with the given status. */
    public InMemoryGsamDataset injectStatus(int callNumber, String status) {
        injectedStatus.put(callNumber, status);
        return this;
    }

    public List<byte[]> records() {
        return records.stream().map(byte[]::clone).toList();
    }

    @Override
    public void cbltdli(String function, PcbMask pcb, byte[] ioArea, String ssa) {
        callCount++;
        String injected = injectedStatus.get(callCount);
        if (injected != null) {
            pcb.setPcbStatus(injected);
            return;
        }
        if (!ImsFunctionCodes.FUNC_ISRT.equals(function)) {
            pcb.setPcbStatus("AD");
            return;
        }
        byte[] rec = new byte[recordLength];
        System.arraycopy(ioArea, 0, rec, 0, Math.min(recordLength, ioArea.length));
        records.add(rec);
        pcb.setPcbStatus("  ");
    }
}
