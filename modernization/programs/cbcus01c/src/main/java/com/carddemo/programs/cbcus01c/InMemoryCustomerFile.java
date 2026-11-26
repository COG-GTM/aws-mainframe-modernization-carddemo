package com.carddemo.programs.cbcus01c;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * In-memory CUSTFILE for tests and embedding. Records are returned in FD-CUST-ID order. Status
 * codes for OPEN, CLOSE and the n-th READ can be forced to exercise the error paths.
 */
public final class InMemoryCustomerFile implements CustomerFile {

    private final List<FdCustfileRec> records;
    private String openStatus = FileStatus.SUCCESS;
    private String closeStatus = FileStatus.SUCCESS;
    private int failReadAt = -1;
    private String failReadStatus;
    private int reads;
    private boolean open;

    public InMemoryCustomerFile(List<String> recordImages) {
        this.records = new ArrayList<>();
        for (String image : recordImages) {
            records.add(FdCustfileRec.fromFixedWidth(image));
        }
        records.sort(Comparator.comparing(FdCustfileRec::fdCustId));
    }

    public InMemoryCustomerFile withOpenStatus(String status) {
        this.openStatus = status;
        return this;
    }

    public InMemoryCustomerFile withCloseStatus(String status) {
        this.closeStatus = status;
        return this;
    }

    /** Forces the given status on the 1-based {@code readNumber}-th READ. */
    public InMemoryCustomerFile withReadStatus(int readNumber, String status) {
        this.failReadAt = readNumber;
        this.failReadStatus = status;
        return this;
    }

    public boolean isOpen() {
        return open;
    }

    @Override
    public String open() {
        if (open) {
            return "41";
        }
        if (FileStatus.SUCCESS.equals(openStatus)) {
            open = true;
            reads = 0;
        }
        return openStatus;
    }

    @Override
    public ReadResult readNext() {
        if (!open) {
            return ReadResult.status(FileStatus.LOGIC_ERROR_READ_NOT_OPEN);
        }
        reads++;
        if (reads == failReadAt) {
            return ReadResult.status(failReadStatus);
        }
        int index = reads - 1 - (failReadAt > 0 && reads > failReadAt ? 1 : 0);
        return index < records.size() ? ReadResult.ok(records.get(index)) : ReadResult.status(FileStatus.END_OF_FILE);
    }

    @Override
    public String close() {
        if (!open) {
            return FileStatus.NOT_OPEN;
        }
        if (FileStatus.SUCCESS.equals(closeStatus)) {
            open = false;
        }
        return closeStatus;
    }
}
