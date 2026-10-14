package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.RecordLayout;

/** CSMSG02Y {@code ABEND-DATA} (134 bytes). Every item has {@code VALUE SPACES}, so a new instance is spaces. */
public final class AbendData extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("ABEND-DATA");

    /** ABEND-CODE PIC X(4). */
    public static final AlphanumericField ABEND_CODE = B.x("ABEND-CODE", 4);
    /** ABEND-CULPRIT PIC X(8). */
    public static final AlphanumericField ABEND_CULPRIT = B.x("ABEND-CULPRIT", 8);
    /** ABEND-REASON PIC X(50). */
    public static final AlphanumericField ABEND_REASON = B.x("ABEND-REASON", 50);
    /** ABEND-MSG PIC X(72). */
    public static final AlphanumericField ABEND_MSG = B.x("ABEND-MSG", 72);

    public static final RecordLayout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    /** New ABEND-DATA with its VALUE SPACES clauses applied. */
    public AbendData() {
        super(LENGTH);
        initializeSpaces();
    }
}
