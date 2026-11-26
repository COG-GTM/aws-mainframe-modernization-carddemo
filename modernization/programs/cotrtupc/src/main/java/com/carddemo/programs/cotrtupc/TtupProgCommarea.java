package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.support.CobolText;
import com.carddemo.programs.cotrtupc.support.FixedRecord;

/** WS-THIS-PROGCOMMAREA (105 bytes) - program-private state appended to CARDDEMO-COMMAREA. */
public final class TtupProgCommarea extends FixedRecord {

    public static final int LENGTH = 105;

    static final int CHANGE_ACTION = 0;   // TTUP-CHANGE-ACTION X(1)
    static final int OLD_TYPE = 1;        // TTUP-OLD-TTYP-TYPE X(02)
    static final int OLD_DESC = 3;        // TTUP-OLD-TTYP-TYPE-DESC X(50)
    static final int NEW_TYPE = 53;       // TTUP-NEW-TTYP-TYPE X(02)
    static final int NEW_DESC = 55;       // TTUP-NEW-TTYP-TYPE-DESC X(50)

    public TtupProgCommarea() {
        super(LENGTH);
    }

    /** INITIALIZE WS-THIS-PROGCOMMAREA. */
    public void initialize() {
        fill(0, LENGTH, ' ');
    }

    public void initializeOldDetails() { fill(OLD_TYPE, 52, ' '); }
    public void initializeNewDetails() { fill(NEW_TYPE, 52, ' '); }

    public char changeAction() { return data[CHANGE_ACTION]; }
    public void changeAction(char c) { data[CHANGE_ACTION] = c; }

    public String oldType() { return get(OLD_TYPE, 2); }
    public void oldType(String v) { set(OLD_TYPE, 2, v); }
    public String oldDesc() { return get(OLD_DESC, 50); }
    public void oldDesc(String v) { set(OLD_DESC, 50, v); }
    public String oldDetails() { return get(OLD_TYPE, 52); }
    public String newType() { return get(NEW_TYPE, 2); }
    public void newType(String v) { set(NEW_TYPE, 2, v); }
    public void newTypeLowValues() { fill(NEW_TYPE, 2, CobolText.LOW_VALUE); }
    public String newDesc() { return get(NEW_DESC, 50); }
    public void newDesc(String v) { set(NEW_DESC, 50, v); }
    public void newDescLowValues() { fill(NEW_DESC, 50, CobolText.LOW_VALUE); }

    // ---- TTUP-CHANGE-ACTION 88-levels ----
    private boolean in(char... values) {
        char c = data[CHANGE_ACTION];
        for (char v : values) {
            if (c == v) {
                return true;
            }
        }
        return false;
    }

    public boolean isDetailsNotFetched() { return in(CobolText.LOW_VALUE, ' '); }
    public boolean isInvalidSearchKeys() { return in('K'); }
    public boolean isDetailsNotFound() { return in('X'); }
    public boolean isShowDetails() { return in('S'); }
    public boolean isCreateNewRecord() { return in('R'); }
    public boolean isReviewNewRecord() { return in('V'); }
    public boolean isDeleteInProgress() { return in('9', '8', '7', '6'); }
    public boolean isConfirmDelete() { return in('9'); }
    public boolean isStartDelete() { return in('8'); }
    public boolean isDeleteDone() { return in('7'); }
    public boolean isDeleteFailed() { return in('6'); }
    public boolean isChangesMade() { return in('E', 'N', 'L', 'F'); }
    public boolean isChangesNotOk() { return in('E'); }
    public boolean isChangesOkNotConfirmed() { return in('N'); }
    public boolean isChangesFailed() { return in('L', 'F'); }
    public boolean isChangesOkayedLockError() { return in('L'); }
    public boolean isChangesOkayedButFailed() { return in('F'); }
    public boolean isChangesOkayedAndDone() { return in('C'); }
    public boolean isChangesBackedOut() { return in('B'); }

    /** SET ... TO TRUE uses the first VALUE of the 88-level. */
    public void setDetailsNotFetched() { changeAction(CobolText.LOW_VALUE); }
    public void setInvalidSearchKeys() { changeAction('K'); }
    public void setDetailsNotFound() { changeAction('X'); }
    public void setShowDetails() { changeAction('S'); }
    public void setCreateNewRecord() { changeAction('R'); }
    public void setConfirmDelete() { changeAction('9'); }
    public void setStartDelete() { changeAction('8'); }
    public void setDeleteDone() { changeAction('7'); }
    public void setDeleteFailed() { changeAction('6'); }
    public void setChangesNotOk() { changeAction('E'); }
    public void setChangesOkNotConfirmed() { changeAction('N'); }
    public void setChangesOkayedLockError() { changeAction('L'); }
    public void setChangesOkayedButFailed() { changeAction('F'); }
    public void setChangesOkayedAndDone() { changeAction('C'); }
    public void setChangesBackedOut() { changeAction('B'); }
}
