package com.carddemo.runtime.vsam;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.WeakHashMap;

/**
 * The task's CICS unit of work over file control: before-images of updates to recoverable files
 * ({@link KsdsFile#setRecoverable}) and the files holding READ UPDATE locks. SYNCPOINT commits (drops the
 * before-images), SYNCPOINT ROLLBACK restores them; both release the locks. Updates to non-recoverable files
 * (CSD {@code RECOVERY(NONE)}) are never backed out, as in CICS. Files are held weakly, like {@link TaskFileState}.
 */
public final class UnitOfWork {

    private static final class State {
        private final List<Runnable> undo = new ArrayList<>();
        private final Set<KsdsFile> files = Collections.newSetFromMap(new WeakHashMap<>());
    }

    private UnitOfWork() {
    }

    private static State state() {
        return TaskFileState.of(UnitOfWork.class, State::new);
    }

    static void touched(KsdsFile file) {
        state().files.add(file);
    }

    static void recordUndo(KsdsFile file, Runnable undo) {
        state().files.add(file);
        state().undo.add(undo);
    }

    /** EXEC CICS SYNCPOINT. */
    public static void commit() {
        State s = state();
        s.undo.clear();
        new ArrayList<>(s.files).forEach(KsdsFile::unlock);
        s.files.clear();
    }

    /** EXEC CICS SYNCPOINT ROLLBACK (also what CICS does when a task abends). */
    public static void rollback() {
        State s = state();
        for (int i = s.undo.size() - 1; i >= 0; i--) {
            s.undo.get(i).run();
        }
        s.undo.clear();
        new ArrayList<>(s.files).forEach(KsdsFile::unlock);
        s.files.clear();
    }
}
