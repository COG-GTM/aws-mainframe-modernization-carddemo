package com.carddemo.runtime.vsam;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.function.Supplier;

/**
 * Per-task file-control state (READ UPDATE holds, browse cursors). File objects are shared by every task of a
 * region, but CICS scopes these to the task and releases them when it ends; {@link #endTask()} does that.
 */
public final class TaskFileState {

    private static final ThreadLocal<Map<Object, Object>> STATE = ThreadLocal.withInitial(IdentityHashMap::new);

    private TaskFileState() {
    }

    @SuppressWarnings("unchecked")
    static <T> T of(Object owner, Supplier<T> init) {
        return (T) STATE.get().computeIfAbsent(owner, k -> init.get());
    }

    /** Releases all holds and browses of the current task. */
    public static void endTask() {
        STATE.remove();
    }
}
