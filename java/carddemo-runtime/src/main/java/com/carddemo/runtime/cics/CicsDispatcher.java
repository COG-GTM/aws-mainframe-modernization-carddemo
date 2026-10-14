package com.carddemo.runtime.cics;

import com.carddemo.runtime.vsam.TaskFileState;

/** Runs a {@link CicsProgram} task and converts its termination into a {@link CicsResponse}. */
public final class CicsDispatcher {

    private CicsDispatcher() {
    }

    public static CicsResponse run(CicsProgram program, CicsContext ctx) {
        CicsOutcome outcome;
        try {
            program.execute(ctx);
            outcome = CicsOutcome.returnToCics();
        } catch (CicsControlTransfer transfer) {
            outcome = transfer.outcome();
            if (outcome.type() == CicsOutcome.Type.ABEND) {
                ctx.syncpointRollback();
            }
        } catch (RuntimeException | Error failure) {
            // Unexpected failure: back out the unit of work like a CICS abend, then propagate the original error.
            try {
                ctx.syncpointRollback();
            } catch (RuntimeException rollbackFailure) {
                failure.addSuppressed(rollbackFailure);
            }
            throw failure;
        } finally {
            ctx.releaseRecordLocks();
            TaskFileState.endTask();
        }
        return new CicsResponse(program.programName(), outcome, ctx.terminalOutputs(), ctx.eib());
    }
}
