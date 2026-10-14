package com.carddemo.runtime.cics;

import com.carddemo.runtime.vsam.TaskFileState;
import com.carddemo.runtime.vsam.UnitOfWork;

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
                UnitOfWork.rollback();
            }
        } finally {
            TaskFileState.endTask();
        }
        return new CicsResponse(program.programName(), outcome, ctx.terminalOutputs(), ctx.eib());
    }
}
