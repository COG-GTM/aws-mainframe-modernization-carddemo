package com.carddemo.runtime.cics;

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
        }
        return new CicsResponse(program.programName(), outcome, ctx.terminalOutputs(), ctx.eib());
    }
}
