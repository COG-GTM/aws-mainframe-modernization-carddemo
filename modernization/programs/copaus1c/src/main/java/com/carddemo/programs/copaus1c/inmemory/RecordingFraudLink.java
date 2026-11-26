package com.carddemo.programs.copaus1c.inmemory;

import com.carddemo.programs.copaus1c.FraudCommarea;
import com.carddemo.programs.copaus1c.gateway.FraudLinkGateway;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

/** In-memory LINK target: records each call and delegates to a scripted COPAUS2C behaviour. */
public final class RecordingFraudLink implements FraudLinkGateway {

    /** A linked-program call as received (snapshot of the COMMAREA before the program ran). */
    public record Call(String program, char action, String acctId, long custId, char fraudStatus) {
    }

    private final Predicate<FraudCommarea> program;
    private final List<Call> calls = new ArrayList<>();

    /** @param program mutates the COMMAREA like the linked program would; returns false for a non-NORMAL EIBRESP */
    public RecordingFraudLink(Predicate<FraudCommarea> program) {
        this.program = program;
    }

    /** COPAUS2C stand-in that reports success and stamps PA-FRAUD-RPT-DATE with {@code rptDate}. */
    public static RecordingFraudLink succeeding(String rptDate) {
        return new RecordingFraudLink(c -> {
            c.getFraudAuthRecord().setFraudRptDate(rptDate);
            c.setUpdateSuccess();
            c.setActMsg("UPDATED");
            return true;
        });
    }

    @Override
    public boolean link(String programName, FraudCommarea commarea) {
        calls.add(new Call(programName, commarea.getAction(), commarea.getAcctId(), commarea.getCustId(),
                commarea.getFraudAuthRecord().getAuthFraud()));
        return program.test(commarea);
    }

    public List<Call> calls() {
        return List.copyOf(calls);
    }
}
