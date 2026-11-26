package com.carddemo.programs.coadm01c;

import com.carddemo.programs.coadm01c.copybook.CardDemoCommarea;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/** In-memory program table: XCTL succeeds only for installed programs and is recorded. */
public final class InMemoryProgramControlGateway implements ProgramControlGateway {

    public record Transfer(String program, CardDemoCommarea commarea) {
    }

    private final Set<String> installed;
    private final List<Transfer> transfers = new ArrayList<>();

    public InMemoryProgramControlGateway(Set<String> installed) {
        this.installed = Set.copyOf(installed);
    }

    /** Every program CardDemo's admin menu and sign-on can transfer to. */
    public static InMemoryProgramControlGateway allCardDemoPrograms() {
        return new InMemoryProgramControlGateway(Set.of(
                "COSGN00C", "COUSR00C", "COUSR01C", "COUSR02C", "COUSR03C", "COTRTLIC", "COTRTUPC"));
    }

    @Override
    public void xctl(String program, CardDemoCommarea commarea) throws PgmIdErrException {
        if (!installed.contains(program)) {
            throw new PgmIdErrException(program);
        }
        transfers.add(new Transfer(program, commarea == null ? null : commarea.copy()));
    }

    public List<Transfer> transfers() {
        return List.copyOf(transfers);
    }
}
