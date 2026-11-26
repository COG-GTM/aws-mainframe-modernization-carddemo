package com.carddemo.programs.comen01c.gateway;

import java.util.HashSet;
import java.util.Set;

/** CICS program definitions held in memory. */
public final class InMemoryProgramInquiryGateway implements ProgramInquiryGateway {

    private final Set<String> installed = new HashSet<>();

    public InMemoryProgramInquiryGateway(String... programs) {
        for (String p : programs) {
            installed.add(p.strip());
        }
    }

    @Override
    public boolean isInstalled(String programName) {
        return installed.contains(programName.strip());
    }
}
