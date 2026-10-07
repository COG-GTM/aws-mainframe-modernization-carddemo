package com.carddemo.common.online;

import java.util.Optional;
import org.springframework.stereotype.Component;

/** The programs of {@link OnlineProgram}, i.e. exactly what the CSD of this estate installs. */
@Component
public class CsdInstalledPrograms implements InstalledPrograms {

    @Override
    public Optional<String> tranIdOf(String programId) {
        return OnlineProgram.find(programId).map(OnlineProgram::tranId);
    }
}
