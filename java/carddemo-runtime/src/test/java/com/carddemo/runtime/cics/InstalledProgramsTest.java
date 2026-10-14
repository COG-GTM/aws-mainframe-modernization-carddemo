package com.carddemo.runtime.cics;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import org.junit.jupiter.api.Test;

class InstalledProgramsTest {

    @Test
    void parsesDefineProgramFromCsd() {
        InstalledPrograms p = InstalledPrograms.fromCsd("""
                DEFINE PROGRAM(COMEN01C) GROUP(CARDDEMO)
                DEFINE TRANSACTION(CM00) GROUP(CARDDEMO)
                       PROGRAM(COMEN01C)
                DEFINE PROGRAM( COSGN00C ) GROUP(CARDDEMO)
                """);
        assertThat(p.names()).containsExactlyInAnyOrder("COMEN01C", "COSGN00C");
        assertThat(p.contains("comen01c ")).isTrue();
        assertThat(p.contains("CM00")).isFalse();
        assertThat(p.contains(null)).isFalse();
    }

    @Test
    void carddemoCsdHasBaseProgramsButNotExtensions() {
        InstalledPrograms p = InstalledPrograms.carddemoCsd();
        assertThat(p.contains("COSGN00C")).isTrue();
        assertThat(p.contains("COMEN01C")).isTrue();
        assertThat(p.contains("COADM01C")).isTrue();
        // COPAUS0C is defined only in app/app-authorization-ims-db2-mq/csd/CRDDEMO2.csd
        assertThat(p.contains("COPAUS0C")).isFalse();
        assertThat(p.with(List.of("COPAUS0C", " ")).contains("COPAUS0C")).isTrue();
    }

    @Test
    void inquireProgramSetsEibresp() {
        CicsContext none = CicsContext.builder().build();
        assertThat(none.inquireProgram("COSGN00C")).isEqualTo(CicsResp.PGMIDERR);
        assertThat(none.eib().eibresp()).isEqualTo(CicsResp.PGMIDERR.code());

        CicsContext csd = CicsContext.builder().installedPrograms(InstalledPrograms.carddemoCsd()).build();
        assertThat(csd.inquireProgram("COSGN00C")).isEqualTo(CicsResp.NORMAL);
        assertThat(csd.eib().eibresp()).isEqualTo(CicsResp.NORMAL.code());
        assertThat(csd.inquireProgram("COPAUS0C")).isEqualTo(CicsResp.PGMIDERR);
    }
}
