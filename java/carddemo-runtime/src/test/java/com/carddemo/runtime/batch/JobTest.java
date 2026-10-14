package com.carddemo.runtime.batch;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;

class JobTest {

    private static JobStep step(String name, int rc) {
        return new JobStep() {
            @Override
            public String stepName() {
                return name;
            }

            @Override
            public int run(DdBindings dd) {
                if (rc < 0) {
                    throw new Abend(-rc, "forced");
                }
                return rc;
            }

            @Override
            public boolean bypass(int highestPriorReturnCode) {
                return name.equals("COND") && highestPriorReturnCode >= 4;
            }
        };
    }

    @Test
    void returnCodesCondAndAbend() {
        List<StepResult> ok = new Job("J1", List.of(step("S1", 0), step("S2", 4), step("COND", 0))).run(new DdBindings());
        assertThat(ok).extracting(StepResult::status).containsExactly(StepResult.Status.COMPLETED,
                StepResult.Status.COMPLETED, StepResult.Status.BYPASSED);
        assertThat(Job.maxcc(ok)).isEqualTo(4);

        List<StepResult> ab = new Job("J2", List.of(step("S1", -999), step("S2", 0))).run(new DdBindings());
        assertThat(ab.get(0).status()).isEqualTo(StepResult.Status.ABENDED);
        assertThat(ab.get(0).code()).isEqualTo(999);
        assertThat(ab.get(1).status()).isEqualTo(StepResult.Status.BYPASSED);
        assertThat(Job.maxcc(ab)).isEqualTo(-1);
        assertThat(new Abend(999, "x").getMessage()).startsWith("ABEND U0999");
    }

    @Test
    void ddBindingsAndFileStatus() {
        DdBindings dd = new DdBindings().bind("acctfile", Path.of("/tmp/a"));
        assertThat(dd.path("ACCTFILE")).isEqualTo(Path.of("/tmp/a"));
        assertThat(dd.find("OTHER")).isEmpty();
        assertThatThrownBy(() -> dd.path("OTHER")).isInstanceOf(IllegalStateException.class);
        assertThat(FileStatus.of("23")).isEqualTo(FileStatus.RECORD_NOT_FOUND);
        assertThat(FileStatus.SUCCESS.successful()).isTrue();
        assertThat(FileStatus.END_OF_FILE.successful()).isFalse();
    }
}
