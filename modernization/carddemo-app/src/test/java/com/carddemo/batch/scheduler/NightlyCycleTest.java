package com.carddemo.batch.scheduler;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.harness.JclCond;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.scheduler.NightlyCycle.Member;
import com.carddemo.common.codec.TestData;
import java.io.IOException;
import java.nio.file.Files;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;

/** The cycle definition: baseline run order, scheduler predecessors as {@code COND}, member parameter routing. */
class NightlyCycleTest {

    @Test
    void membersRunInTheBaselineOrder() throws IOException {
        String chain = Files.readAllLines(TestData.resolve("docs/validation/baseline/00-ORDER.md")).stream()
                .filter(l -> l.contains("ACCTFILE") && l.contains("->")).findFirst().orElseThrow();
        List<String> baseline = Arrays.stream(chain.replace("`", "").split("->")).map(String::strip).toList();
        List<String> names = NightlyCycle.MEMBERS.stream().map(Member::name).toList();
        assertThat(baseline).containsSubsequence(names);
        assertThat(names).containsExactly("READACCT", "READCARD", "READCUST", "READXREF", "POSTTRAN", "INTCALC",
                "TRANBKP", "COMBTRAN", "TRANREPT", "CREASTMT", "PRTCATBL");
    }

    @Test
    void predecessorsAreEarlierMembers() {
        List<String> seen = new ArrayList<>();
        for (Member member : NightlyCycle.MEMBERS) {
            assertThat(seen).as(member.name()).containsAll(member.predecessors());
            seen.add(member.name());
        }
    }

    @Test
    void condIsTheSchedulerEndedOkCondition() {
        Member combtran = NightlyCycle.member("combtran").orElseThrow();
        assertThat(combtran.cond()).isEqualTo("((4,LT,TRANBKP),(4,LT,INTCALC))");
        assertThat(NightlyCycle.member("POSTTRAN").orElseThrow().cond()).isEmpty();
        JclCond cond = JclCond.parse(combtran.cond());
        assertThat(cond.bypass(Map.of("TRANBKP", ReturnCode.WARNING, "INTCALC", ReturnCode.OK), false)).isFalse();
        assertThat(cond.bypass(Map.of("TRANBKP", ReturnCode.OK, "INTCALC", ReturnCode.ERROR), false)).isTrue();
        assertThat(cond.bypass(Map.of("TRANBKP", ReturnCode.OK, "INTCALC", ReturnCode.OK), true)).isTrue();
        NightlyCycle.MEMBERS.stream().filter(m -> !m.predecessors().isEmpty())
                .forEach(m -> assertThat(JclCond.parse(m.cond())).as(m.name()).isNotNull());
    }

    @Test
    void memberQualifiedParametersOnlyReachThatMember() {
        JobParameters cycle = new JobParametersBuilder()
                .addLocalDate("run-date", LocalDate.of(2022, 7, 6))
                .addString("encoding", "ASCII")
                .addString("POSTTRAN.STEP15.SYSOUT", "/tmp/posttran.txt")
                .addString("READACCT.SYSOUT", "/tmp/readacct.txt")
                .addString("STEP10.SYSOUT", "/tmp/any-step10.txt")
                .addString("AFTER-IMAGES", "/tmp/after")
                .addString("AFTER-IMAGES.TRANSACT", "/tmp/TRANSACT.ksds")
                .toJobParameters();
        JobParameters posttran = NightlyCycleJobConfiguration.memberParameters(cycle,
                NightlyCycle.member("POSTTRAN").orElseThrow(), 7L);
        assertThat(posttran.getParameters().keySet()).containsExactlyInAnyOrder("run-date", "encoding",
                "STEP15.SYSOUT", "STEP10.SYSOUT", NightlyCycleJobConfiguration.MEMBER,
                NightlyCycleJobConfiguration.CYCLE_EXECUTION);
        assertThat(posttran.getString("STEP15.SYSOUT")).isEqualTo("/tmp/posttran.txt");
        assertThat(posttran.getString(NightlyCycleJobConfiguration.MEMBER)).isEqualTo("POSTTRAN");
        assertThat(posttran.getParameters().get(NightlyCycleJobConfiguration.CYCLE_EXECUTION).isIdentifying())
                .isFalse();
        JobParameters readacct = NightlyCycleJobConfiguration.memberParameters(cycle,
                NightlyCycle.member("READACCT").orElseThrow(), null);
        assertThat(readacct.getString("SYSOUT")).isEqualTo("/tmp/readacct.txt");
        assertThat(readacct.getParameters()).doesNotContainKey("STEP15.SYSOUT")
                .doesNotContainKey(NightlyCycleJobConfiguration.CYCLE_EXECUTION);
    }
}
