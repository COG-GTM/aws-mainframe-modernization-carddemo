package com.carddemo.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.api.Test;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.stereotype.Component;

/**
 * Offline lint of {@code aws/}: state machines reference only existing states and Batch job definitions,
 * job definitions follow conventions.md, and every job in the image has a job definition.
 * (Online validation: {@code aws stepfunctions validate-state-machine-definition}, see README.)
 */
class AwsArtifactsTest {

    static final Path AWS = Path.of("aws");
    static final Set<String> ENV = Set.of("DB_HOST", "DB_PORT", "DB_NAME", "DB_USER", "DB_PASSWORD", "DB_SCHEMA",
            "S3_BUCKET", "AWS_REGION");
    static final Set<String> OPTIONAL_MODULE_JOBS = Set.of("purge-expired-authorizations");
    private final ObjectMapper mapper = new ObjectMapper();

    @ParameterizedTest
    @ValueSource(strings = {"daily-cycle.asl.json", "transaction-report.asl.json"})
    void stateMachineIsConsistent(String file) throws IOException {
        JsonNode asl = mapper.readTree(AWS.resolve("state-machine").resolve(file).toFile());
        JsonNode states = asl.get("States");
        assertThat(states.has(asl.get("StartAt").asText())).isTrue();
        Set<String> reachable = new HashSet<>();
        reachable.add(asl.get("StartAt").asText());
        for (Iterator<Map.Entry<String, JsonNode>> it = states.fields(); it.hasNext();) {
            Map.Entry<String, JsonNode> e = it.next();
            JsonNode s = e.getValue();
            String type = s.get("Type").asText();
            assertThat(type).isIn("Task", "Pass", "Choice", "Wait", "Succeed", "Fail");
            List<String> targets = new java.util.ArrayList<>();
            if (s.has("Next")) {
                targets.add(s.get("Next").asText());
            }
            if (s.has("Default")) {
                targets.add(s.get("Default").asText());
            }
            s.path("Choices").forEach(c -> targets.add(c.get("Next").asText()));
            s.path("Catch").forEach(c -> targets.add(c.get("Next").asText()));
            for (String t : targets) {
                assertThat(states.has(t)).as("%s -> %s", e.getKey(), t).isTrue();
            }
            reachable.addAll(targets);
            if (!type.equals("Succeed") && !type.equals("Fail") && !type.equals("Choice")) {
                assertThat(s.has("Next") || s.path("End").asBoolean()).as(e.getKey()).isTrue();
            }
            if (s.path("Resource").asText().equals("arn:aws:states:::batch:submitJob.sync")) {
                String def = s.at("/Arguments/JobDefinition").asText();
                String job = def.substring("carddemo-".length());
                if (!OPTIONAL_MODULE_JOBS.contains(job)) {
                    assertThat(AWS.resolve("job-definitions/" + job + ".json")).as(def).exists();
                }
                assertThat(s.at("/Arguments/ContainerOverrides/Command/0").asText()).isEqualTo("--job=" + job);
                assertThat(s.has("Retry")).isTrue();
                assertThat(s.path("Catch").get(0).get("Next").asText()).isEqualTo("NotifyFailure");
            }
        }
        for (Iterator<String> it = states.fieldNames(); it.hasNext();) {
            String name = it.next();
            assertThat(reachable).as("state %s is unreachable", name).contains(name);
        }
    }

    @Test
    void dailyCycleOrderFollowsTheSchedulers() throws IOException {
        JsonNode states = mapper.readTree(AWS.resolve("state-machine/daily-cycle.asl.json").toFile()).get("States");
        assertThat(states.at("/PostDailyTransactionsCheck/Choices/1/Next").asText())
                .isEqualTo("PostDailyTransactionsWarning");
        assertThat(states.at("/PostDailyTransactionsWarning/Next").asText()).isEqualTo("WaitStep");
        assertThat(states.at("/WaitStep/Type").asText()).isEqualTo("Wait");
        assertThat(states.at("/WaitStep/Next").asText()).isEqualTo("BackupTransactions");
        assertThat(states.at("/CalculateInterestCheck/Choices/0/Next").asText()).isEqualTo("CombineTransactions");
        assertThat(states.at("/CombineTransactionsCheck/Choices/0/Next").asText()).isEqualTo("CreateStatements");
        assertThat(states.at("/CreateStatementsCheck/Choices/0/Next").asText()).isEqualTo("StatementPdf");
        assertThat(states.toString()).doesNotContain("CLOSEFIL").doesNotContain("OPENFIL");
    }

    @Test
    void everyJobHasAConventionalJobDefinition() throws Exception {
        ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AnnotationTypeFilter(Component.class));
        Set<String> jobClasses = new HashSet<>();
        scanner.findCandidateComponents("com.carddemo.batch").forEach(bd -> jobClasses.add(bd.getBeanClassName()));
        long jobs = jobClasses.stream().filter(c -> {
            try {
                return com.carddemo.batch.core.CardDemoJob.class.isAssignableFrom(Class.forName(c));
            } catch (ClassNotFoundException e) {
                throw new IllegalStateException(e);
            }
        }).count();

        try (Stream<Path> files = Files.list(AWS.resolve("job-definitions"))) {
            List<Path> defs = files.toList();
            assertThat(defs).hasSize((int) jobs);
            for (Path f : defs) {
                JsonNode d = mapper.readTree(f.toFile());
                String job = f.getFileName().toString().replace(".json", "");
                assertThat(d.get("jobDefinitionName").asText()).isEqualTo("carddemo-" + job);
                assertThat(d.get("platformCapabilities").get(0).asText()).isEqualTo("FARGATE");
                JsonNode c = d.get("containerProperties");
                assertThat(c.at("/command/0").asText()).isEqualTo("--job=" + job);
                Set<String> names = new HashSet<>();
                c.get("environment").forEach(e -> names.add(e.get("name").asText()));
                c.get("secrets").forEach(e -> names.add(e.get("name").asText()));
                assertThat(names).containsAll(ENV).doesNotContain("AWS_DEFAULT_REGION", "REGION");
                assertThat(c.at("/logConfiguration/options/awslogs-group").asText()).isEqualTo("/aws/batch/carddemo");
                assertThat(d.at("/timeout/attemptDurationSeconds").asInt()).isEqualTo(3600);
            }
        }
    }
}
