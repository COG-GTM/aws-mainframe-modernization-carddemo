package com.carddemo.zerotrust.posting;

import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.posting.domain.PostingOutcome;
import com.carddemo.zerotrust.audit.AuditChainVerifier;
import com.carddemo.zerotrust.audit.AuditEntry;
import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.identity.AccessTokenVerifier;
import com.carddemo.zerotrust.identity.LocalTokenIssuer;
import com.carddemo.zerotrust.identity.MutableClock;
import com.carddemo.zerotrust.identity.WorkloadCredentials;
import com.carddemo.zerotrust.masking.DataMasker;
import com.carddemo.zerotrust.policy.PolicyDecisionPoint;
import com.carddemo.zerotrust.policy.PolicyLoader;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.scope.context.StepSynchronizationManager;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.batch.test.MetaDataInstanceFactory;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * Outcomes are audited only once the chunk writer has succeeded. A writer failure yields
 * TRANSACTION_WRITE_FAILED / NOT_WRITTEN entries, never a TRANSACTION_POSTED for data that
 * did not reach the datasets.
 */
class SecuredOutcomeWriterTest {

    private static final String ISSUER_ID = "urn:carddemo:local-issuer";
    private static final String AUDIENCE = "urn:carddemo:posting";

    @TempDir
    Path dir;

    private MutableClock clock;
    private WorkloadCredentials post;
    private HashChainedAuditLog audit;
    private Path auditPath;
    private SecuredTransactionPostingService secured;
    private PostingAuditBuffer buffer;
    private org.springframework.batch.core.StepExecution stepExecution;

    @BeforeEach
    void setUp() throws Exception {
        clock = new MutableClock(Instant.parse("2025-01-01T00:00:00Z"));
        issuer = new LocalTokenIssuer(ISSUER_ID, Duration.ofMinutes(5), clock);
        post = WorkloadCredentials.generate("svc-posttran-post");
        issuer.registerWorkload(post.workloadId(), post.publicKey());
        AccessTokenVerifier verifier =
                new AccessTokenVerifier(issuer.verificationKey(), ISSUER_ID, AUDIENCE, clock);
        auditPath = dir.resolve("audit.jsonl");
        audit = new HashChainedAuditLog(auditPath, clock);
        PolicyDecisionPoint pdp = new PolicyDecisionPoint(PolicyLoader.load(
                getClass().getResourceAsStream("/zero-trust-policy.yaml")));
        secured = new SecuredTransactionPostingService(verifier, pdp, audit, new DataMasker());
        buffer = new PostingAuditBuffer();
        stepExecution = MetaDataInstanceFactory.createStepExecution();
        StepSynchronizationManager.register(stepExecution);
    }

    private LocalTokenIssuer issuer;

    @AfterEach
    void tearDown() throws Exception {
        StepSynchronizationManager.close();
        audit.close();
    }

    private PostingOutcome processed(String id, String card, String amount) throws Exception {
        // Run the processor so the buffer holds (outcome -> caller), as in the real flow.
        SecuredPostingProcessor processor = new SecuredPostingProcessor(
                item -> new PostingOutcome.Posted(item.transaction()),
                new com.carddemo.zerotrust.identity.WorkloadTokenSource(post, issuer, AUDIENCE, clock),
                secured, buffer);
        DailyTransaction item = PostingTestFixtures.item(id, card, amount);
        return processor.process(item);
    }

    private List<AuditEntry> entries() throws Exception {
        audit.close();
        List<AuditEntry> es = Files.readAllLines(auditPath).stream()
                .map(AuditEntry::fromJson).toList();
        audit = new HashChainedAuditLog(auditPath, clock);
        return es;
    }

    @Test
    void writerFailureAuditsNotWritten() throws Exception {
        PostingOutcome outcome = processed("t-1", PostingTestFixtures.CARD_OK, "10.00");
        ItemWriter<PostingOutcome> failing = chunk -> {
            throw new IllegalStateException("duplicate key");
        };
        SecuredOutcomeWriter writer = new SecuredOutcomeWriter(failing, buffer, secured,
                audit, new DataMasker());
        Chunk<PostingOutcome> chunk = new Chunk<>(List.of(outcome));
        assertThatThrownBy(() -> writer.write(chunk))
                .isInstanceOf(IllegalStateException.class);
        List<AuditEntry> es = entries();
        assertThat(es).hasSize(1);
        assertThat(es.get(0).action()).isEqualTo("TRANSACTION_WRITE_FAILED");
        assertThat(es.get(0).outcome()).isEqualTo("NOT_WRITTEN");
        assertThat(es.get(0).detail()).contains("************0001")
                .contains("IllegalStateException");
        var ctx = stepExecution.getJobExecution().getExecutionContext();
        assertThat(ctx.containsKey(SecuredOutcomeWriter.POSTED_COUNT_KEY)).isFalse();
        assertThat(AuditChainVerifier.verify(auditPath).valid()).isTrue();
    }

    @Test
    void writerSuccessAuditsOutcomeAndTotals() throws Exception {
        PostingOutcome o1 = processed("t-2", PostingTestFixtures.CARD_OK, "10.00");
        PostingOutcome o2 = processed("t-3", PostingTestFixtures.CARD_OK, "5.50");
        ItemWriter<PostingOutcome> ok = chunk -> { };
        SecuredOutcomeWriter writer = new SecuredOutcomeWriter(ok, buffer, secured,
                audit, new DataMasker());
        writer.write(new Chunk<>(List.of(o1, o2)));
        List<AuditEntry> es = entries();
        assertThat(es).hasSize(2);
        assertThat(es).allSatisfy(e -> {
            assertThat(e.action()).isEqualTo("TRANSACTION_POSTED");
            assertThat(e.outcome()).isEqualTo("POSTED");
        });
        var ctx = stepExecution.getJobExecution().getExecutionContext();
        assertThat(ctx.getLong(SecuredOutcomeWriter.POSTED_COUNT_KEY)).isEqualTo(2);
        assertThat(ctx.getString(SecuredOutcomeWriter.POSTED_AMOUNT_KEY)).isEqualTo("15.50");
    }
}
