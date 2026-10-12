package com.carddemo.zerotrust.posting;

import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.posting.domain.PostingLedger;
import com.carddemo.posting.domain.PostingOutcome;
import com.carddemo.posting.domain.TransactionPostingService;
import com.carddemo.recordio.layout.Account;
import com.carddemo.zerotrust.audit.AuditChainVerifier;
import com.carddemo.zerotrust.audit.AuditEntry;
import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.identity.AccessTokenVerifier;
import com.carddemo.zerotrust.identity.LocalTokenIssuer;
import com.carddemo.zerotrust.identity.MutableClock;
import com.carddemo.zerotrust.identity.TokenRejectedException;
import com.carddemo.zerotrust.identity.WorkloadCredentials;
import com.carddemo.zerotrust.masking.DataMasker;
import com.carddemo.zerotrust.policy.PolicyDecisionPoint;
import com.carddemo.zerotrust.policy.PolicyLoader;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class SecuredTransactionPostingServiceTest {

    private static final String ISSUER_ID = "urn:carddemo:local-issuer";
    private static final String AUDIENCE = "urn:carddemo:posting";

    @TempDir
    Path dir;

    private MutableClock clock;
    private LocalTokenIssuer issuer;
    private WorkloadCredentials post;
    private WorkloadCredentials report;
    private AccessTokenVerifier verifier;
    private HashChainedAuditLog audit;
    private Path auditPath;
    private PostingLedger ledger;
    private SecuredTransactionPostingService secured;

    @BeforeEach
    void setUp() throws Exception {
        clock = new MutableClock(Instant.parse("2025-01-01T00:00:00Z"));
        issuer = new LocalTokenIssuer(ISSUER_ID, Duration.ofMinutes(5), clock);
        post = WorkloadCredentials.generate("svc-posttran-post");
        report = WorkloadCredentials.generate("svc-posttran-report");
        issuer.registerWorkload(post.workloadId(), post.publicKey());
        issuer.registerWorkload(report.workloadId(), report.publicKey());
        verifier = new AccessTokenVerifier(issuer.verificationKey(), ISSUER_ID, AUDIENCE, clock);
        auditPath = dir.resolve("audit.jsonl");
        audit = new HashChainedAuditLog(auditPath, clock);
        ledger = PostingTestFixtures.ledger();
        PolicyDecisionPoint pdp = new PolicyDecisionPoint(PolicyLoader.load(
                getClass().getResourceAsStream("/zero-trust-policy.yaml")));
        secured = new SecuredTransactionPostingService(verifier, pdp, audit, new DataMasker());
    }

    private String token(WorkloadCredentials c) {
        return issuer.issue(c.createClientAssertion(ISSUER_ID, clock), AUDIENCE);
    }

    private TransactionPostingService delegate() {
        return new TransactionPostingService(ledger, Clock.systemUTC());
    }

    private List<AuditEntry> entries() throws Exception {
        audit.close();
        List<AuditEntry> es = Files.readAllLines(auditPath).stream()
                .map(AuditEntry::fromJson).toList();
        audit = new HashChainedAuditLog(auditPath, clock);
        return es;
    }

    @Test
    void validTokenPostsAndAuditsMasked() throws Exception {
        PostingOutcome out = secured.process(token(post),
                PostingTestFixtures.item("t-1", PostingTestFixtures.CARD_OK, "10.00"), delegate());
        assertThat(out).isInstanceOf(PostingOutcome.Posted.class);
        List<AuditEntry> es = entries();
        assertThat(es).hasSize(1);
        assertThat(es.get(0).action()).isEqualTo("TRANSACTION_POSTED");
        assertThat(es.get(0).outcome()).isEqualTo("POSTED");
        assertThat(es.get(0).detail()).contains("************0001");
        assertThat(es.get(0).detail()).doesNotContain(PostingTestFixtures.CARD_OK);
        assertThat(es.get(0).detail()).contains("authz=ALLOW role=posting-job");
        assertThat(AuditChainVerifier.verify(auditPath).valid()).isTrue();
    }

    @Test
    void invalidCardAuditsRejectedWithCode100() throws Exception {
        PostingOutcome out = secured.process(token(post),
                PostingTestFixtures.item("t-2", PostingTestFixtures.CARD_BAD, "10.00"), delegate());
        assertThat(out).isInstanceOf(PostingOutcome.Rejected.class);
        List<AuditEntry> es = entries();
        assertThat(es).hasSize(1);
        assertThat(es.get(0).action()).isEqualTo("TRANSACTION_REJECTED");
        assertThat(es.get(0).detail()).contains("code=100");
        assertThat(es.get(0).detail()).contains("************9999");
        assertThat(es.get(0).detail()).doesNotContain(PostingTestFixtures.CARD_BAD);
    }

    @Test
    void expiredTokenIsDeniedAndLedgerUntouched() throws Exception {
        String token = token(post);
        clock.advance(Duration.ofMinutes(6));
        BigDecimal before = ledger.findAccount(PostingTestFixtures.ACCT_OK)
                .map(Account::currentBalance).orElseThrow();
        assertThatThrownBy(() -> secured.process(token,
                PostingTestFixtures.item("t-3", PostingTestFixtures.CARD_OK, "10.00"), delegate()))
                .isInstanceOf(AccessDeniedException.class);
        BigDecimal after = ledger.findAccount(PostingTestFixtures.ACCT_OK)
                .map(Account::currentBalance).orElseThrow();
        assertThat(after).isEqualByComparingTo(before);
        List<AuditEntry> es = entries();
        assertThat(es).hasSize(1);
        assertThat(es.get(0).action()).isEqualTo("AUTHZ_DECISION");
        assertThat(es.get(0).outcome()).isEqualTo("DENY");
        assertThat(es.get(0).detail()).contains("token rejected");
    }

    @Test
    void forgedTokenIsDeniedAndLedgerUntouched() throws Exception {
        LocalTokenIssuer other = new LocalTokenIssuer(ISSUER_ID, Duration.ofMinutes(5), clock);
        WorkloadCredentials rogue = WorkloadCredentials.generate("svc-posttran-post");
        other.registerWorkload(rogue.workloadId(), rogue.publicKey());
        String forged = other.issue(rogue.createClientAssertion(ISSUER_ID, clock), AUDIENCE);
        BigDecimal before = ledger.findAccount(PostingTestFixtures.ACCT_OK)
                .map(Account::currentBalance).orElseThrow();
        assertThatThrownBy(() -> secured.process(forged,
                PostingTestFixtures.item("t-4", PostingTestFixtures.CARD_OK, "10.00"), delegate()))
                .isInstanceOf(AccessDeniedException.class);
        assertThat(ledger.findAccount(PostingTestFixtures.ACCT_OK)
                .map(Account::currentBalance).orElseThrow()).isEqualByComparingTo(before);
        assertThat(entries().get(0).outcome()).isEqualTo("DENY");
    }

    @Test
    void wrongWorkloadIsDeniedByPolicyAndLedgerUntouched() throws Exception {
        // svc-posttran-report has a valid token but no run-posting grant.
        String token = token(report);
        BigDecimal before = ledger.findAccount(PostingTestFixtures.ACCT_OK)
                .map(Account::currentBalance).orElseThrow();
        assertThatThrownBy(() -> secured.process(token,
                PostingTestFixtures.item("t-5", PostingTestFixtures.CARD_OK, "10.00"), delegate()))
                .isInstanceOf(AccessDeniedException.class)
                .hasMessageContaining("denied");
        assertThat(ledger.findAccount(PostingTestFixtures.ACCT_OK)
                .map(Account::currentBalance).orElseThrow()).isEqualByComparingTo(before);
        List<AuditEntry> es = entries();
        assertThat(es.get(0).outcome()).isEqualTo("DENY");
        assertThat(es.get(0).detail()).contains("NO_MATCHING_ROLE");
    }

    @Test
    void tokenWithWrongAudienceIsRejected() {
        String token = issuer.issue(post.createClientAssertion(ISSUER_ID, clock),
                "urn:carddemo:other");
        assertThatThrownBy(() -> secured.process(token,
                PostingTestFixtures.item("t-6", PostingTestFixtures.CARD_OK, "10.00"), delegate()))
                .isInstanceOf(AccessDeniedException.class);
    }

    @Test
    void tokenSourceRefreshPathFeedsProcessor() throws Exception {
        var source = new com.carddemo.zerotrust.identity.WorkloadTokenSource(
                post, issuer, AUDIENCE, clock);
        SecuredPostingProcessor processor = new SecuredPostingProcessor(
                item -> delegate().process(item), source, secured, new PostingAuditBuffer());
        PostingOutcome out = processor.process(
                PostingTestFixtures.item("t-7", PostingTestFixtures.CARD_OK, "10.00"));
        assertThat(out).isInstanceOf(PostingOutcome.Posted.class);
    }
}
