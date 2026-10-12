package com.carddemo.zerotrust.posting;

import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.identity.AccessTokenVerifier;
import com.carddemo.zerotrust.identity.LocalTokenIssuer;
import com.carddemo.zerotrust.identity.WorkloadCredentials;
import com.carddemo.zerotrust.identity.WorkloadTokenSource;
import com.carddemo.zerotrust.masking.DataMasker;
import com.carddemo.zerotrust.policy.Policy;
import com.carddemo.zerotrust.policy.PolicyDecisionPoint;
import com.carddemo.zerotrust.policy.PolicyLoader;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.Resource;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.time.Clock;

/**
 * Wires the Zero Trust controls around the unchanged posttran posting job.
 *
 * <p>The local issuer and the workload key pair are created in memory at startup and the
 * workload is registered with the issuer — a stand-in for production, where the issuer is
 * the platform OIDC/ICAM provider and the workload key comes from the platform
 * workload-identity system (inherited, not claimed here).
 */
@Configuration
public class ZeroTrustConfiguration {

    @Bean
    Clock zeroTrustClock() {
        return Clock.systemUTC();
    }

    @Bean
    WorkloadCredentials workloadCredentials(ZeroTrustProperties properties) {
        return WorkloadCredentials.generate(properties.workloadId());
    }

    @Bean
    LocalTokenIssuer localTokenIssuer(ZeroTrustProperties properties,
                                      WorkloadCredentials credentials, Clock zeroTrustClock) {
        LocalTokenIssuer issuer = new LocalTokenIssuer(properties.issuer(),
                properties.tokenTtl(), zeroTrustClock);
        issuer.registerWorkload(credentials.workloadId(), credentials.publicKey());
        return issuer;
    }

    @Bean
    AccessTokenVerifier accessTokenVerifier(LocalTokenIssuer issuer,
                                            ZeroTrustProperties properties,
                                            Clock zeroTrustClock) {
        return new AccessTokenVerifier(issuer.verificationKey(), properties.issuer(),
                properties.audience(), zeroTrustClock);
    }

    @Bean
    WorkloadTokenSource workloadTokenSource(WorkloadCredentials credentials,
                                            LocalTokenIssuer issuer,
                                            ZeroTrustProperties properties,
                                            Clock zeroTrustClock) {
        return new WorkloadTokenSource(credentials, issuer, properties.audience(), zeroTrustClock);
    }

    @Bean
    PolicyDecisionPoint policyDecisionPoint(ZeroTrustProperties properties) {
        Resource resource = new DefaultResourceLoader().getResource(properties.policy());
        try (InputStream in = resource.getInputStream()) {
            Policy policy = PolicyLoader.load(in);
            return new PolicyDecisionPoint(policy);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot load policy " + properties.policy(), e);
        }
    }

    @Bean
    HashChainedAuditLog hashChainedAuditLog(ZeroTrustProperties properties, Clock zeroTrustClock) {
        return new HashChainedAuditLog(properties.auditLog(), zeroTrustClock);
    }

    @Bean
    DataMasker dataMasker() {
        return new DataMasker();
    }

    @Bean
    PostedTotals postedTotals() {
        return new PostedTotals();
    }

    @Bean
    SecuredTransactionPostingService securedTransactionPostingService(
            AccessTokenVerifier verifier, PolicyDecisionPoint pdp,
            HashChainedAuditLog auditLog, DataMasker masker) {
        return new SecuredTransactionPostingService(verifier, pdp, auditLog, masker);
    }

    @Bean
    CutoverApprovals cutoverApprovals(PolicyDecisionPoint pdp, HashChainedAuditLog auditLog) {
        return new CutoverApprovals(pdp, auditLog);
    }

    @Bean
    AuditReader auditReader(PolicyDecisionPoint pdp, HashChainedAuditLog auditLog,
                            ZeroTrustProperties properties) {
        return new AuditReader(pdp, auditLog, properties.auditLog());
    }

    /**
     * Decorates the posttran beans by composition: the {@code @JobScope}d
     * {@code postingProcessor} target bean ({@code scopedTarget.postingProcessor}) is wrapped
     * in the secured processor, and the {@code dailyTransactionPostingJob} job is wrapped in
     * the job-level enforcement bracket. The posttran beans and steps are used unchanged.
     */
    @Bean
    static BeanPostProcessor zeroTrustDecoratingPostProcessor(
            ObjectProvider<WorkloadTokenSource> tokenSource,
            ObjectProvider<AccessTokenVerifier> verifier,
            ObjectProvider<PolicyDecisionPoint> pdp,
            ObjectProvider<HashChainedAuditLog> auditLog,
            ObjectProvider<PostedTotals> postedTotals,
            ObjectProvider<DataMasker> masker,
            ObjectProvider<SecuredTransactionPostingService> secured,
            ObjectProvider<org.springframework.batch.core.repository.JobRepository> jobRepository) {
        return new BeanPostProcessor() {
            @Override
            @SuppressWarnings("unchecked")
            public Object postProcessAfterInitialization(Object bean, String beanName)
                    throws BeansException {
                if ("scopedTarget.postingProcessor".equals(beanName)
                        && bean instanceof ItemProcessor<?, ?> processor) {
                    return new SecuredPostingProcessor(
                            (ItemProcessor<com.carddemo.posting.domain.DailyTransaction,
                                    com.carddemo.posting.domain.PostingOutcome>) processor,
                            tokenSource.getObject(), secured.getObject(), postedTotals.getObject());
                }
                if ("dailyTransactionPostingJob".equals(beanName)
                        && bean instanceof org.springframework.batch.core.Job job) {
                    return new SecuredJob(job, tokenSource.getObject(), verifier.getObject(),
                            pdp.getObject(), auditLog.getObject(), postedTotals.getObject(),
                            masker.getObject(), jobRepository.getObject());
                }
                return bean;
            }
        };
    }
}
