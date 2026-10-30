package com.carddemo.authfrds.domain;

import com.carddemo.authfrds.domain.ims.ImsStatusException;
import com.carddemo.authfrds.domain.ims.PendingAuthorizationStore;
import com.carddemo.authfrds.domain.ims.PendingAuthorizationSummary;
import java.util.concurrent.ConcurrentHashMap;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionOperations;

/** COPAUS1C paths that reach AUTHFRDS: ENTER (view), PF5 (MARK-AUTH-FRAUD) and PF8 (next authorization). */
@Service
public class AuthorizationFraudService {

    public record ToggleResult(String message, FraudResult fraud, AuthorizationDetail authorization) {
    }

    private final PendingAuthorizationStore ims;
    private final FraudRecordService fraudRecords;
    private final TransactionOperations tx;
    private final ConcurrentHashMap<String, Object> keyLocks = new ConcurrentHashMap<>();

    public AuthorizationFraudService(PendingAuthorizationStore ims, FraudRecordService fraudRecords) {
        this(ims, fraudRecords, TransactionOperations.withoutTransaction());
    }

    @Autowired
    public AuthorizationFraudService(PendingAuthorizationStore ims, FraudRecordService fraudRecords,
                                     ObjectProvider<TransactionOperations> tx) {
        this(ims, fraudRecords, tx.getIfAvailable(TransactionOperations::withoutTransaction));
    }

    public AuthorizationFraudService(PendingAuthorizationStore ims, FraudRecordService fraudRecords,
                                     TransactionOperations tx) {
        this.ims = ims;
        this.fraudRecords = fraudRecords;
        this.tx = tx;
    }

    /** BR-AF-025/026: PROCESS-ENTER-KEY -> READ-AUTH-RECORD (GU summary, GNP detail by key). */
    public AuthorizationDetail view(long acctId, AuthKey key) {
        summary(acctId);
        return detail(acctId, key);
    }

    /** BR-AF-027: PROCESS-PF8-KEY -> READ-AUTH-RECORD + READ-NEXT-AUTH-RECORD. */
    public AuthorizationDetail next(long acctId, AuthKey key) {
        summary(acctId);
        detail(acctId, key);
        return wrapIms(() -> ims.findNextDetail(acctId, key), AuthFraudRules.READ_NEXT_ERROR, "BR-AF-027")
                .orElseThrow(() -> new LegacyRuleException("BR-AF-027", LegacyRuleException.Kind.END_OF_DATA,
                        AuthFraudRules.LAST_AUTHORIZATION));
    }

    /**
     * BR-AF-020..024: PF5 MARK-AUTH-FRAUD. One unit of work: the COPAUS2C Db2 write and the IMS REPL commit
     * together (CICS SYNCPOINT) or roll back together (SYNCPOINT ROLLBACK) - any exception rolls back Db2 here.
     * PF5 on one authorization is serialised (IMS held the segment for the unit of work); if the Db2 commit
     * fails after the IMS REPL, the IMS stub detail is restored so both stores roll back.
     */
    public ToggleResult toggleFraud(long acctId, Long custId, AuthKey key) {
        synchronized (keyLocks.computeIfAbsent(acctId + "/" + key, k -> new Object())) {
            AuthorizationDetail[] replaced = new AuthorizationDetail[1];
            try {
                return tx.execute(status -> toggleInUnitOfWork(acctId, custId, key, replaced));
            } catch (RuntimeException e) {
                if (replaced[0] != null) {
                    ims.replaceDetail(acctId, replaced[0]);
                }
                throw e;
            }
        }
    }

    private ToggleResult toggleInUnitOfWork(long acctId, Long custId, AuthKey key, AuthorizationDetail[] replaced) {
        PendingAuthorizationSummary sum = summary(acctId);
        AuthorizationDetail current = detail(acctId, key);
        // BR-AF-020: PA-FRAUD-CONFIRMED -> remove ('R'), otherwise report ('F')
        FraudAction action = AuthFraudRules.toggle(current.authFraud());
        // BR-AF-021: WS-FRAUD-DATA = acct id, cust id (CDEMO-CUST-ID), detail with the toggled flag, action
        long cust = custId != null ? custId : sum.custId();
        FraudResult result = fraudRecords.record(new FraudRequest(acctId, cust, action.code(),
                current.withFraud(action.code(), current.fraudRptDate())));
        if (!result.success()) {
            // BR-AF-023: WS-FRD-UPDT-FAILED -> WS-MESSAGE = WS-FRD-ACT-MSG, SYNCPOINT ROLLBACK
            throw new LegacyRuleException("BR-AF-023", LegacyRuleException.Kind.FRAUD_UPDATE_FAILED,
                    result.message(), result.sqlCode());
        }
        // BR-AF-022: UPDATE-AUTH-DETAILS - REPL the returned record (flag + PA-FRAUD-RPT-DATE MM/DD/YY)
        AuthorizationDetail updated = current.withFraud(action.code(), result.fraudRptDate());
        try {
            ims.replaceDetail(acctId, updated);
            replaced[0] = current;
        } catch (ImsStatusException e) {
            // BR-AF-024: REPL failed -> ROLL-BACK (undoes the AUTHFRDS write) + ' System error while FRAUD Tagging'
            throw new LegacyRuleException("BR-AF-024", LegacyRuleException.Kind.IMS_ERROR,
                    AuthFraudRules.fraudTaggingRollback(e.status()));
        }
        return new ToggleResult(AuthFraudRules.toggleMessage(action), result, updated);
    }

    private PendingAuthorizationSummary summary(long acctId) {
        return wrapIms(() -> ims.findSummary(acctId), AuthFraudRules.READ_SUMMARY_ERROR, "BR-AF-025")
                .orElseThrow(() -> notFound("account " + acctId + " has no pending authorization summary"));
    }

    private AuthorizationDetail detail(long acctId, AuthKey key) {
        return wrapIms(() -> ims.findDetail(acctId, key), AuthFraudRules.READ_DETAILS_ERROR, "BR-AF-025")
                .orElseThrow(() -> notFound("authorization " + key + " not found for account " + acctId));
    }

    /** BR-AF-025: GE/GB set AUTHS-EOF. COPAUS1C has no message for it; this text is Java-only. */
    private static LegacyRuleException notFound(String what) {
        return new LegacyRuleException("BR-AF-025", LegacyRuleException.Kind.NOT_FOUND, what);
    }

    private static <T> T wrapIms(java.util.function.Supplier<T> call, String prefix, String ruleId) {
        try {
            return call.get();
        } catch (ImsStatusException e) {
            throw new LegacyRuleException(ruleId, LegacyRuleException.Kind.IMS_ERROR, prefix + e.status());
        }
    }
}
