package com.carddemo.zerotrust.masking;

import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionLayout;
import com.carddemo.zerotrust.audit.AuditAction;
import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.policy.AccessRequest;
import com.carddemo.zerotrust.policy.Decision;
import com.carddemo.zerotrust.policy.PolicyDecisionPoint;
import com.carddemo.zerotrust.policy.Subject;
import com.carddemo.zerotrust.posting.AccessDeniedException;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * The {@code view-rejects} read path: decodes a DALYREJS file (430-byte records — the
 * 350-byte DALYTRAN image, a 4-digit reason code at offset 350, and 76 bytes of reason
 * text at 354) and returns rows with the card number already masked. Access is gated by
 * the {@code view-rejects} permission and audited.
 */
public final class RejectView {

    public static final int RECORD_LENGTH = 430;
    private static final int IMAGE_LENGTH = Transaction.LENGTH;
    private static final int REASON_CODE = 350;
    private static final int REASON_TEXT = 354;
    private static final int REASON_TEXT_LENGTH = 76;

    private final PolicyDecisionPoint pdp;
    private final HashChainedAuditLog audit;
    private final DataMasker masker;

    public RejectView(PolicyDecisionPoint pdp, HashChainedAuditLog audit, DataMasker masker) {
        this.pdp = pdp;
        this.audit = audit;
        this.masker = masker;
    }

    public List<MaskedReject> view(Subject subject, Path rejectsFile) {
        Decision decision = pdp.decide(AccessRequest.of(subject, "view-rejects"));
        audit.append(subject.id(), subject.kind(), AuditAction.REJECTS_VIEWED,
                decision.allowed() ? "ALLOW" : "DENY", rejectsFile.getFileName().toString(),
                decision.allowed()
                        ? "authz=ALLOW role=" + decision.grantingRole().orElse("")
                        : "authz=" + decision.reason());
        if (!decision.allowed()) {
            throw new AccessDeniedException("authorization denied: " + decision.reason());
        }
        return read(rejectsFile);
    }

    /** Decodes a DALYREJS file without the authorization gate (used by tests/tools). */
    public List<MaskedReject> read(Path rejectsFile) {
        byte[] bytes;
        try {
            bytes = Files.readAllBytes(rejectsFile);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read rejects " + rejectsFile, e);
        }
        if (bytes.length % RECORD_LENGTH != 0) {
            throw new IllegalArgumentException(
                    "rejects file length " + bytes.length + " is not a multiple of " + RECORD_LENGTH);
        }
        List<MaskedReject> out = new ArrayList<>();
        RecordEncoding enc = RecordEncoding.EBCDIC;
        for (int off = 0; off < bytes.length; off += RECORD_LENGTH) {
            byte[] slice = Arrays.copyOfRange(bytes, off, off + RECORD_LENGTH);
            FixedWidthRecord record = new FixedWidthRecord(slice, enc);
            FixedWidthRecord image = new FixedWidthRecord(
                    Arrays.copyOfRange(slice, 0, IMAGE_LENGTH), enc);
            Transaction t = TransactionLayout.INSTANCE.decode(image);
            int code = (int) record.unsignedInt(REASON_CODE, 4);
            String text = record.trimmedText(REASON_TEXT, REASON_TEXT_LENGTH);
            out.add(new MaskedReject(t.id(), masker.maskPan(t.cardNumber()),
                    t.amount() == null ? BigDecimal.ZERO : t.amount(), code, text));
        }
        return out;
    }
}
