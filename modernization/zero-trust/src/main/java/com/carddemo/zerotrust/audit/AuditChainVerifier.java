package com.carddemo.zerotrust.audit;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Replays an audit log file and re-checks the hash chain end to end. Detects modified
 * fields (hash mismatch), deleted entries (sequence gap / prevHash mismatch), reordered or
 * inserted entries, malformed lines, and — when an externally held {@link AuditAnchor} is
 * supplied — tail truncation or a replaced head.
 */
public final class AuditChainVerifier {

    private AuditChainVerifier() {
    }

    public static VerificationResult verify(Path path) {
        return verify(path, null);
    }

    public static VerificationResult verify(Path path, AuditAnchor expectedHead) {
        List<String> lines;
        try {
            lines = Files.readAllLines(path, StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read audit log " + path, e);
        }
        long expectedSeq = 1;
        String prevHash = AuditEntry.GENESIS_HASH;
        String headHash = prevHash;
        long checked = 0;
        for (String line : lines) {
            AuditEntry entry;
            try {
                entry = AuditEntry.fromJson(line);
            } catch (AuditIntegrityException e) {
                return VerificationResult.broken(checked, expectedSeq, "malformed line: " + e.getMessage());
            }
            if (entry.seq() != expectedSeq) {
                return VerificationResult.broken(checked, expectedSeq,
                        "sequence gap: expected seq " + expectedSeq + " but found " + entry.seq());
            }
            if (!entry.prevHash().equals(prevHash)) {
                return VerificationResult.broken(checked, entry.seq(),
                        "prevHash mismatch: entry does not chain to its predecessor");
            }
            String recomputed = AuditEntry.sha256Hex(entry.canonicalJson());
            if (!recomputed.equals(entry.hash())) {
                return VerificationResult.broken(checked, entry.seq(),
                        "hash mismatch: entry content was modified");
            }
            prevHash = entry.hash();
            headHash = entry.hash();
            expectedSeq++;
            checked++;
        }
        if (expectedHead != null
                && (expectedHead.seq() != expectedSeq - 1 || !expectedHead.hash().equals(headHash))) {
            long badSeq = expectedHead.seq() > expectedSeq - 1 ? expectedSeq : expectedHead.seq();
            return VerificationResult.broken(checked, badSeq,
                    "anchor mismatch: log head differs from the expected anchor "
                            + "(tail truncated or head replaced)");
        }
        return VerificationResult.valid(checked);
    }
}
