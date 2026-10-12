package com.carddemo.zerotrust.audit;

import java.nio.file.Path;

/**
 * Command-line verifier. Usage: {@code AuditChainVerifierCli <log> [expectedHeadHash]}.
 * Prints exactly one line — {@code VALID entries=N head=<hash>} or
 * {@code BROKEN at seq=K: <problem>} — and exits 0 or 1.
 */
public final class AuditChainVerifierCli {

    private AuditChainVerifierCli() {
    }

    public static int run(String[] args) {
        if (args.length < 1 || args.length > 2) {
            System.err.println("usage: AuditChainVerifierCli <audit-log> [expectedHeadHash]");
            return 2;
        }
        Path path = Path.of(args[0]);
        VerificationResult result;
        try {
            if (args.length == 2) {
                // The anchor's sequence is taken from the log tail when only a hash is given:
                // an exact-seq anchor requires the caller to pass seq too, so a hash-only
                // anchor checks the head hash against the last entry.
                var lines = readLines(path);
                long lastSeq = lines.isEmpty() ? 0
                        : AuditEntry.fromJson(lines.getLast()).seq();
                result = AuditChainVerifier.verify(path, new AuditAnchor(lastSeq, args[1]));
            } else {
                result = AuditChainVerifier.verify(path);
            }
        } catch (RuntimeException e) {
            System.out.println("BROKEN at seq=0: " + e.getMessage());
            return 1;
        }
        if (result.valid()) {
            String head = readHead(path);
            System.out.println("VALID entries=" + result.entriesChecked() + " head=" + head);
            return 0;
        }
        System.out.println("BROKEN at seq=" + result.firstBadSeq() + ": " + result.problem());
        return 1;
    }

    private static java.util.List<String> readLines(Path path) {
        try {
            return java.nio.file.Files.readAllLines(path);
        } catch (java.io.IOException e) {
            throw new java.io.UncheckedIOException(e);
        }
    }

    private static String readHead(Path path) {
        try {
            var lines = java.nio.file.Files.readAllLines(path);
            return lines.isEmpty() ? AuditEntry.GENESIS_HASH
                    : AuditEntry.fromJson(lines.getLast()).hash();
        } catch (Exception e) {
            return "<unreadable>";
        }
    }

    public static void main(String[] args) {
        System.exit(run(args));
    }
}
