package com.carddemo.zerotrust.audit;

import com.carddemo.zerotrust.policy.SubjectKind;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class HashChainedAuditLogTest {

    @TempDir
    Path dir;

    private Path writeLog(String name, int entries) throws Exception {
        Path p = dir.resolve(name);
        try (HashChainedAuditLog log = new HashChainedAuditLog(p, Clock.systemUTC())) {
            for (int i = 0; i < entries; i++) {
                log.append("svc-posttran-post", SubjectKind.WORKLOAD,
                        AuditAction.TRANSACTION_POSTED, "POSTED", "txn-" + i, "detail " + i);
            }
        }
        return p;
    }

    private static List<String> lines(Path p) throws Exception {
        return new ArrayList<>(Files.readAllLines(p, StandardCharsets.UTF_8));
    }

    private static void write(Path p, List<String> lines) throws Exception {
        Files.write(p, lines, StandardCharsets.UTF_8);
    }

    @Test
    void appendedChainVerifies() throws Exception {
        Path p = writeLog("ok.jsonl", 5);
        VerificationResult r = AuditChainVerifier.verify(p);
        assertThat(r.valid()).isTrue();
        assertThat(r.entriesChecked()).isEqualTo(5);
    }

    @Test
    void modifiedFieldIsDetected() throws Exception {
        Path p = writeLog("mod.jsonl", 5);
        List<String> ls = lines(p);
        ls.set(2, ls.get(2).replace("detail 2", "detail X"));
        write(p, ls);
        VerificationResult r = AuditChainVerifier.verify(p);
        assertThat(r.valid()).isFalse();
        assertThat(r.firstBadSeq()).isEqualTo(3);
        assertThat(r.problem()).contains("hash mismatch");
    }

    @Test
    void deletedEntryIsDetected() throws Exception {
        Path p = writeLog("del.jsonl", 5);
        List<String> ls = lines(p);
        ls.remove(2);
        write(p, ls);
        VerificationResult r = AuditChainVerifier.verify(p);
        assertThat(r.valid()).isFalse();
        assertThat(r.firstBadSeq()).isEqualTo(3);
    }

    @Test
    void reorderedEntriesAreDetected() throws Exception {
        Path p = writeLog("swap.jsonl", 5);
        List<String> ls = lines(p);
        String tmp = ls.get(1);
        ls.set(1, ls.get(2));
        ls.set(2, tmp);
        write(p, ls);
        VerificationResult r = AuditChainVerifier.verify(p);
        assertThat(r.valid()).isFalse();
        assertThat(r.firstBadSeq()).isEqualTo(2);
    }

    @Test
    void insertedEntryIsDetected() throws Exception {
        Path p = writeLog("ins.jsonl", 5);
        List<String> ls = lines(p);
        AuditEntry fake = AuditEntry.of(3, "2025-01-01T00:00:00Z", "mallory",
                "WORKLOAD", "TRANSACTION_POSTED", "POSTED", "txn-x", "injected",
                AuditEntry.fromJson(ls.get(2)).prevHash());
        // even a well-formed entry cannot match the expected seq/prevHash position twice
        ls.add(3, fake.toJson());
        write(p, ls);
        VerificationResult r = AuditChainVerifier.verify(p);
        assertThat(r.valid()).isFalse();
        assertThat(r.firstBadSeq()).isEqualTo(4);
    }

    @Test
    void malformedLineIsDetected() throws Exception {
        Path p = writeLog("mal.jsonl", 3);
        List<String> ls = lines(p);
        ls.set(1, "{not json");
        write(p, ls);
        VerificationResult r = AuditChainVerifier.verify(p);
        assertThat(r.valid()).isFalse();
        assertThat(r.firstBadSeq()).isEqualTo(2);
        assertThat(r.problem()).contains("malformed");
    }

    @Test
    void reopenResumesChain() throws Exception {
        Path p = writeLog("resume.jsonl", 3);
        AuditAnchor head;
        try (HashChainedAuditLog log = new HashChainedAuditLog(p, Clock.systemUTC())) {
            AuditEntry e = log.append("svc-posttran-post", SubjectKind.WORKLOAD,
                    AuditAction.JOB_COMPLETED, "COMPLETED", "job", "done");
            assertThat(e.seq()).isEqualTo(4);
            head = log.head();
        }
        assertThat(AuditChainVerifier.verify(p).valid()).isTrue();
        assertThat(AuditChainVerifier.verify(p, head).valid()).isTrue();
    }

    @Test
    void openingTamperedLogRefusesToAppend() throws Exception {
        Path p = writeLog("tampered.jsonl", 3);
        List<String> ls = lines(p);
        ls.set(1, ls.get(1).replace("txn-1", "txn-Z"));
        write(p, ls);
        assertThatThrownBy(() -> new HashChainedAuditLog(p, Clock.systemUTC()))
                .isInstanceOf(AuditIntegrityException.class);
    }

    @Test
    void anchorDetectsTailTruncation() throws Exception {
        Path p = writeLog("anchor.jsonl", 5);
        AuditAnchor head;
        try (HashChainedAuditLog log = new HashChainedAuditLog(p, Clock.systemUTC())) {
            head = log.head();
        }
        List<String> ls = lines(p);
        ls.removeLast();
        write(p, ls);
        VerificationResult r = AuditChainVerifier.verify(p, head);
        assertThat(r.valid()).isFalse();
        assertThat(r.problem()).contains("anchor");
    }

    @Test
    void anchorDetectsReplacedHead() throws Exception {
        Path p = writeLog("head.jsonl", 4);
        AuditAnchor head;
        try (HashChainedAuditLog log = new HashChainedAuditLog(p, Clock.systemUTC())) {
            head = log.head();
        }
        // rewrite the last entry keeping the chain internally valid is impossible without
        // recomputing — replace with a full valid rewrite of a different log instead
        Path other = writeLog("head-other.jsonl", 4);
        Files.copy(other, p, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        VerificationResult r = AuditChainVerifier.verify(p, head);
        assertThat(r.valid()).isFalse();
    }

    @Test
    void cliPrintsValidLine() throws Exception {
        Path p = writeLog("cli.jsonl", 3);
        String out = captureStdout(() -> AuditChainVerifierCli.run(new String[]{p.toString()}));
        assertThat(out.trim()).matches("VALID entries=3 head=[0-9a-f]{64}");
    }

    @Test
    void cliPrintsBrokenLine() throws Exception {
        Path p = writeLog("clibad.jsonl", 3);
        List<String> ls = lines(p);
        ls.set(1, ls.get(1).replace("POSTED", "XXXXXX"));
        write(p, ls);
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        PrintStream old = System.out;
        int code;
        try {
            System.setOut(new PrintStream(buf));
            code = AuditChainVerifierCli.run(new String[]{p.toString()});
        } finally {
            System.setOut(old);
        }
        assertThat(code).isEqualTo(1);
        assertThat(buf.toString().trim()).matches("BROKEN at seq=2: .+");
    }

    private static String captureStdout(Runnable r) {
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        PrintStream old = System.out;
        try {
            System.setOut(new PrintStream(buf));
            r.run();
        } finally {
            System.setOut(old);
        }
        return buf.toString();
    }
}
