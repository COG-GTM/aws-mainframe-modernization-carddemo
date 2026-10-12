package com.carddemo.zerotrust.audit;

import com.carddemo.zerotrust.policy.SubjectKind;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Clock;
import java.time.Instant;
import java.util.List;

/**
 * Append-only, tamper-evident audit log: a JSON Lines file where every entry's hash
 * covers its content and its predecessor's hash. Appends are synchronized and fsync'd.
 *
 * <p>Opening an existing file verifies the whole chain first and resumes the sequence and
 * {@code prevHash} from the last entry. A broken chain throws {@link AuditIntegrityException}
 * and nothing more is appended — the log fails closed.
 */
public final class HashChainedAuditLog implements AutoCloseable {

    private final Path path;
    private final Clock clock;
    private final FileChannel channel;

    private long seq;
    private String prevHash = AuditEntry.GENESIS_HASH;

    public HashChainedAuditLog(Path path, Clock clock) {
        this.path = path;
        this.clock = clock;
        try {
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
            if (Files.exists(path) && Files.size(path) > 0) {
                resume();
            }
            this.channel = FileChannel.open(path,
                    StandardOpenOption.CREATE, StandardOpenOption.APPEND);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot open audit log " + path, e);
        }
    }

    private void resume() throws IOException {
        VerificationResult result = AuditChainVerifier.verify(path);
        if (!result.valid()) {
            throw new AuditIntegrityException("refusing to append to a broken audit chain at "
                    + path + ": seq=" + result.firstBadSeq() + " " + result.problem());
        }
        List<String> lines = Files.readAllLines(path, StandardCharsets.UTF_8);
        if (!lines.isEmpty()) {
            AuditEntry last = AuditEntry.fromJson(lines.getLast());
            this.seq = last.seq();
            this.prevHash = last.hash();
        }
    }

    public synchronized AuditEntry append(String actor, SubjectKind actorKind, String action,
                                          String outcome, String recordKey, String detail) {
        AuditEntry entry = AuditEntry.of(seq + 1, Instant.now(clock).toString(), actor,
                actorKind.name(), action, outcome, recordKey, detail, prevHash);
        try {
            channel.write(StandardCharsets.UTF_8.encode(entry.toJson() + "\n"));
            channel.force(false);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot append to audit log " + path, e);
        }
        seq = entry.seq();
        prevHash = entry.hash();
        return entry;
    }

    /** The current chain head, for storing as an external anchor. */
    public synchronized AuditAnchor head() {
        return new AuditAnchor(seq, prevHash);
    }

    public Path path() {
        return path;
    }

    @Override
    public synchronized void close() throws IOException {
        channel.close();
    }
}
