package com.carddemo.zerotrust.audit;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * One append-only audit record. {@code hash} is the SHA-256 of the canonical JSON of all
 * preceding fields in declared order; {@code prevHash} chains each entry to its predecessor
 * so modifications, deletions, insertions and reordering are all detectable.
 *
 * <p>Records never contain a full card number, SSN or password — card numbers pass through
 * the masker and {@code recordKey} carries the non-sensitive transaction id.
 */
public record AuditEntry(long seq, String timestamp, String actor, String actorKind,
                         String action, String outcome, String recordKey, String detail,
                         String prevHash, String hash) {

    public static final String GENESIS_HASH = "0".repeat(64);
    private static final ObjectMapper MAPPER = new ObjectMapper();

    /** Canonical JSON of every field except {@code hash}, in declared order. */
    public String canonicalJson() {
        ObjectNode n = MAPPER.createObjectNode();
        n.put("seq", seq);
        n.put("timestamp", timestamp);
        n.put("actor", actor);
        n.put("actorKind", actorKind);
        n.put("action", action);
        n.put("outcome", outcome);
        n.put("recordKey", recordKey);
        n.put("detail", detail);
        n.put("prevHash", prevHash);
        return n.toString();
    }

    /** JSON Lines form, all fields including {@code hash}, in declared order. */
    public String toJson() {
        ObjectNode n = MAPPER.createObjectNode();
        n.put("seq", seq);
        n.put("timestamp", timestamp);
        n.put("actor", actor);
        n.put("actorKind", actorKind);
        n.put("action", action);
        n.put("outcome", outcome);
        n.put("recordKey", recordKey);
        n.put("detail", detail);
        n.put("prevHash", prevHash);
        n.put("hash", hash);
        return n.toString();
    }

    public static AuditEntry of(long seq, String timestamp, String actor, String actorKind,
                                String action, String outcome, String recordKey, String detail,
                                String prevHash) {
        AuditEntry unsigned = new AuditEntry(seq, timestamp, actor, actorKind, action, outcome,
                recordKey, detail, prevHash, "");
        return new AuditEntry(seq, timestamp, actor, actorKind, action, outcome,
                recordKey, detail, prevHash, sha256Hex(unsigned.canonicalJson()));
    }

    public static AuditEntry fromJson(String line) {
        try {
            var n = MAPPER.readTree(line);
            return new AuditEntry(
                    n.required("seq").asLong(),
                    n.required("timestamp").asText(),
                    n.required("actor").asText(),
                    n.required("actorKind").asText(),
                    n.required("action").asText(),
                    n.required("outcome").asText(),
                    n.required("recordKey").asText(),
                    n.required("detail").asText(),
                    n.required("prevHash").asText(),
                    n.required("hash").asText());
        } catch (Exception e) {
            throw new AuditIntegrityException("malformed audit line: " + abbreviate(line));
        }
    }

    public static String sha256Hex(String canonicalJson) {
        try {
            byte[] digest = MessageDigest.getInstance("SHA-256")
                    .digest(canonicalJson.getBytes(StandardCharsets.UTF_8));
            StringBuilder hex = new StringBuilder(64);
            for (byte b : digest) {
                hex.append(Character.forDigit((b >> 4) & 0xF, 16));
                hex.append(Character.forDigit(b & 0xF, 16));
            }
            return hex.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    private static String abbreviate(String line) {
        return line.length() <= 80 ? line : line.substring(0, 80) + "...";
    }
}
