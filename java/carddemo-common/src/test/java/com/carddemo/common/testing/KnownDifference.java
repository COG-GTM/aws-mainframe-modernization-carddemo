package com.carddemo.common.testing;

/**
 * A documented, accepted divergence between expected and actual output. Declaring one is a deliberate,
 * reviewed decision: the comparison fails both on undeclared differences and on declared ones that no longer
 * occur (so the list cannot silently go stale).
 *
 * @param recordNumber one-based record number
 * @param field        copybook data name of the differing item
 * @param reason       why the difference is accepted
 */
public record KnownDifference(int recordNumber, String field, String reason) {
}
