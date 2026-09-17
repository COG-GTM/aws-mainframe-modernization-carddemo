package com.carddemo.transaction.service;

import java.math.BigInteger;
import java.security.SecureRandom;
import org.springframework.stereotype.Component;

/**
 * The COBOL programs derived TRAN-ID from a counter held in the file. That counter cannot be shared
 * by several service instances, so the modern id keeps the 16 character width but is drawn at
 * random from base 36: 36^16 is about 2^82.7 values, so a billion transactions collide with
 * probability below 1e-7 (birthday bound n^2 / 2N).
 */
@Component
public class TransactionIdGenerator {

    private static final int LENGTH = 16;
    private static final BigInteger SPACE = BigInteger.valueOf(36).pow(LENGTH);

    private final SecureRandom random = new SecureRandom();

    public String next() {
        byte[] bytes = new byte[16];
        random.nextBytes(bytes);
        String id = new BigInteger(1, bytes).mod(SPACE).toString(36).toUpperCase();
        return "0".repeat(LENGTH - id.length()) + id;
    }
}
