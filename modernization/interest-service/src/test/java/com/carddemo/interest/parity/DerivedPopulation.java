package com.carddemo.interest.parity;

import com.carddemo.interest.batch.InterestDatasets;

import java.io.ByteArrayOutputStream;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Builds a derived input population from the shipped datasets so that the parity harness can
 * exercise the arithmetic the shipped data cannot reach.
 *
 * <p>The shipped {@code TCATBALF} dump holds a zero balance on every record and every shipped
 * account has a blank {@code ACCT-GROUP-ID}, so the real population only ever walks one path
 * through the rate rules. This helper keeps the real {@code ACCTDATA}, {@code CARDXREF} and
 * {@code DISCGRP} datasets and rewrites two things: the pricing group on each account, and the
 * category balances, covering positive, negative, zero, capacity-stressing and
 * truncate-not-round amounts across the {@code A0000000nn}, {@code ZEROAPR} and blank
 * (i.e. {@code DEFAULT}) pricing groups.
 *
 * <p>The encoding here is written independently of the production record reader, on purpose.
 */
final class DerivedPopulation {

    private static final Charset CP037 = Charset.forName("IBM037");
    private static final int ACCOUNT_LENGTH = 300;
    private static final int TCATBAL_LENGTH = 50;
    private static final int ACCOUNT_GROUP_OFFSET = 112;

    private static final List<String> PRICING_GROUPS =
            List.of("A000000001", "ZEROAPR", "", "A000000002", "A000000004");

    private static final List<String> BALANCES =
            List.of("79.60", "-1234.56", "0.00", "9999999.99", "100.07", "-0.01");

    private static final List<String> CATEGORIES = List.of("0001", "0002", "0003", "0004");

    private DerivedPopulation() {
    }

    static InterestDatasets build(Path ebcdicDirectory) {
        InterestDatasets shipped = InterestDatasets.fromDirectory(ebcdicDirectory);
        byte[] accountImage = withPricingGroups(shipped.rawAccountImage());
        byte[] tcatbalImage = synthesiseCategoryBalances(accountImage);
        return new InterestDatasets(tcatbalImage, shipped.rawCardXrefImage(), accountImage,
                shipped.rawDisclosureGroupImage());
    }

    private static byte[] withPricingGroups(byte[] accountImage) {
        byte[] image = accountImage.clone();
        int accountCount = image.length / ACCOUNT_LENGTH;
        for (int index = 0; index < accountCount; index++) {
            String group = PRICING_GROUPS.get(index % PRICING_GROUPS.size());
            putText(image, index * ACCOUNT_LENGTH + ACCOUNT_GROUP_OFFSET, 10, group);
        }
        return image;
    }

    private static byte[] synthesiseCategoryBalances(byte[] accountImage) {
        List<String> accountIds = new ArrayList<>();
        for (int offset = 0; offset < accountImage.length; offset += ACCOUNT_LENGTH) {
            accountIds.add(new String(accountImage, offset, 11, CP037));
        }
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        int sequence = 0;
        for (String accountId : accountIds) {
            for (String category : CATEGORIES) {
                byte[] record = new byte[TCATBAL_LENGTH];
                Arrays.fill(record, (byte) 0x40);
                putText(record, 0, 11, accountId);
                putText(record, 11, 2, "01");
                putText(record, 13, 4, category);
                putZoned(record, 17, 11, 2, new BigDecimal(BALANCES.get(sequence % BALANCES.size())));
                putText(record, 28, 22, "");
                out.writeBytes(record);
                sequence++;
            }
        }
        return out.toByteArray();
    }

    private static void putText(byte[] record, int offset, int length, String value) {
        String text = value.length() > length ? value.substring(0, length) : value;
        byte[] bytes = (text + " ".repeat(length - text.length())).getBytes(CP037);
        System.arraycopy(bytes, 0, record, offset, length);
    }

    private static void putZoned(byte[] record, int offset, int digits, int scale, BigDecimal value) {
        String plain = value.abs().movePointRight(scale).toBigInteger().toString();
        String padded = "0".repeat(Math.max(0, digits - plain.length())) + plain;
        String overpunch = value.signum() < 0 ? "}JKLMNOPQR" : "{ABCDEFGHI";
        String encoded = padded.substring(0, digits - 1)
                + overpunch.charAt(padded.charAt(digits - 1) - '0');
        System.arraycopy(encoded.getBytes(CP037), 0, record, offset, digits);
    }
}
