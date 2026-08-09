package com.carddemo.interest.batch;

import com.carddemo.interest.domain.Account;
import com.carddemo.interest.domain.CardXref;
import com.carddemo.interest.domain.DisclosureGroup;
import com.carddemo.interest.domain.TransactionCategoryBalance;
import com.carddemo.mainframe.io.FixedLengthRecordReader;
import com.carddemo.interest.io.codec.AccountCodec;
import com.carddemo.interest.io.codec.CardXrefCodec;
import com.carddemo.interest.io.codec.DisclosureGroupCodec;
import com.carddemo.interest.io.codec.TransactionCategoryBalanceCodec;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * The four EBCDIC input datasets of the {@code INTCALC} job, held as raw dataset images.
 *
 * <p>DD-name to dataset mapping, from {@code app/jcl/INTCALC.jcl:27-41}:
 * <ul>
 *   <li>{@code TCATBALF} &rarr; {@code AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS} (static dump
 *       {@code AWS.M2.CARDDEMO.TCATBALF.PS})</li>
 *   <li>{@code XREFFILE} / {@code XREFFIL1} &rarr; {@code AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS} and
 *       its account-id alternate-index path (static dump {@code AWS.M2.CARDDEMO.CARDXREF.PS})</li>
 *   <li>{@code ACCTFILE} &rarr; {@code AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS} (static dump
 *       {@code AWS.M2.CARDDEMO.ACCTDATA.PS})</li>
 *   <li>{@code DISCGRP} &rarr; {@code AWS.M2.CARDDEMO.DISCGRP.VSAM.KSDS} (static dump
 *       {@code AWS.M2.CARDDEMO.DISCGRP.PS})</li>
 * </ul>
 *
 * <p>Working from dataset images rather than file handles keeps the batch testable: the parity
 * harness can feed derived populations through exactly the same code path as the shipped dumps.
 */
public final class InterestDatasets {

    /** File names of the static EBCDIC dumps under {@code app/data/EBCDIC}. */
    public static final String TCATBALF_DATASET = "AWS.M2.CARDDEMO.TCATBALF.PS";
    public static final String XREFFILE_DATASET = "AWS.M2.CARDDEMO.CARDXREF.PS";
    public static final String ACCTFILE_DATASET = "AWS.M2.CARDDEMO.ACCTDATA.PS";
    public static final String DISCGRP_DATASET = "AWS.M2.CARDDEMO.DISCGRP.PS";

    private final byte[] transactionCategoryBalanceImage;
    private final byte[] cardXrefImage;
    private final byte[] accountImage;
    private final byte[] disclosureGroupImage;

    public InterestDatasets(byte[] transactionCategoryBalanceImage,
                            byte[] cardXrefImage,
                            byte[] accountImage,
                            byte[] disclosureGroupImage) {
        this.transactionCategoryBalanceImage = transactionCategoryBalanceImage.clone();
        this.cardXrefImage = cardXrefImage.clone();
        this.accountImage = accountImage.clone();
        this.disclosureGroupImage = disclosureGroupImage.clone();
    }

    /** Loads the four dataset dumps from a directory such as {@code app/data/EBCDIC}. */
    public static InterestDatasets fromDirectory(Path directory) {
        return new InterestDatasets(
                read(directory.resolve(TCATBALF_DATASET)),
                read(directory.resolve(XREFFILE_DATASET)),
                read(directory.resolve(ACCTFILE_DATASET)),
                read(directory.resolve(DISCGRP_DATASET)));
    }

    /** {@code TCATBALF} — transaction category balances, in ascending key order. */
    public List<TransactionCategoryBalance> transactionCategoryBalances() {
        return FixedLengthRecordReader
                .split(transactionCategoryBalanceImage, TransactionCategoryBalanceCodec.recordLength(), TCATBALF_DATASET)
                .stream().map(TransactionCategoryBalanceCodec::decode).toList();
    }

    /** {@code XREFFILE} — card cross-references. */
    public List<CardXref> cardXrefs() {
        return FixedLengthRecordReader
                .split(cardXrefImage, CardXrefCodec.recordLength(), XREFFILE_DATASET)
                .stream().map(CardXrefCodec::decode).toList();
    }

    /** {@code ACCTFILE} — account master. */
    public List<Account> accounts() {
        return FixedLengthRecordReader
                .split(accountImage, AccountCodec.recordLength(), ACCTFILE_DATASET)
                .stream().map(AccountCodec::decode).toList();
    }

    /** {@code DISCGRP} — disclosed interest rates. */
    public List<DisclosureGroup> disclosureGroups() {
        return FixedLengthRecordReader
                .split(disclosureGroupImage, DisclosureGroupCodec.recordLength(), DISCGRP_DATASET)
                .stream().map(DisclosureGroupCodec::decode).toList();
    }

    /** Raw {@code TCATBALF} dataset image, for byte-level tooling such as the parity harness. */
    public byte[] rawTransactionCategoryBalanceImage() {
        return transactionCategoryBalanceImage.clone();
    }

    /** Raw {@code XREFFILE} dataset image. */
    public byte[] rawCardXrefImage() {
        return cardXrefImage.clone();
    }

    /** Raw {@code ACCTFILE} dataset image. */
    public byte[] rawAccountImage() {
        return accountImage.clone();
    }

    /** Raw {@code DISCGRP} dataset image. */
    public byte[] rawDisclosureGroupImage() {
        return disclosureGroupImage.clone();
    }

    private static byte[] read(Path dataset) {
        try {
            return Files.readAllBytes(dataset);
        } catch (IOException e) {
            throw new UncheckedIOException("Cannot read dataset " + dataset, e);
        }
    }
}
