package com.carddemo.posting.repository;

import com.carddemo.posting.domain.CardNumber;
import com.carddemo.posting.domain.CardXref;
import com.carddemo.posting.io.codec.CardXrefCodec;
import com.carddemo.mainframe.io.FixedLengthRecordReader;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

/** Keyed in-memory view of an {@code XREFFILE} dataset image. */
public final class InMemoryCardXrefRepository implements CardXrefRepository {

    private final Map<CardNumber, CardXref> byCardNumber = new LinkedHashMap<>();

    public InMemoryCardXrefRepository(byte[] datasetImage, String datasetName) {
        FixedLengthRecordReader.split(datasetImage, CardXrefCodec.recordLength(), datasetName)
                .forEach(record -> {
                    CardXref xref = CardXrefCodec.decode(record);
                    byCardNumber.putIfAbsent(xref.cardNumber(), xref);
                });
    }

    @Override
    public Optional<CardXref> findByCardNumber(CardNumber cardNumber) {
        return Optional.ofNullable(byCardNumber.get(cardNumber));
    }
}
