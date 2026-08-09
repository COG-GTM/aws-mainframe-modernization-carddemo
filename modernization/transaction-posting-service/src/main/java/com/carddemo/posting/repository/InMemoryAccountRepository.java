package com.carddemo.posting.repository;

import com.carddemo.mainframe.io.FixedLengthRecordReader;
import com.carddemo.posting.domain.Account;
import com.carddemo.posting.domain.AccountId;
import com.carddemo.posting.exception.DatasetIntegrityException;
import com.carddemo.posting.io.codec.AccountCodec;

import java.io.ByteArrayOutputStream;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

/**
 * Keyed in-memory view of an {@code ACCTFILE} dataset image that keeps the raw record for every
 * account.
 *
 * <p>Keeping the images, rather than re-encoding accounts from scratch, is what makes
 * {@link #save(Account)} behave like the COBOL {@code REWRITE}: only the fields the copybook
 * describes are replaced, and the record keeps its position in the file.
 */
public final class InMemoryAccountRepository implements AccountRepository {

    private final Map<AccountId, byte[]> imagesById = new LinkedHashMap<>();

    public InMemoryAccountRepository(byte[] datasetImage, String datasetName) {
        FixedLengthRecordReader.split(datasetImage, AccountCodec.recordLength(), datasetName)
                .forEach(record -> imagesById.putIfAbsent(AccountCodec.decode(record).id(), record));
    }

    @Override
    public Optional<Account> findById(AccountId accountId) {
        return Optional.ofNullable(imagesById.get(accountId)).map(AccountCodec::decode);
    }

    @Override
    public void save(Account account) {
        byte[] image = imagesById.get(account.id());
        if (image == null) {
            throw new DatasetIntegrityException(
                    "Cannot rewrite account " + account.id() + ": no such record in ACCTFILE");
        }
        imagesById.put(account.id(), AccountCodec.encodeInto(image, account));
    }

    /** The account master as it stands after the run, in its original (key) order. */
    public byte[] datasetImage() {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        imagesById.values().forEach(out::writeBytes);
        return out.toByteArray();
    }
}
