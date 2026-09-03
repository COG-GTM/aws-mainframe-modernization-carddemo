package com.carddemo.posting.repository;

import com.carddemo.mainframe.io.EbcdicText;
import com.carddemo.mainframe.io.FixedLengthRecordReader;
import com.carddemo.posting.domain.TransactionCategoryBalance;
import com.carddemo.posting.domain.TransactionCategoryKey;
import com.carddemo.posting.io.codec.TransactionCategoryBalanceCodec;

import java.io.ByteArrayOutputStream;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;

/**
 * Keyed in-memory view of a {@code TCATBALF} dataset image, with the COBOL record-area semantics
 * the file's byte layout depends on.
 *
 * <p>The program reads into a single working-storage record area
 * ({@code READ TCATBAL-FILE INTO TRAN-CAT-BAL-RECORD}, {@code app/cbl/CBTRN02C.cbl:474}) and
 * writes back out of it. On a missing key nothing is moved into the area, and the subsequent
 * {@code INITIALIZE} ({@code app/cbl/CBTRN02C.cbl:504}) does not touch {@code FILLER}, so a
 * created record inherits the 22 filler bytes of the last record read. That is reproduced here by
 * carrying the record area rather than by encoding new records over blanks — in the shipped
 * dataset the filler is 22 EBCDIC zeros, not spaces, so the difference is visible on every created
 * record.
 */
public final class InMemoryTransactionCategoryBalanceRepository
        implements TransactionCategoryBalanceRepository {

    private final Map<TransactionCategoryKey, byte[]> imagesByKey = new HashMap<>();
    private final Set<TransactionCategoryKey> originalKeys;
    private byte[] recordArea;

    public InMemoryTransactionCategoryBalanceRepository(byte[] datasetImage, String datasetName) {
        this.recordArea = EbcdicText.blankRecord(TransactionCategoryBalanceCodec.recordLength());
        FixedLengthRecordReader
                .split(datasetImage, TransactionCategoryBalanceCodec.recordLength(), datasetName)
                .forEach(record -> imagesByKey.putIfAbsent(
                        TransactionCategoryBalanceCodec.decode(record).key(), record));
        this.originalKeys = Set.copyOf(imagesByKey.keySet());
    }

    @Override
    public Optional<TransactionCategoryBalance> findByKey(TransactionCategoryKey key) {
        byte[] image = imagesByKey.get(key);
        if (image == null) {
            return Optional.empty();
        }
        recordArea = image.clone();
        return Optional.of(TransactionCategoryBalanceCodec.decode(image));
    }

    @Override
    public void save(TransactionCategoryBalance balance) {
        byte[] written = TransactionCategoryBalanceCodec.encodeInto(recordArea, balance);
        imagesByKey.put(balance.key(), written);
        recordArea = written.clone();
    }

    /** How many buckets this run had to create. */
    public int createdCount() {
        return imagesByKey.size() - originalKeys.size();
    }

    /**
     * The file as it stands after the run, in ascending key order — the order in which a VSAM KSDS
     * returns its records, and therefore the order an unload of the file would produce.
     */
    public byte[] datasetImage() {
        Map<TransactionCategoryKey, byte[]> ordered =
                new TreeMap<>(Comparator.comparing(TransactionCategoryKey::keyText));
        ordered.putAll(imagesByKey);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        ordered.values().forEach(out::writeBytes);
        return out.toByteArray();
    }
}
