package com.carddemo.batch.intcalc;

import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import com.carddemo.common.file.RecordFiles;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * CARDXREF opened {@code INPUT} and read by its alternate key XREF-ACCT-ID (CXACAIX, {@code XREFFIL1} in the JCL) from
 * a KSDS unload file: the first record of each account in primary-key (file) order, the one the AIX returns first and
 * the one {@code CardXrefRepository.findFirstByAcctIdOrderByCardNumAsc} returns in table mode.
 */
public final class XrefByAccount implements KeyedDataset<Long, CardXrefRecord> {

    private final String ddname;
    private final Path path;
    private final RecordEncoding encoding;
    private final Map<Long, CardXrefRecord> byAccount = new HashMap<>();
    private boolean open;

    public XrefByAccount(String ddname, Path path, RecordEncoding encoding) {
        this.ddname = ddname;
        this.path = path;
        this.encoding = encoding;
    }

    @Override
    public String ddname() {
        return ddname;
    }

    @Override
    public void open() {
        if (open) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.ALREADY_OPEN);
        }
        List<FixedWidthRecord> all = encoding == RecordEncoding.EBCDIC
                ? RecordFiles.readFixed(ddname, path, CardXrefRecord.MAPPER.layout(), encoding)
                : RecordFiles.readLines(ddname, path, CardXrefRecord.MAPPER.layout(), encoding);
        byAccount.clear();
        for (FixedWidthRecord r : all) {
            if (!CardXrefRecord.MAPPER.isLowValues(r)) {
                CardXrefRecord xref = CardXrefRecord.MAPPER.fromRecord(r);
                byAccount.putIfAbsent(xref.acctId(), xref);
            }
        }
        open = true;
    }

    @Override
    public Optional<CardXrefRecord> read(Long acctId) {
        if (!open) {
            throw new FileStatusException(ddname, "READ", FileStatus.NOT_OPEN);
        }
        return Optional.ofNullable(byAccount.get(acctId));
    }

    @Override
    public void write(CardXrefRecord data) {
        throw new FileStatusException(ddname, "WRITE", FileStatus.NOT_OPEN_OUTPUT);
    }

    @Override
    public boolean rewrite(CardXrefRecord data) {
        throw new FileStatusException(ddname, "REWRITE", FileStatus.NOT_OPEN_IO);
    }

    @Override
    public void close() {
        if (!open) {
            throw new FileStatusException(ddname, "CLOSE", FileStatus.NOT_OPEN);
        }
        open = false;
    }
}
