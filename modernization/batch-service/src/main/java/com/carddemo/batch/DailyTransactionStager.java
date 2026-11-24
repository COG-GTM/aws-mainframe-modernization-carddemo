package com.carddemo.batch;

import com.carddemo.common.domain.DailyTransaction;
import com.carddemo.common.domain.DailyTransactionRepository;
import com.carddemo.migration.CopybookLayouts;
import com.carddemo.migration.RecordDecoder;
import com.carddemo.migration.RecordReader;
import com.carddemo.migration.SourceEncoding;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Map;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Accepts a DALYTRAN file (350-byte CVTRA06Y records, EBCDIC or ASCII) and stages it for POSTTRAN. In AWS this is
 * triggered by an S3 PUT (see docs/modernization/05-batch.md); the record decoding is shared with the VSAM loader.
 */
@Service
public class DailyTransactionStager {

    private final DailyTransactionRepository daily;

    public DailyTransactionStager(DailyTransactionRepository daily) {
        this.daily = daily;
    }

    @Transactional
    public int stage(InputStream in, SourceEncoding encoding) throws IOException {
        int count = 0;
        for (byte[] rec : RecordReader.read(in, CopybookLayouts.DAILY_TRANSACTION.recordLength(), encoding)) {
            Map<String, Object> row = RecordDecoder.decode(rec, CopybookLayouts.DAILY_TRANSACTION, encoding);
            DailyTransaction t = new DailyTransaction();
            t.setTranId((String) row.get("tran_id"));
            t.setTypeCd((String) row.get("type_cd"));
            t.setCatCd(((Number) row.get("cat_cd")).intValue());
            t.setSource((String) row.get("source"));
            t.setDescription((String) row.get("description"));
            t.setAmount((BigDecimal) row.get("amount"));
            t.setMerchantId(((Number) row.get("merchant_id")).longValue());
            t.setMerchantName((String) row.get("merchant_name"));
            t.setMerchantCity((String) row.get("merchant_city"));
            t.setMerchantZip((String) row.get("merchant_zip"));
            t.setCardNum((String) row.get("card_num"));
            t.setOrigTs((LocalDateTime) row.get("orig_ts"));
            t.setProcTs((LocalDateTime) row.get("proc_ts"));
            t.setStatus(PostTransactionsService.PENDING);
            daily.save(t);
            count++;
        }
        return count;
    }
}
