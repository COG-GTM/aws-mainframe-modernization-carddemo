package com.carddemo.batch.posttran.batch;

import com.carddemo.batch.posttran.logic.PostingService;
import com.carddemo.batch.posttran.logic.ValidationResult;
import com.carddemo.batch.posttran.record.DailyTransaction;
import com.carddemo.batch.posttran.record.RejectRecord;
import org.springframework.batch.infrastructure.item.Chunk;
import org.springframework.batch.infrastructure.item.ItemWriter;

/**
 * Body of the main PERFORM UNTIL loop. Records are validated and posted one at a time, in input
 * order, so each validation sees the balances left by the previous posting, as in the COBOL.
 */
public class PostingItemWriter implements ItemWriter<DailyTransaction> {

    private final PostingService postingService;
    private final RejectFileWriter rejects;
    private final PostTranCounters counters;

    public PostingItemWriter(PostingService postingService, RejectFileWriter rejects, PostTranCounters counters) {
        this.postingService = postingService;
        this.rejects = rejects;
        this.counters = counters;
    }

    @Override
    public void write(Chunk<? extends DailyTransaction> chunk) {
        for (DailyTransaction tran : chunk) {
            counters.countTransaction();
            ValidationResult result = postingService.validate(tran);
            if (result.valid()) {
                postingService.post(tran, result);
            } else {
                counters.countReject();
                rejects.write(RejectRecord.format(tran, result.reasonCode(), result.description()));
            }
        }
    }
}
