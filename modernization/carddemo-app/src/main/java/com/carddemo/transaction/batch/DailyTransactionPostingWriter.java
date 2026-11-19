package com.carddemo.transaction.batch;

import com.carddemo.common.batch.SysoutWriter;
import com.carddemo.common.cobol.Db2Timestamp;
import com.carddemo.transaction.domain.DailyReject;
import com.carddemo.transaction.service.DailyTransactionPostingService;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;

import java.time.Clock;
import java.time.LocalDateTime;

/**
 * Routes each validated record (cbl:211-216): valid → {@link DailyTransactionPostingService#post} with
 * {@code TRAN-PROC-TS} from the job clock; invalid → one DALYREJS record ({@code 2500-WRITE-REJECT-REC}). No business rule
 * lives here (D-018).
 */
public class DailyTransactionPostingWriter implements ItemWriter<PostingItem> {

    private final DailyTransactionPostingService posting;
    private final ItemWriter<DailyReject> rejects;
    private final PosttranStepReport report;
    private final SysoutWriter sysout;
    private final Clock clock;

    public DailyTransactionPostingWriter(DailyTransactionPostingService posting, ItemWriter<DailyReject> rejects,
                                         PosttranStepReport report, SysoutWriter sysout, Clock clock) {
        this.posting = posting;
        this.rejects = rejects;
        this.report = report;
        this.sysout = sysout;
        this.clock = clock;
    }

    @Override
    public void write(Chunk<? extends PostingItem> chunk) throws Exception {
        for (PostingItem item : chunk) {
            if (item.validation().isValid()) {
                posting.post(item.transaction(), item.validation().xrefAcctId(), Db2Timestamp.format(LocalDateTime.now(clock)),
                        sysout);
                report.count(false);
            } else {
                rejects.write(Chunk.of(new DailyReject(item.transaction(), item.validation().reasonCode(),
                        item.validation().reasonDesc())));
                report.count(true);
            }
        }
    }
}
