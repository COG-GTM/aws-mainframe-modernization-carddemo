package com.carddemo.transaction.batch;

import com.carddemo.common.record.CobolRecord;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * Stages DALYREJS images in {@code batch.posttran_reject} inside the record's transaction (Restart state, BND-003),
 * so a committed reject survives a failure and a restart of the same JobInstance.
 */
@Component
public class RejectStore {

    private final PosttranRejectRepository repository;

    public RejectStore(PosttranRejectRepository repository) {
        this.repository = repository;
    }

    @Transactional(propagation = Propagation.REQUIRED)
    public void stage(long jobInstanceId, DailyTransaction transaction, int reason, String description) {
        CobolRecord reject = RejectRecordLayout.of(transaction.record(), reason, description);
        repository.save(new PosttranReject(jobInstanceId, transaction.inputSeq(), reject.bytes()));
    }

    @Transactional(readOnly = true)
    public List<byte[]> images(long jobInstanceId) {
        return repository.findByJobInstanceIdOrderByInputSeq(jobInstanceId).stream().map(PosttranReject::getRecord).toList();
    }
}
