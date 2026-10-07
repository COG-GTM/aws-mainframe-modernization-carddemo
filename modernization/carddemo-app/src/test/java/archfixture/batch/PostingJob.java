package archfixture.batch;

import archfixture.account.AccountCallsBatch;
import org.springframework.batch.core.Job;

/** Legal on its own: batch may use every domain and Spring Batch. */
public class PostingJob {
    private Job job;
    private AccountCallsBatch account;

    public void run() {
        account = null;
        job = null;
    }
}
