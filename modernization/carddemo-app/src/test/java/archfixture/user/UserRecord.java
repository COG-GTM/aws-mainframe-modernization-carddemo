package archfixture.user;

import org.springframework.batch.core.Job;

/** Illegal: Spring Batch API outside the batch domain. */
public class UserRecord {
    public Job job;
}
