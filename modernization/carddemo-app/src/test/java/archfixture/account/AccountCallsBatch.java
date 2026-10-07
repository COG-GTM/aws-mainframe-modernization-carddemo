package archfixture.account;

import archfixture.batch.PostingJob;

/** Illegal: no domain may depend on batch (and this closes an account <-> batch cycle). */
public class AccountCallsBatch {
    public void post() {
        new PostingJob().run();
    }
}
