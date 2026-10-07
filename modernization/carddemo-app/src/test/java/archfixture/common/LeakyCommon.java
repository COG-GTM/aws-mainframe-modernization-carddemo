package archfixture.common;

import archfixture.user.UserRecord;

/** Illegal: the shared kernel may not depend on a domain. */
public class LeakyCommon {
    public UserRecord user;
}
