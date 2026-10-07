package archfixture.web;

import archfixture.account.AccountService;

/** Legal: the web layer may use domain services. */
public class LoginEndpoint {
    public AccountService accounts;
}
