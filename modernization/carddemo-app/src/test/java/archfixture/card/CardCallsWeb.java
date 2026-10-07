package archfixture.card;

import archfixture.web.LoginEndpoint;

/** Illegal: a domain depending on the web layer. */
public class CardCallsWeb {
    public LoginEndpoint endpoint;
}
