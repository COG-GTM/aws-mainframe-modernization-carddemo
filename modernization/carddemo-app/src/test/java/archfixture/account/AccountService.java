package archfixture.account;

import archfixture.card.CardApi;

/** Legal: account may use card's public API. */
public class AccountService {
    private final CardApi cards = new CardApi();

    public String view(long accountId) {
        return cards.cardFor(accountId);
    }
}
