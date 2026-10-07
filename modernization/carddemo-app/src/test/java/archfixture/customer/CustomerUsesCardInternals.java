package archfixture.customer;

import archfixture.card.internal.CardStore;

/** Illegal twice: customer may not use card, and card.internal is private. */
public class CustomerUsesCardInternals {
    public String peek() {
        return new CardStore().load();
    }
}
