package archfixture.card;

public class CardApi {
    public String cardFor(long accountId) {
        return "4111" + accountId;
    }
}
