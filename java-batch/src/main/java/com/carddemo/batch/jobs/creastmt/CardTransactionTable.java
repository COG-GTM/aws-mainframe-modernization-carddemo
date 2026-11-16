package com.carddemo.batch.jobs.creastmt;

import com.carddemo.batch.model.StatementTransactionRecord;

import java.util.ArrayList;
import java.util.List;

/**
 * WS-TRNX-TABLE: transactions grouped by card in TRNXFILE order, loaded once up front
 * (8500-READTRNX-READ). Bounded like the COBOL table: 51 cards x 10 transactions;
 * exceeding either is an error here instead of a storage overlay.
 */
public class CardTransactionTable {

    static final int MAX_CARDS = 51;
    static final int MAX_TRANS_PER_CARD = 10;

    private final List<String> cardNums = new ArrayList<>();
    private final List<List<StatementTransactionRecord>> trans = new ArrayList<>();

    public CardTransactionTable(List<StatementTransactionRecord> trnxInKeyOrder) {
        String saveCard = null;
        for (StatementTransactionRecord t : trnxInKeyOrder) {
            String card = t.trnxKey().trnxCardNum();
            if (!card.equals(saveCard)) {
                if (cardNums.size() == MAX_CARDS) {
                    throw new IllegalStateException("WS-CARD-TBL overflow: more than " + MAX_CARDS + " cards");
                }
                cardNums.add(card);
                trans.add(new ArrayList<>());
                saveCard = card;
            }
            List<StatementTransactionRecord> forCard = trans.get(trans.size() - 1);
            if (forCard.size() == MAX_TRANS_PER_CARD) {
                throw new IllegalStateException("WS-TRAN-TBL overflow: card " + card + " has more than "
                        + MAX_TRANS_PER_CARD + " transactions");
            }
            forCard.add(t);
        }
    }

    /**
     * 4000-TRNXFILE-GET: scans the table in order, stopping once a card number sorts above
     * {@code cardNum}, and returns that card's transactions.
     */
    public List<StatementTransactionRecord> forCard(String cardNum) {
        List<StatementTransactionRecord> found = new ArrayList<>();
        for (int i = 0; i < cardNums.size() && cardNums.get(i).compareTo(cardNum) <= 0; i++) {
            if (cardNums.get(i).equals(cardNum)) {
                found.addAll(trans.get(i));
            }
        }
        return found;
    }
}
