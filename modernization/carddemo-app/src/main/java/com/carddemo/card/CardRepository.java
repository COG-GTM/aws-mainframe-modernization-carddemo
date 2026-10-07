package com.carddemo.card;

import com.carddemo.common.data.KeysetPage;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Limit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

/**
 * CARDDATA access paths: {@code READ} by CARD-NUM (COCRDSLC, COCRDUPC, CBTRN01C), {@code REWRITE} (COCRDUPC), the
 * CARDAIX read by CARD-ACCT-ID (COCRDSLC), the sequential read in key order (CBACT02C, CBEXPORT) and the COCRDLIC
 * browse: card-number order, seven cards per screen, optionally filtered by account (ADR-0011). The account filter
 * uses index {@code card_acct_id_ix (acct_id, card_num)}, which returns the same cards in the same order as
 * COCRDLIC's browse of the primary key with {@code 9500-FILTER-RECORDS}.
 */
public interface CardRepository extends JpaRepository<Card, String> {

    /** COCRDLIC {@code WS-MAX-SCREEN-LINES VALUE 7}. */
    int COCRDLIC_SCREEN_ROWS = 7;

    List<Card> findAllByOrderByCardNumAsc();

    /**
     * COCRDUPC {@code READ FILE('CARDDAT') UPDATE}: locks the row until the transaction ends and returns the version
     * the {@code REWRITE} must still match (ADR-0010).
     */
    @Query(value = "select version from card where card_num = :cardNum for update", nativeQuery = true)
    Optional<Long> lockVersion(@Param("cardNum") String cardNum);

    /**
     * CARDAIX {@code READ} by CARD-ACCT-ID: the first card of the account. The non-unique AIX is built by BLDINDEX
     * from the base cluster in key order, so "first" is the lowest card number.
     */
    Optional<Card> findFirstByAcctIdOrderByCardNumAsc(long acctId);

    List<Card> findByCardNumGreaterThanEqualOrderByCardNumAsc(String startCardNum, Limit limit);

    List<Card> findByCardNumGreaterThanOrderByCardNumAsc(String lastCardNumShown, Limit limit);

    List<Card> findByCardNumLessThanOrderByCardNumDesc(String firstCardNumShown, Limit limit);

    List<Card> findByAcctIdAndCardNumGreaterThanEqualOrderByCardNumAsc(long acctId, String startCardNum, Limit limit);

    List<Card> findByAcctIdAndCardNumGreaterThanOrderByCardNumAsc(long acctId, String lastCardNumShown, Limit limit);

    List<Card> findByAcctIdAndCardNumLessThanOrderByCardNumDesc(long acctId, String firstCardNumShown, Limit limit);

    /** ENTER / first display: {@code STARTBR GTEQ} at {@code startCardNum} ({@code ""} = LOW-VALUES). */
    default KeysetPage<Card> browseFrom(String startCardNum) {
        return KeysetPage.forward(l -> findByCardNumGreaterThanEqualOrderByCardNumAsc(startCardNum, l),
                COCRDLIC_SCREEN_ROWS);
    }

    /** PF8 from the last card shown. */
    default KeysetPage<Card> nextPage(String lastCardNumShown) {
        return KeysetPage.forward(l -> findByCardNumGreaterThanOrderByCardNumAsc(lastCardNumShown, l),
                COCRDLIC_SCREEN_ROWS);
    }

    /** PF7 from the first card shown. */
    default KeysetPage<Card> previousPage(String firstCardNumShown) {
        return KeysetPage.backward(l -> findByCardNumLessThanOrderByCardNumDesc(firstCardNumShown, l),
                COCRDLIC_SCREEN_ROWS);
    }

    default KeysetPage<Card> browseFrom(long acctId, String startCardNum) {
        return KeysetPage.forward(l -> findByAcctIdAndCardNumGreaterThanEqualOrderByCardNumAsc(acctId, startCardNum,
                l), COCRDLIC_SCREEN_ROWS);
    }

    default KeysetPage<Card> nextPage(long acctId, String lastCardNumShown) {
        return KeysetPage.forward(l -> findByAcctIdAndCardNumGreaterThanOrderByCardNumAsc(acctId, lastCardNumShown,
                l), COCRDLIC_SCREEN_ROWS);
    }

    default KeysetPage<Card> previousPage(long acctId, String firstCardNumShown) {
        return KeysetPage.backward(l -> findByAcctIdAndCardNumLessThanOrderByCardNumDesc(acctId, firstCardNumShown,
                l), COCRDLIC_SCREEN_ROWS);
    }
}
