package com.carddemo.account.online;

import com.carddemo.account.Account;
import com.carddemo.customer.Customer;
import java.util.List;

/**
 * Result of {@code 9000-READ-ACCT}: the account, its customer and the cards on the account.
 *
 * @param cardNum     {@code CDEMO-CARD-NUM}: the card of the {@code CXACAIX} record read (lowest card number)
 * @param cardNumbers every card number of the account in the alternate index, ascending
 */
public record AccountDetails(Account account, Customer customer, String cardNum, List<String> cardNumbers) {

    public AccountDetails {
        cardNumbers = List.copyOf(cardNumbers);
    }
}
