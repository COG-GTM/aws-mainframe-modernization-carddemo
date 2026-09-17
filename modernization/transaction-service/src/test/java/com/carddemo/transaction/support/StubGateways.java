package com.carddemo.transaction.support;

import com.carddemo.transaction.client.AccountGateway;
import com.carddemo.transaction.client.AccountView;
import com.carddemo.transaction.client.CardGateway;
import com.carddemo.transaction.client.CardXrefView;
import com.carddemo.transaction.client.PostingResult;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;

/** In-memory stand ins for account-service and card-service. */
@TestConfiguration
public class StubGateways {

    @Bean
    @Primary
    public StubCardGateway stubCardGateway() {
        return new StubCardGateway();
    }

    @Bean
    @Primary
    public StubAccountGateway stubAccountGateway() {
        return new StubAccountGateway();
    }

    public static class StubCardGateway implements CardGateway {

        private final Map<String, CardXrefView> byCard = new HashMap<>();

        public void register(String cardNumber, long customerId, long accountId) {
            byCard.put(cardNumber, new CardXrefView(cardNumber, customerId, accountId));
        }

        public void clear() {
            byCard.clear();
        }

        @Override
        public Optional<CardXrefView> xrefByCard(String cardNumber) {
            return Optional.ofNullable(byCard.get(cardNumber));
        }

        @Override
        public Optional<CardXrefView> xrefByAccount(long accountId) {
            return byCard.values().stream().filter(xref -> xref.accountId() == accountId).findFirst();
        }
    }

    public static class StubAccountGateway implements AccountGateway {

        private final Map<Long, AccountView> accounts = new HashMap<>();
        private final Map<Long, PostingResult> forcedResults = new HashMap<>();
        private final List<BigDecimal> settlements = new ArrayList<>();
        private final List<BigDecimal> postings = new ArrayList<>();

        public void register(long accountId, String groupId, BigDecimal balance) {
            accounts.put(accountId, new AccountView(accountId, "Y", balance, groupId));
        }

        public void forceResult(long accountId, PostingResult result) {
            forcedResults.put(accountId, result);
        }

        public List<BigDecimal> settlements() {
            return settlements;
        }

        public List<BigDecimal> postings() {
            return postings;
        }

        public void clear() {
            accounts.clear();
            forcedResults.clear();
            settlements.clear();
            postings.clear();
        }

        @Override
        public Optional<AccountView> find(long accountId) {
            return Optional.ofNullable(accounts.get(accountId));
        }

        @Override
        public PostingResult post(long accountId, BigDecimal amount, LocalDate transactionDate) {
            postings.add(amount);
            if (forcedResults.containsKey(accountId)) {
                return forcedResults.get(accountId);
            }
            AccountView account = accounts.get(accountId);
            if (account == null) {
                return null;
            }
            BigDecimal balance = account.currentBalance().add(amount);
            accounts.put(accountId, new AccountView(accountId, account.activeStatus(), balance,
                    account.groupId()));
            return new PostingResult(true, null, null, balance, BigDecimal.ZERO, BigDecimal.ZERO);
        }

        @Override
        public void settleInterest(long accountId, BigDecimal totalInterest) {
            settlements.add(totalInterest);
        }
    }
}
