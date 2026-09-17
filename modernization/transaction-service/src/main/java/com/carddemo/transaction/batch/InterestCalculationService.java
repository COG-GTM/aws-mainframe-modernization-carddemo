package com.carddemo.transaction.batch;

import com.carddemo.transaction.client.AccountGateway;
import com.carddemo.transaction.client.AccountView;
import com.carddemo.transaction.client.CardGateway;
import com.carddemo.transaction.client.CardXrefView;
import com.carddemo.transaction.domain.CategoryBalance;
import com.carddemo.transaction.domain.DisclosureGroup;
import com.carddemo.transaction.domain.DisclosureGroupId;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.repository.CategoryBalanceRepository;
import com.carddemo.transaction.repository.DisclosureGroupRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import com.carddemo.transaction.service.TransactionIdGenerator;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Port of CBACT04C (JCL INTCALC). TCATBAL is read in account order; for every category balance
 * the monthly interest is
 *
 * <pre>COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200</pre>
 *
 * and an interest transaction (type 01, category 05, source System) is written. At the account
 * break the accumulated interest is added to the balance and the cycle totals are reset.
 *
 * <p>Paragraph 1200-A-GET-INTEREST-RATE falls back to the DEFAULT disclosure group when the
 * account group has no row, which is reproduced here. Paragraph 1400-COMPUTE-FEES is empty in
 * the COBOL, so no fee logic is invented.
 */
@Service
public class InterestCalculationService {

    private static final String INTEREST_TYPE_CD = "01";
    private static final int INTEREST_CAT_CD = 5;
    private static final BigDecimal MONTHS_TIMES_PERCENT = new BigDecimal("1200");

    private final CategoryBalanceRepository categoryBalances;
    private final DisclosureGroupRepository disclosureGroups;
    private final TransactionRepository transactions;
    private final TransactionIdGenerator idGenerator;
    private final AccountGateway accounts;
    private final CardGateway cards;
    private final Clock clock;

    public InterestCalculationService(CategoryBalanceRepository categoryBalances,
                                      DisclosureGroupRepository disclosureGroups,
                                      TransactionRepository transactions,
                                      TransactionIdGenerator idGenerator,
                                      AccountGateway accounts,
                                      CardGateway cards,
                                      Clock clock) {
        this.categoryBalances = categoryBalances;
        this.disclosureGroups = disclosureGroups;
        this.transactions = transactions;
        this.idGenerator = idGenerator;
        this.accounts = accounts;
        this.cards = cards;
        this.clock = clock;
    }

    @Transactional
    public InterestReport calculateInterest() {
        Map<Long, BigDecimal> interestByAccount = new LinkedHashMap<>();
        List<CategoryBalance> balances = categoryBalances.findAllByOrderByIdAcctIdAscIdTypeCdAscIdCatCdAsc();

        for (CategoryBalance balance : balances) {
            Long accountId = balance.getId().getAcctId();
            Optional<AccountView> account = accounts.find(accountId);
            if (account.isEmpty()) {
                continue;
            }
            BigDecimal rate = interestRate(account.get().groupId(), balance);
            if (rate.signum() == 0) {
                continue;
            }
            BigDecimal monthlyInterest = balance.getBalance()
                    .multiply(rate)
                    .divide(MONTHS_TIMES_PERCENT, 2, RoundingMode.HALF_UP);
            writeInterestTransaction(accountId, monthlyInterest);
            interestByAccount.merge(accountId, monthlyInterest, BigDecimal::add);
        }

        interestByAccount.forEach(accounts::settleInterest);
        balances.forEach(CategoryBalance::reset);
        categoryBalances.saveAll(balances);
        return new InterestReport(interestByAccount.size(), interestByAccount.values().stream()
                .reduce(BigDecimal.ZERO, BigDecimal::add));
    }

    /** Paragraphs 1200-GET-INTEREST-RATE and 1200-A-GET-DEFAULT-INT-RATE. */
    private BigDecimal interestRate(String accountGroupId, CategoryBalance balance) {
        String typeCd = balance.getId().getTypeCd();
        Integer catCd = balance.getId().getCatCd();
        return disclosureGroups.findById(new DisclosureGroupId(accountGroupId, typeCd, catCd))
                .or(() -> disclosureGroups.findById(
                        new DisclosureGroupId(DisclosureGroup.DEFAULT_GROUP_ID, typeCd, catCd)))
                .map(DisclosureGroup::getInterestRate)
                .orElse(BigDecimal.ZERO);
    }

    /** Paragraph 1300-WRITE-TRANSACTION-FILE. */
    private void writeInterestTransaction(Long accountId, BigDecimal monthlyInterest) {
        String cardNumber = cards.xrefByAccount(accountId)
                .map(CardXrefView::cardNumber)
                .orElse(null);
        if (cardNumber == null) {
            return;
        }
        LocalDateTime now = LocalDateTime.now(clock);
        Transaction transaction = new Transaction(idGenerator.next(), cardNumber,
                INTEREST_TYPE_CD, INTEREST_CAT_CD, monthlyInterest);
        transaction.setSource("System");
        transaction.setDescription("Int. for a/c " + accountId);
        transaction.setMerchantId(0L);
        transaction.setMerchantName("Interest calculation");
        transaction.setMerchantCity("N/A");
        transaction.setMerchantZip("N/A");
        transaction.setOrigTs(now);
        transaction.setProcTs(now);
        transactions.save(transaction);
    }

    public record InterestReport(int accountsSettled, BigDecimal totalInterest) {
    }
}
