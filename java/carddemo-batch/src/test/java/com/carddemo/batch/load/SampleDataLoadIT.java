package com.carddemo.batch.load;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.Account;
import com.carddemo.data.AccountRepository;
import com.carddemo.data.CardRepository;
import com.carddemo.data.CardXrefRepository;
import com.carddemo.data.Customer;
import com.carddemo.data.CustomerRepository;
import com.carddemo.data.DailyTransaction;
import com.carddemo.data.DailyTransactionRepository;
import com.carddemo.data.DisclosureGroupId;
import com.carddemo.data.DisclosureGroupRepository;
import com.carddemo.data.PostgresTestcontainersConfiguration;
import com.carddemo.data.TranCatBalanceId;
import com.carddemo.data.TranCatBalanceRepository;
import com.carddemo.data.TranCategoryId;
import com.carddemo.data.TranCategoryRepository;
import com.carddemo.data.TranTypeRepository;
import com.carddemo.data.TransactionRepository;
import com.carddemo.data.UserSecurity;
import com.carddemo.data.UserSecurityRepository;
import com.carddemo.legacyio.CardDemoLayouts;
import com.carddemo.legacyio.LegacyFileCodec;
import com.carddemo.legacyio.Record;
import com.carddemo.legacyio.RecordLayout;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Map;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.crypto.password.PasswordEncoder;

/** Runs {@code sampleDataLoadJob} against PostgreSQL and checks the tables against the sample files in app/data. */
@SpringBootTest(properties = "spring.batch.job.enabled=false")
@Import(PostgresTestcontainersConfiguration.class)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class SampleDataLoadIT {

    private static final Path DATA = Path.of("..", "..", "app", "data").toAbsolutePath().normalize();

    @Autowired
    private JobLauncher jobLauncher;

    @Autowired
    private Job sampleDataLoadJob;

    @Autowired
    private SampleDataLoader loader;

    @Autowired
    private JdbcTemplate jdbc;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    private AccountRepository accounts;

    @Autowired
    private CustomerRepository customers;

    @Autowired
    private CardRepository cards;

    @Autowired
    private CardXrefRepository xrefs;

    @Autowired
    private TransactionRepository transactions;

    @Autowired
    private DailyTransactionRepository dailyTransactions;

    @Autowired
    private TranTypeRepository tranTypes;

    @Autowired
    private TranCategoryRepository tranCategories;

    @Autowired
    private DisclosureGroupRepository disclosureGroups;

    @Autowired
    private TranCatBalanceRepository tranCatBalances;

    @Autowired
    private UserSecurityRepository users;

    private JobExecution execution;

    @BeforeAll
    void runJob() throws Exception {
        execution = jobLauncher.run(sampleDataLoadJob, new JobParametersBuilder()
                .addString(SampleDataLoadJobConfiguration.DATA_DIR_PARAMETER, DATA.toString())
                .toJobParameters());
    }

    @Test
    void jobCompletesAndCountsEveryRecord() {
        assertThat(execution.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertThat(execution.getStepExecutions()).singleElement()
                .satisfies(step -> assertThat(step.getWriteCount()).isEqualTo(636));
    }

    @Test
    void recordCountsMatchTheSampleFiles() {
        assertThat(accounts.count()).isEqualTo(50);
        assertThat(customers.count()).isEqualTo(50);
        assertThat(cards.count()).isEqualTo(50);
        assertThat(xrefs.count()).isEqualTo(50);
        assertThat(transactions.count()).isZero();
        assertThat(dailyTransactions.count()).isEqualTo(300);
        assertThat(tranTypes.count()).isEqualTo(7);
        assertThat(tranCategories.count()).isEqualTo(18);
        assertThat(disclosureGroups.count()).isEqualTo(51);
        assertThat(tranCatBalances.count()).isEqualTo(50);
        assertThat(users.count()).isEqualTo(10);
    }

    @Test
    void accountMatchesAcctdata() {
        Account account = accounts.findById(1L).orElseThrow();
        assertThat(account.getActiveStatus()).isEqualTo("Y");
        assertThat(account.getCurrBal()).isEqualByComparingTo("194.00");
        assertThat(account.getCreditLimit()).isEqualByComparingTo("2020.00");
        assertThat(account.getCashCreditLimit()).isEqualByComparingTo("1020.00");
        assertThat(account.getOpenDate()).isEqualTo(LocalDate.of(2014, 11, 20));
        assertThat(account.getExpirationDate()).isEqualTo(LocalDate.of(2025, 5, 20));
        assertThat(account.getReissueDate()).isEqualTo(LocalDate.of(2025, 5, 20));
        assertThat(account.getCurrCycCredit()).isEqualByComparingTo(BigDecimal.ZERO);
        assertThat(account.getAddrZip()).isEqualTo("A000000000");
        assertThat(account.getGroupId()).isEmpty();
        assertThat(sum("select sum(curr_bal) from accounts")).isEqualByComparingTo("12269.00");
        assertThat(sum("select sum(credit_limit) from accounts")).isEqualByComparingTo("233711.00");
    }

    @Test
    void customerMatchesCustdata() throws IOException {
        Record source = firstRecord("custdata.txt", CardDemoLayouts.CUSTOMER);
        Customer customer = customers.findById(source.getLong("CUST-ID")).orElseThrow();
        assertThat(customer.getFirstName()).isEqualTo(source.getString("CUST-FIRST-NAME").stripTrailing());
        assertThat(customer.getLastName()).isEqualTo(source.getString("CUST-LAST-NAME").stripTrailing());
        assertThat(customer.getAddrLine1()).isEqualTo(source.getString("CUST-ADDR-LINE-1").stripTrailing());
        assertThat(customer.getAddrStateCd()).isEqualTo(source.getString("CUST-ADDR-STATE-CD"));
        assertThat(customer.getAddrCountryCd()).isEqualTo("USA");
        assertThat(customer.getSsn()).isEqualTo(source.getDecimal("CUST-SSN").intValueExact());
        assertThat(customer.getGovtIssuedId()).isEqualTo(source.getString("CUST-GOVT-ISSUED-ID").stripTrailing());
        assertThat(customer.getDob()).isEqualTo(LocalDate.parse(source.getString("CUST-DOB-YYYY-MM-DD")));
        assertThat(customer.getEftAccountId()).isEqualTo(source.getString("CUST-EFT-ACCOUNT-ID").stripTrailing());
        assertThat(customer.getPriCardHolderInd()).isEqualTo("Y");
        assertThat(customer.getFicoCreditScore()).isEqualTo(source.getDecimal("CUST-FICO-CREDIT-SCORE").intValueExact());
    }

    @Test
    void cardAndCrossReferenceMatchCarddataAndCardxref() throws IOException {
        Record source = firstRecord("carddata.txt", CardDemoLayouts.CARD);
        long acctId = source.getLong("CARD-ACCT-ID");
        assertThat(cards.findByAcctIdOrderByCardNum(acctId)).singleElement().satisfies(card -> {
            assertThat(card.getCardNum()).isEqualTo(source.getString("CARD-NUM"));
            assertThat(card.getCvvCd()).isEqualTo(source.getDecimal("CARD-CVV-CD").intValueExact());
            assertThat(card.getEmbossedName()).isEqualTo(source.getString("CARD-EMBOSSED-NAME").stripTrailing());
            assertThat(card.getExpirationDate()).isEqualTo(LocalDate.parse(source.getString("CARD-EXPIRAION-DATE")));
            assertThat(card.getActiveStatus()).isEqualTo("Y");
            assertThat(xrefs.findById(card.getCardNum())).hasValueSatisfying(xref -> {
                assertThat(xref.getCustId()).isEqualTo(50L);
                assertThat(xref.getAcctId()).isEqualTo(acctId);
            });
        });
        assertThat(acctId).isEqualTo(50L);
        assertThat(xrefs.findByAcctIdOrderByCardNum(acctId)).hasSize(1);
        assertThat(jdbc.queryForObject("select count(*) from cards c join accounts a on a.acct_id = c.acct_id",
                Integer.class)).isEqualTo(50);
    }

    @Test
    void dailyTransactionsKeepFileOrderAndValues() {
        DailyTransaction first = dailyTransactions.findById(1).orElseThrow();
        assertThat(first.getTranId()).isEqualTo("0000000000683580");
        assertThat(first.getTypeCd()).isEqualTo("01");
        assertThat(first.getCatCd()).isEqualTo(1);
        assertThat(first.getSource()).isEqualTo("POS TERM");
        assertThat(first.getDescription()).isEqualTo("Purchase at Abshire-Lowe");
        assertThat(first.getAmount()).isEqualByComparingTo("504.77");
        assertThat(first.getMerchantId()).isEqualTo(800000000L);
        assertThat(first.getMerchantName()).isEqualTo("Abshire-Lowe");
        assertThat(first.getMerchantCity()).isEqualTo("North Enoshaven");
        assertThat(first.getMerchantZip()).isEqualTo("72112");
        assertThat(first.getCardNum()).isEqualTo("4859452612877065");
        assertThat(first.getOrigTs()).isEqualTo(LocalDateTime.of(2022, 6, 10, 19, 27, 53));
        assertThat(first.getProcTs()).isNull();
        assertThat(dailyTransactions.findById(300)).hasValueSatisfying(
                last -> assertThat(last.getTranId()).isEqualTo("0000000996722787"));
        assertThat(sum("select sum(amount) from daily_transactions")).isEqualByComparingTo("104801.54");
        assertThat(jdbc.queryForObject(
                "select count(*) from daily_transactions d left join card_xrefs x on x.card_num = d.card_num"
                        + " where x.card_num is null", Integer.class)).isZero();
    }

    @Test
    void referenceTablesMatchTheirFiles() {
        assertThat(tranTypes.findById("01")).hasValueSatisfying(
                type -> assertThat(type.getDescription()).isEqualTo("Purchase"));
        assertThat(tranTypes.findById("07")).hasValueSatisfying(
                type -> assertThat(type.getDescription()).isEqualTo("Adjustment"));
        assertThat(tranCategories.findById(new TranCategoryId("01", 3))).hasValueSatisfying(
                category -> assertThat(category.getDescription()).isEqualTo("Convenience Check Debit"));
        assertThat(disclosureGroups.findById(new DisclosureGroupId("A000000000", "01", 2))).hasValueSatisfying(
                group -> assertThat(group.getIntRate()).isEqualByComparingTo("25.00"));
        assertThat(sum("select sum(int_rate) from disclosure_groups")).isEqualByComparingTo("375.00");
        assertThat(jdbc.queryForList("select distinct acct_group_id from disclosure_groups order by 1", String.class))
                .containsExactly("A000000000", "DEFAULT", "ZEROAPR");
        assertThat(tranCatBalances.findById(new TranCatBalanceId(1L, "01", 1))).hasValueSatisfying(
                balance -> assertThat(balance.getBalance()).isEqualByComparingTo(BigDecimal.ZERO));
    }

    @Test
    void usersComeFromUsrsecWithBcryptPasswords() {
        UserSecurity admin = users.findById("ADMIN001").orElseThrow();
        assertThat(admin.getFirstName()).isEqualTo("MARGARET");
        assertThat(admin.getLastName()).isEqualTo("GOLD");
        assertThat(admin.getUserType()).isEqualTo("A");
        assertThat(admin.getPasswordHash()).startsWith("$2a$").isNotEqualTo("PASSWORD");
        assertThat(passwordEncoder.matches("PASSWORD", admin.getPasswordHash())).isTrue();
        assertThat(passwordEncoder.matches("PASSWORDX", admin.getPasswordHash())).isFalse();

        assertThat(users.findById("USER0005")).hasValueSatisfying(user -> {
            assertThat(user.getFirstName()).isEqualTo("LEE");
            assertThat(user.getLastName()).isEqualTo("TING");
            assertThat(user.getUserType()).isEqualTo("U");
        });
        assertThat(jdbc.queryForObject("select count(*) from users where user_type = 'A'", Integer.class)).isEqualTo(5);
    }

    @Test
    void reloadingReplacesRatherThanDuplicates() {
        Map<String, Integer> counts = loader.replaceAll(DATA);
        assertThat(counts).containsEntry("accounts", 50).containsEntry("daily_transactions", 300)
                .containsEntry("users", 10);
        assertThat(accounts.count()).isEqualTo(50);
        assertThat(dailyTransactions.count()).isEqualTo(300);
    }

    private static Record firstRecord(String asciiFile, RecordLayout layout) throws IOException {
        return LegacyFileCodec.ascii().read(DATA.resolve("ASCII").resolve(asciiFile), layout).getFirst().record();
    }

    private BigDecimal sum(String sql) {
        return jdbc.queryForObject(sql, BigDecimal.class);
    }
}
