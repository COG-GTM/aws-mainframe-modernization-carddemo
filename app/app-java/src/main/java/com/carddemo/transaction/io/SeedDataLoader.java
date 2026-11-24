package com.carddemo.transaction.io;

import com.carddemo.transaction.CardDemoProperties;
import com.carddemo.transaction.cobol.CobolFileException;
import com.carddemo.transaction.cobol.FileStatus;
import com.carddemo.transaction.repository.AccountRepository;
import com.carddemo.transaction.repository.CardRepository;
import com.carddemo.transaction.repository.CardXrefRepository;
import com.carddemo.transaction.repository.CustomerRepository;
import com.carddemo.transaction.repository.TransactionCategoryBalanceRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;

/**
 * IDCAMS DEFINE + REPRO equivalent: loads the VSAM KSDS clusters (ACCTDATA, CARDDATA, CARDXREF, CUSTDATA,
 * TCATBALF) from the ASCII extracts in {@code app/data/ASCII} when the store is empty. Runs before the
 * Spring Batch job launcher.
 */
@Component
@Order(Ordered.HIGHEST_PRECEDENCE)
@ConditionalOnProperty(prefix = "carddemo.seed", name = "enabled", havingValue = "true")
public class SeedDataLoader implements ApplicationRunner {

    private static final Logger log = LoggerFactory.getLogger(SeedDataLoader.class);

    private final CardDemoProperties properties;
    private final AccountRepository accounts;
    private final CardRepository cards;
    private final CardXrefRepository xrefs;
    private final CustomerRepository customers;
    private final TransactionCategoryBalanceRepository tcatbals;

    public SeedDataLoader(CardDemoProperties properties, AccountRepository accounts, CardRepository cards,
                          CardXrefRepository xrefs, CustomerRepository customers,
                          TransactionCategoryBalanceRepository tcatbals) {
        this.properties = properties;
        this.accounts = accounts;
        this.cards = cards;
        this.xrefs = xrefs;
        this.customers = customers;
        this.tcatbals = tcatbals;
    }

    @Override
    @Transactional
    public void run(ApplicationArguments args) {
        if (accounts.count() > 0) {
            log.info("Seed skipped: ACCTDATA already contains {} records", accounts.count());
            return;
        }
        Path dir = Path.of(properties.seed().dataDir());
        accounts.saveAll(load(dir.resolve("acctdata.txt"), "ACCTFILE", RecordLayouts::account));
        cards.saveAll(load(dir.resolve("carddata.txt"), "CARDFILE", RecordLayouts::card));
        xrefs.saveAll(load(dir.resolve("cardxref.txt"), "XREFFILE", RecordLayouts::cardXref));
        customers.saveAll(load(dir.resolve("custdata.txt"), "CUSTFILE", RecordLayouts::customer));
        tcatbals.saveAll(load(dir.resolve("tcatbal.txt"), "TCATBALF", RecordLayouts::transactionCategoryBalance));
        log.info("Seeded from {}: {} accounts, {} cards, {} xrefs, {} customers, {} category balances",
                dir.toAbsolutePath().normalize(), accounts.count(), cards.count(), xrefs.count(),
                customers.count(), tcatbals.count());
    }

    private static <T> List<T> load(Path file, String ddName, Function<String, T> layout) {
        try {
            return Files.readAllLines(file, StandardCharsets.US_ASCII).stream()
                    .filter(line -> !line.isBlank())
                    .map(layout)
                    .toList();
        } catch (IOException e) {
            throw new CobolFileException(ddName, FileStatus.FILE_NOT_FOUND, "ERROR LOADING " + file, e);
        }
    }
}
