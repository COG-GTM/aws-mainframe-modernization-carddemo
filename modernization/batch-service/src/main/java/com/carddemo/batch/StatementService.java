package com.carddemo.batch;

import com.carddemo.common.domain.Account;
import com.carddemo.common.domain.AccountRepository;
import com.carddemo.common.domain.CardTransaction;
import com.carddemo.common.domain.CardTransactionRepository;
import com.carddemo.common.domain.CardXref;
import com.carddemo.common.domain.CardXrefRepository;
import com.carddemo.common.domain.Customer;
import com.carddemo.common.domain.CustomerRepository;
import java.math.BigDecimal;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** CREASTMT / CBSTM03A + CBSTM03B: one statement per card cross-reference, in XREF key order. */
@Service
public class StatementService {

    private final CardXrefRepository xrefs;
    private final CustomerRepository customers;
    private final AccountRepository accounts;
    private final CardTransactionRepository transactions;

    public StatementService(CardXrefRepository xrefs, CustomerRepository customers, AccountRepository accounts,
                            CardTransactionRepository transactions) {
        this.xrefs = xrefs;
        this.customers = customers;
        this.accounts = accounts;
        this.transactions = transactions;
    }

    @Transactional(readOnly = true)
    public JobResult run() {
        StringBuilder out = new StringBuilder();
        int statements = 0;
        for (CardXref x : xrefs.findAll(Sort.by("cardNum"))) {
            Customer c = customers.findById(x.getCustId()).orElse(null);
            Account a = accounts.findById(x.getAcctId()).orElse(null);
            if (c == null || a == null) {
                continue;
            }
            statements++;
            String name = Stream.of(c.getFirstName(), c.getMiddleName(), c.getLastName())
                    .filter(s -> s != null && !s.isBlank()).map(String::trim).collect(Collectors.joining(" "));
            String address = Stream.of(c.getAddrLine1(), c.getAddrLine2())
                    .filter(s -> s != null && !s.isBlank()).map(String::trim).collect(Collectors.joining(" "));
            out.append("*".repeat(31)).append("START OF STATEMENT").append("*".repeat(31)).append('\n');
            out.append(name).append('\n').append(address).append('\n');
            out.append(String.format("%s %s %s %s%n", nz(c.getAddrLine3()), nz(c.getAddrStateCd()),
                    nz(c.getAddrCountryCd()), nz(c.getAddrZip())));
            out.append("-".repeat(80)).append('\n');
            out.append(String.format("%-20s%-60s%n", "Account ID         :", String.format("%011d", a.getAcctId())));
            out.append(String.format("%-20s%-60s%n", "Current Balance    :", a.getCurrBal().toPlainString()));
            out.append(String.format("%-20s%-60s%n", "FICO Score         :", c.getFicoCreditScore()));
            out.append("-".repeat(80)).append('\n');
            out.append(String.format("%-16s %-51s %11s%n", "Tran ID", "Tran Details", "Tran Amount"));
            BigDecimal total = BigDecimal.ZERO;
            for (CardTransaction t : transactions.findByCardNumOrderByTranIdAsc(x.getCardNum())) {
                out.append(String.format("%-16s %-51.51s %,11.2f%n", t.getTranId(), t.getDescription(),
                        t.getAmount()));
                total = total.add(t.getAmount());
            }
            out.append(String.format("%-68s %,11.2f%n", "Total EXP:", total));
            out.append("*".repeat(32)).append("END OF STATEMENT").append("*".repeat(32)).append('\n');
        }
        return JobResult.of("CREASTMT", 0, out.toString(), "statements", statements);
    }

    private static String nz(String s) {
        return s == null ? "" : s.trim();
    }
}
