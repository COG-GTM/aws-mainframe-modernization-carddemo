package com.carddemo.programs.copaus0c;

import com.carddemo.programs.copaus0c.copybook.AccountRecord;
import com.carddemo.programs.copaus0c.copybook.CardDemoCommarea;
import com.carddemo.programs.copaus0c.copybook.CardXrefRecord;
import com.carddemo.programs.copaus0c.copybook.CustomerRecord;
import com.carddemo.programs.copaus0c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus0c.copybook.PendingAuthSummary;
import com.carddemo.programs.copaus0c.inmemory.InMemoryAccountRepository;
import com.carddemo.programs.copaus0c.inmemory.InMemoryCardXrefRepository;
import com.carddemo.programs.copaus0c.inmemory.InMemoryCustomerRepository;
import com.carddemo.programs.copaus0c.inmemory.InMemoryPendingAuthDb;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;

/** Hand-built CICS/IMS state for account 00000000001 with seven pending authorizations. */
final class TestData {
    static final Clock CLOCK = Clock.fixed(LocalDateTime.of(2026, 11, 26, 10, 15, 30).toInstant(ZoneOffset.UTC),
            ZoneOffset.UTC);

    final InMemoryCardXrefRepository xref = new InMemoryCardXrefRepository();
    final InMemoryAccountRepository accounts = new InMemoryAccountRepository();
    final InMemoryCustomerRepository customers = new InMemoryCustomerRepository();
    final InMemoryPendingAuthDb ims = new InMemoryPendingAuthDb();
    final Copaus0cService service = new Copaus0cService(xref, accounts, customers, ims, CLOCK);

    TestData() {
        xref.add(new CardXrefRecord("4111111111111111", 7, 1));
        accounts.add(new AccountRecord(1, "Y", new BigDecimal("194.00"), new BigDecimal("2020.00"),
                new BigDecimal("1020.00"), "2014-11-20", "2025-05-20", "2025-05-20", BigDecimal.ZERO.setScale(2),
                BigDecimal.ZERO.setScale(2), "12345     ", "A000000000"));
        customers.add(new CustomerRecord(7, pad("Jane", 25), pad("Quincy", 25), pad("Doe", 25),
                pad("1 Main  Street", 50), pad("Unit 5", 50), pad("Springfield", 50), "IL", "USA",
                pad("62701", 10), pad("(217)555-0100x9", 15), pad("", 15), 123456789, pad("", 20),
                "1980-01-01", pad("", 10), "Y", 750));
        ims.addSummary(new PendingAuthSummary(1, 7, "A", List.of("  ", "  ", "  ", "  ", "  "),
                new BigDecimal("5000.00"), new BigDecimal("1000.00"), new BigDecimal("-123.45"),
                new BigDecimal("12.50"), 4, 3, new BigDecimal("456.78"), new BigDecimal("1234567.89")));
        for (int i = 1; i <= 7; i++) {
            ims.addDetail(1, detail(i, i % 2 == 1 ? "00" : "05", new BigDecimal(i * 10 + ".25")));
        }
    }

    static PendingAuthDetails detail(int n, String respCode, BigDecimal approvedAmt) {
        return new PendingAuthDetails(99990 + n, 999999990 + n, "26112" + n, "1015" + (10 + n),
                "4111111111111111", "SALE", "1227", "0100  ", "POS   ", "AB" + n + "   ", respCode, "0000",
                "000000", approvedAmt, approvedAmt, "5411", "840", "01", pad("M" + n, 15), pad("Shop " + n, 22),
                pad("City", 13), "IL", pad("62701", 9), pad("TXN00000000000" + n, 15), "P", " ", "        ");
    }

    static String key(int n) {
        return PendingAuthDetails.authorizationKey(99990 + n, 999999990 + n);
    }

    static String pad(String s, int n) {
        return String.format("%-" + n + "s", s);
    }

    /** COMMAREA as handed over by a calling program (CDEMO-PGM-ENTER) for the given account. */
    static CardDemoCommarea fromMenu(String acctId) {
        CardDemoCommarea ca = new CardDemoCommarea();
        ca.initialize();
        ca.cdemoFromProgram = "COMEN01C";
        ca.cdemoPgmContext = CardDemoCommarea.CDEMO_PGM_ENTER;
        ca.cdemoAcctId = acctId;
        return ca;
    }
}
