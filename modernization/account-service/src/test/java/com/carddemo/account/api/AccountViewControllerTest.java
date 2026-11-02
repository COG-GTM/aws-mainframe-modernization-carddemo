package com.carddemo.account.api;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.account.client.CardXrefClient;
import com.carddemo.account.client.CardXrefView;
import com.carddemo.account.client.CustomerClient;
import com.carddemo.account.client.CustomerUpdate;
import com.carddemo.account.client.CustomerView;
import com.carddemo.account.domain.Account;
import com.carddemo.account.repository.AccountPostingRepository;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.common.error.RecordChangedException;
import com.carddemo.testsupport.AdminCaller;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

/** COACTVWC / COACTUPC flows; card-service and customer-service are mocked at the client seam. */
@SpringBootTest
@AutoConfigureMockMvc
@Import(AdminCaller.class)
class AccountViewControllerTest {

    private static final long ACCT_ID = 1L;
    private static final long CUST_ID = 1L;

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private AccountRepository accounts;

    @Autowired
    private AccountPostingRepository postings;

    @MockBean
    private CardXrefClient xrefs;

    @MockBean
    private CustomerClient customers;

    private long accountVersion;

    @BeforeEach
    void setUp() {
        postings.deleteAll();
        accounts.deleteAll();
        Account account = new Account(ACCT_ID);
        account.setCurrBal(new BigDecimal("194.00"));
        account.setCreditLimit(new BigDecimal("2020.00"));
        account.setCashCreditLimit(new BigDecimal("1020.00"));
        account.setOpenDate(LocalDate.of(2014, 11, 20));
        account.setExpirationDate(LocalDate.of(2025, 5, 20));
        account.setReissueDate(LocalDate.of(2025, 5, 20));
        account.setAddrZip("A000000000");
        account.setGroupId("DEFAULT");
        accountVersion = accounts.save(account).getVersion();
        when(xrefs.byAccount(ACCT_ID)).thenReturn(Optional.of(new CardXrefView("0500024453765740", CUST_ID, ACCT_ID)));
        when(customers.get(CUST_ID)).thenReturn(Optional.of(customer(3)));
    }

    private static CustomerView customer(long version) {
        return new CustomerView(CUST_ID, "Immanuel", "Madeline", "Kessler", "618 Deshaun Route", "Apt. 802",
                "Altenwerthshire", "NC", "USA", "27546", "(908)119-8310", "(212)693-8684", "8000",
                "000000000049", LocalDate.of(1961, 6, 8), "0053581756", "Y", 274, version);
    }

    /** The view's values typed back, i.e. no change; tests override single fields. */
    private String body(String overrides, long acctVersion, long custVersion) {
        String base = """
                "accountVersion": %d, "customerVersion": %d, "activeStatus": "Y", "creditLimit": 2020.00,
                "cashCreditLimit": "1,020.00", "currentBalance": 194, "currentCycleCredit": 0,
                "currentCycleDebit": 0, "openDate": "2014-11-20", "expirationDate": "2025-05-20",
                "reissueDate": "2025-05-20", "groupId": "default", "dateOfBirth": "1961-06-08",
                "ficoScore": "274", "firstName": "IMMANUEL", "middleName": "Madeline", "lastName": "Kessler",
                "addressLine1": "618 Deshaun Route", "addressLine2": "Apt. 802", "city": "Altenwerthshire",
                "stateCode": "NC", "zip": "27546", "phoneNumber1": "908-119-8310",
                "phoneNumber2": "(212)693-8684", "governmentIssuedId": "000000000049",
                "eftAccountId": "0053581756", "primaryCardHolder": "Y"
                """.formatted(acctVersion, custVersion);
        return "{" + base + (overrides.isEmpty() ? "" : ", " + overrides) + "}";
    }

    @Test
    void viewReturnsAccountAndCustomerAsOneScreen() throws Exception {
        mockMvc.perform(get("/api/v1/accounts/{id}/view", "00000000001"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.accountId").value("00000000001"))
                .andExpect(jsonPath("$.creditLimit").value(2020.00))
                .andExpect(jsonPath("$.customerId").value("000000001"))
                .andExpect(jsonPath("$.ssn").value("***-**-8000"))
                .andExpect(jsonPath("$.city").value("Altenwerthshire"))
                .andExpect(jsonPath("$.phoneNumber1").value("(908)119-8310"))
                .andExpect(jsonPath("$.customerVersion").value(3));
    }

    @Test
    void viewRejectsABadAccountIdWithTheCobolMessage() throws Exception {
        mockMvc.perform(get("/api/v1/accounts/{id}/view", "00000000000"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Account Filter must  be a non-zero 11 digit number"))
                .andExpect(jsonPath("$.fieldErrors[0].field").value("accountId"));
        mockMvc.perform(get("/api/v1/accounts/{id}/view", "12AB"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void viewReportsWhichFileMissedTheAccount() throws Exception {
        when(xrefs.byAccount(2L)).thenReturn(Optional.empty());
        mockMvc.perform(get("/api/v1/accounts/{id}/view", "2"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Account:00000000002 not found in Cross ref file."));
        when(xrefs.byAccount(3L)).thenReturn(Optional.of(new CardXrefView("1", CUST_ID, 3L)));
        mockMvc.perform(get("/api/v1/accounts/{id}/view", "3"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Account:00000000003 not found in Acct Master file."));
        when(customers.get(CUST_ID)).thenReturn(Optional.empty());
        mockMvc.perform(get("/api/v1/accounts/{id}/view", "1"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("CustId:000000001 not found in customer master."));
    }

    @Test
    void unchangedValuesAreRejectedAsNoChange() throws Exception {
        mockMvc.perform(put("/api/v1/accounts/{id}", ACCT_ID).contentType(MediaType.APPLICATION_JSON)
                        .content(body("", accountVersion, 3)))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("No change detected with respect to values fetched."));
        verify(customers, never()).update(any(Long.class), any());
    }

    @Test
    void editsReportTheFirstCobolMessageAndEveryFailingField() throws Exception {
        mockMvc.perform(put("/api/v1/accounts/{id}", ACCT_ID).contentType(MediaType.APPLICATION_JSON)
                        .content(body("\"activeStatus\": \"y\", \"zip\": \"2754\", \"ficoScore\": 900", accountVersion, 3)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Account Status must be Y or N."))
                .andExpect(jsonPath("$.fieldErrors[0].field").value("activeStatus"))
                .andExpect(jsonPath("$.fieldErrors[1].message").value("FICO Score: should be between 300 and 850"))
                .andExpect(jsonPath("$.fieldErrors[2].message").value("Zip must be all numeric."));
    }

    @Test
    void updateRewritesAccountAndCustomer() throws Exception {
        when(customers.update(eq(CUST_ID), any())).thenReturn(customer(4));
        mockMvc.perform(put("/api/v1/accounts/{id}", ACCT_ID).contentType(MediaType.APPLICATION_JSON)
                        .content(body("\"creditLimit\": \"$3,000.00\", \"currentBalance\": \"-5.25\", "
                                + "\"ficoScore\": 700, \"phoneNumber1\": \"201-555-0100\", \"ssn\": \"123-45-6789\"",
                                accountVersion, 3)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.creditLimit").value(3000.00))
                .andExpect(jsonPath("$.currentBalance").value(-5.25))
                .andExpect(jsonPath("$.customerVersion").value(4));
        Account saved = accounts.findById(ACCT_ID).orElseThrow();
        assertThat(saved.getCreditLimit()).isEqualByComparingTo("3000.00");
        assertThat(saved.getGroupId()).isEqualTo("default");
        assertThat(saved.getAddrZip()).isEqualTo("A000000000");
        ArgumentCaptor<CustomerUpdate> update = ArgumentCaptor.forClass(CustomerUpdate.class);
        verify(customers).update(eq(CUST_ID), update.capture());
        assertThat(update.getValue().phone1()).isEqualTo("(201)555-0100");
        assertThat(update.getValue().ssn()).isEqualTo(123456789L);
        assertThat(update.getValue().ficoScore()).isEqualTo(700);
        assertThat(update.getValue().countryCode()).isEqualTo("USA");
        assertThat(update.getValue().version()).isEqualTo(3);
    }

    @Test
    void aStaleVersionIsAConflict() throws Exception {
        mockMvc.perform(put("/api/v1/accounts/{id}", ACCT_ID).contentType(MediaType.APPLICATION_JSON)
                        .content(body("\"ficoScore\": 700", accountVersion, 2)))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.message").value("Record changed by some one else. Please review"));
        verify(customers, never()).update(any(Long.class), any());
    }

    @Test
    void aCustomerConflictRollsTheAccountBack() throws Exception {
        when(customers.update(eq(CUST_ID), any())).thenThrow(new RecordChangedException());
        mockMvc.perform(put("/api/v1/accounts/{id}", ACCT_ID).contentType(MediaType.APPLICATION_JSON)
                        .content(body("\"creditLimit\": 9999, \"ficoScore\": 700", accountVersion, 3)))
                .andExpect(status().isConflict());
        Account unchanged = accounts.findById(ACCT_ID).orElseThrow();
        assertThat(unchanged.getCreditLimit()).isEqualByComparingTo("2020.00");
        assertThat(unchanged.getVersion()).isEqualTo(accountVersion);
    }

    @Test
    void theVersionsAreRequired() throws Exception {
        mockMvc.perform(put("/api/v1/accounts/{id}", ACCT_ID).contentType(MediaType.APPLICATION_JSON)
                        .content("{\"activeStatus\": \"Y\"}"))
                .andExpect(status().isBadRequest());
    }
}
