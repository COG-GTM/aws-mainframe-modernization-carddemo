package com.carddemo.customer.api;

import com.carddemo.testsupport.AdminCaller;
import org.springframework.context.annotation.Import;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.customer.domain.Customer;
import com.carddemo.customer.repository.CustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest
@AutoConfigureMockMvc
@Import(AdminCaller.class)
class CustomerControllerTest {

    private static final long CUST_ID = 100000001L;

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private CustomerRepository customers;

    @BeforeEach
    void setUp() {
        customers.deleteAll();
        Customer customer = new Customer(CUST_ID);
        customer.setFirstName("JOHN");
        customer.setLastName("DOE");
        customer.setSsn(123456789L);
        customer.setFicoCreditScore(720);
        customers.save(customer);
    }

    @Test
    void returnsTheCustomerWithAMaskedSsn() throws Exception {
        mockMvc.perform(get("/api/v1/customers/{id}", CUST_ID))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.ssnLast4").value("6789"))
                .andExpect(jsonPath("$.ficoScore").value(720));
    }

    @Test
    void rejectsAnOutOfRangeFicoScore() throws Exception {
        mockMvc.perform(put("/api/v1/customers/{id}", CUST_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {"firstName": "JOHN", "lastName": "DOE", "ficoScore": 900}
                                """))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("ficoScore"));
    }

    @Test
    void returnsTheVersionAndRejectsAStaleOne() throws Exception {
        long version = customers.findById(CUST_ID).orElseThrow().getVersion();
        mockMvc.perform(get("/api/v1/customers/{id}", CUST_ID))
                .andExpect(jsonPath("$.version").value(version));
        mockMvc.perform(put("/api/v1/customers/{id}", CUST_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {"firstName": "JOHN", "lastName": "DOE", "version": %d}
                                """.formatted(version + 1)))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.message").value("Record changed by some one else. Please review"));
    }

    @Test
    void updatesTheSsnOnlyWhenGiven() throws Exception {
        mockMvc.perform(put("/api/v1/customers/{id}", CUST_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {"firstName": "JOHN", "lastName": "DOE", "ficoScore": 700}
                                """))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.ssnLast4").value("6789"));
        mockMvc.perform(put("/api/v1/customers/{id}", CUST_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {"firstName": "JOHN", "lastName": "DOE", "ficoScore": 700, "ssn": 987654321}
                                """))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.ssnLast4").value("4321"));
    }
}
