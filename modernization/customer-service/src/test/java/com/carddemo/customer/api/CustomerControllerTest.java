package com.carddemo.customer.api;

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
}
