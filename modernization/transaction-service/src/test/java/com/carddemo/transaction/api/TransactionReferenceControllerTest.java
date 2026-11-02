package com.carddemo.transaction.api;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.transaction.domain.TransactionCategoryId;
import com.carddemo.transaction.domain.TransactionCategoryType;
import com.carddemo.transaction.domain.TransactionType;
import com.carddemo.transaction.repository.TransactionCategoryTypeRepository;
import com.carddemo.transaction.repository.TransactionTypeRepository;
import com.carddemo.transaction.support.StubGateways;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest
@AutoConfigureMockMvc
@Import(StubGateways.class)
class TransactionReferenceControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private TransactionTypeRepository types;

    @Autowired
    private TransactionCategoryTypeRepository categories;

    @BeforeEach
    void setUp() {
        categories.deleteAll();
        types.deleteAll();
        types.save(new TransactionType("02", "Payment"));
        types.save(new TransactionType("01", "Purchase"));
        categories.save(new TransactionCategoryType(new TransactionCategoryId("02", 1), "Bill Payment - Online"));
        categories.save(new TransactionCategoryType(new TransactionCategoryId("01", 5), "Interest Amount"));
        categories.save(new TransactionCategoryType(new TransactionCategoryId("01", 1), "Regular Sales Draft"));
    }

    @Test
    void listsTransactionTypesInKeyOrder() throws Exception {
        mockMvc.perform(get("/api/v1/transaction-types"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(2))
                .andExpect(jsonPath("$.content[0].typeCode").value("01"))
                .andExpect(jsonPath("$.content[0].description").value("Purchase"))
                .andExpect(jsonPath("$.content[1].typeCode").value("02"));
    }

    @Test
    void listsTransactionCategoriesInKeyOrder() throws Exception {
        mockMvc.perform(get("/api/v1/transaction-categories").param("size", "2"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalPages").value(2))
                .andExpect(jsonPath("$.content[0].typeCode").value("01"))
                .andExpect(jsonPath("$.content[0].categoryCode").value(1))
                .andExpect(jsonPath("$.content[1].categoryCode").value(5))
                .andExpect(jsonPath("$.content[1].description").value("Interest Amount"));
    }

    @Test
    void rejectsAPageSizeAboveTheMaximum() throws Exception {
        mockMvc.perform(get("/api/v1/transaction-types").param("size", "201"))
                .andExpect(status().isBadRequest());
    }
}
