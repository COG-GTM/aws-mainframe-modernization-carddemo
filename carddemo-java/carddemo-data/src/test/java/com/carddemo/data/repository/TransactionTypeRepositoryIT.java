package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.entity.TransactionType;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class TransactionTypeRepositoryIT extends RepositoryIT {

    @Autowired
    TransactionTypeRepository types;

    @Test
    void readsByTypeCode() {
        var purchase = new TransactionType();
        purchase.setTypeCd("01");
        purchase.setDescription("Purchase");
        types.save(purchase);
        flushAndClear();

        assertThat(types.findById("01")).get().extracting(TransactionType::getDescription).isEqualTo("Purchase");
        assertThat(types.findById("99")).isEmpty();
    }
}
