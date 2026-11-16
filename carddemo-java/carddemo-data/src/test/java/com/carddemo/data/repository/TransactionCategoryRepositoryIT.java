package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.entity.TransactionCategory;
import com.carddemo.data.entity.TransactionCategoryId;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class TransactionCategoryRepositoryIT extends RepositoryIT {

    @Autowired
    TransactionCategoryRepository categories;

    @Test
    void readsByTypeAndCategory() {
        save("01", 2, "Regular Purchase Return");
        save("01", 1, "Regular Sales Draft");
        save("02", 1, "Cash payment");
        flushAndClear();

        assertThat(categories.findById(new TransactionCategoryId("01", (short) 1))).get()
                .extracting(TransactionCategory::getDescription).isEqualTo("Regular Sales Draft");
        assertThat(categories.findByIdTypeCdOrderByIdCatCd("01"))
                .extracting(TransactionCategory::getDescription)
                .containsExactly("Regular Sales Draft", "Regular Purchase Return");
    }

    private void save(String typeCd, int catCd, String description) {
        var c = new TransactionCategory();
        c.setId(new TransactionCategoryId(typeCd, (short) catCd));
        c.setDescription(description);
        categories.save(c);
    }
}
