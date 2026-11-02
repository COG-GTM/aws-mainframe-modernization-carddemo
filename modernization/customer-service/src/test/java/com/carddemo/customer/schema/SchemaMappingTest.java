package com.carddemo.customer.schema;

import com.carddemo.common.schema.AbstractSchemaMappingTest;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
class SchemaMappingTest extends AbstractSchemaMappingTest {

    SchemaMappingTest() {
        super("customer");
    }
}
