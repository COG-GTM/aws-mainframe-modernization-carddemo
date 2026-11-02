package com.carddemo.transaction.schema;

import com.carddemo.common.schema.AbstractSchemaMappingTest;
import com.carddemo.transaction.support.StubGateways;
import org.springframework.context.annotation.Import;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
@Import(StubGateways.class)
class SchemaMappingTest extends AbstractSchemaMappingTest {

    SchemaMappingTest() {
        super("transaction");
    }
}
