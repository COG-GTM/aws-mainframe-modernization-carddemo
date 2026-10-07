package com.carddemo.customer;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.support.PostgresRepositoryTest;
import com.carddemo.support.Samples;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

/** CUSTDATA access paths: read by CUST-ID, rewrite, sequential in key order. */
class CustomerRepositoryIT extends PostgresRepositoryTest {

    @Autowired
    CustomerRepository customers;

    List<CustomerRecord> sample;

    @BeforeEach
    void load() {
        loadSamples(Dataset.CUSTDATA);
        sample = CustomerRecord.MAPPER.fromRecords(Samples.read(Dataset.CUSTDATA, RecordEncoding.EBCDIC));
    }

    @Test
    void readsByKeyAndSequentially() {
        CustomerRecord any = sample.get(12);
        Customer customer = customers.findById(any.custId()).orElseThrow();
        assertThat(customer.toRecord()).isEqualTo(any);
        assertThat(customer.getDobDt()).isEqualTo(LocalDate.parse(any.dob()));
        assertThat(customer.getPriCardHolderInd()).isIn(PrimaryCardHolder.YES, PrimaryCardHolder.NO);
        assertThat(customers.findAllByOrderByCustIdAsc()).extracting(Customer::toRecord)
                .containsExactlyElementsOf(sample.stream().sorted(Comparator.comparingInt(CustomerRecord::custId))
                        .toList());
    }

    @Test
    void rewriteBumpsTheVersion() {
        CustomerRecord any = sample.get(0);
        Customer customer = customers.findById(any.custId()).orElseThrow();
        CustomerRecord changed = new CustomerRecord(any.custId(), "Changed", any.middleName(), any.lastName(),
                any.addrLine1(), any.addrLine2(), any.addrLine3(), any.addrStateCd(), any.addrCountryCd(),
                any.addrZip(), any.phoneNum1(), any.phoneNum2(), any.ssn(), any.govtIssuedId(), "1999-02-28",
                any.eftAccountId(), PrimaryCardHolder.NO, 850);
        customer.update(changed);
        customers.saveAndFlush(customer);
        entityManager.clear();
        Customer reread = customers.findById(any.custId()).orElseThrow();
        assertThat(reread.toRecord()).isEqualTo(changed);
        assertThat(reread.getDobDt()).isEqualTo(LocalDate.of(1999, 2, 28));
        assertThat(reread.getVersion()).isEqualTo(1);
    }
}
