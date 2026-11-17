package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.entity.Customer;
import java.time.LocalDate;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class CustomerRepositoryIT extends RepositoryIT {

    @Autowired
    CustomerRepository customers;

    @Test
    void readsAndRewritesByCustId() {
        Customer c = new Customer();
        c.setCustId(1);
        c.setFirstName("TEST");
        c.setMiddleName("FAKE");
        c.setLastName("CUSTOMER");
        c.setAddrLine1("1 TEST STREET");
        c.setAddrLine2("UNIT 0");
        c.setAddrLine3("TESTVILLE");
        c.setAddrStateCd("ZZ");
        c.setAddrCountryCd("USA");
        c.setAddrZip("00000");
        c.setPhoneNum1("(555)555-0100");
        c.setPhoneNum2("(555)555-0101");
        c.setSsn(999000001);
        c.setGovtIssuedId("TEST-GOVT-ID-0001");
        c.setDob(LocalDate.of(1970, 1, 1));
        c.setEftAccountId("0000000001");
        c.setPriCardHolderInd("Y");
        c.setFicoCreditScore((short) 274);
        customers.save(c);
        flushAndClear();

        var read = customers.findById(1).orElseThrow();
        assertThat(read.getSsn()).isEqualTo(999000001);
        assertThat(read.getDob()).isEqualTo(LocalDate.of(1970, 1, 1));
        assertThat(read.getFicoCreditScore()).isEqualTo((short) 274);

        read.setAddrLine2("");
        flushAndClear();

        assertThat(customers.findById(1).orElseThrow().getAddrLine2()).isEmpty();
    }
}
