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
        c.setFirstName("Immanuel");
        c.setMiddleName("Madeline");
        c.setLastName("Kessler");
        c.setAddrLine1("618 Deshaun Route");
        c.setAddrLine2("Apt. 802");
        c.setAddrLine3("Altenwerthshire");
        c.setAddrStateCd("NC");
        c.setAddrCountryCd("USA");
        c.setAddrZip("12546");
        c.setPhoneNum1("(908)119-8310");
        c.setPhoneNum2("(373)693-8684");
        c.setSsn(20973888);
        c.setGovtIssuedId("00000000000049368437");
        c.setDob(LocalDate.of(1961, 6, 8));
        c.setEftAccountId("0053581756");
        c.setPriCardHolderInd("Y");
        c.setFicoCreditScore((short) 274);
        customers.save(c);
        flushAndClear();

        var read = customers.findById(1).orElseThrow();
        assertThat(read.getSsn()).isEqualTo(20973888);
        assertThat(read.getDob()).isEqualTo(LocalDate.of(1961, 6, 8));
        assertThat(read.getFicoCreditScore()).isEqualTo((short) 274);

        read.setAddrLine2("");
        flushAndClear();

        assertThat(customers.findById(1).orElseThrow().getAddrLine2()).isEmpty();
    }
}
