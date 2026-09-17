package com.carddemo.customer.service;

import com.carddemo.common.error.NotFoundException;
import com.carddemo.customer.api.dto.CustomerUpdateRequest;
import com.carddemo.customer.domain.Customer;
import com.carddemo.customer.repository.CustomerRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class CustomerService {

    private final CustomerRepository customers;

    public CustomerService(CustomerRepository customers) {
        this.customers = customers;
    }

    @Transactional(readOnly = true)
    public Customer get(long custId) {
        return customers.findById(custId)
                .orElseThrow(() -> new NotFoundException("Customer " + custId + " not found"));
    }

    @Transactional(readOnly = true)
    public Page<Customer> list(Pageable pageable) {
        return customers.findAll(pageable);
    }

    @Transactional
    public Customer update(long custId, CustomerUpdateRequest request) {
        Customer customer = get(custId);
        customer.setFirstName(request.firstName());
        customer.setMiddleName(request.middleName());
        customer.setLastName(request.lastName());
        customer.setAddrLine1(request.addressLine1());
        customer.setAddrLine2(request.addressLine2());
        customer.setAddrLine3(request.addressLine3());
        customer.setAddrStateCd(request.stateCode());
        customer.setAddrCountryCd(request.countryCode());
        customer.setAddrZip(request.zip());
        customer.setPhoneNum1(request.phone1());
        customer.setPhoneNum2(request.phone2());
        customer.setGovtIssuedId(request.governmentIssuedId());
        customer.setDateOfBirth(request.dateOfBirth());
        customer.setEftAccountId(request.eftAccountId());
        customer.setPriCardHolderInd(request.primaryCardHolderIndicator());
        customer.setFicoCreditScore(request.ficoScore());
        return customers.save(customer);
    }
}
