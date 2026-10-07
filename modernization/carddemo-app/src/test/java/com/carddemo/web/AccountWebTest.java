package com.carddemo.web;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.account.Account;
import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountStatus;
import com.carddemo.card.CardXref;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.customer.Customer;
import com.carddemo.customer.CustomerRecord;
import com.carddemo.customer.PrimaryCardHolder;
import com.carddemo.user.UserType;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;

/** Account 1 / customer 1 / two cards in mocked repositories (values valid for every {@code COACTUPC} edit). */
abstract class AccountWebTest extends OnlineWebTest {

    static final String ACCOUNTS = "/api/v1/accounts";
    static final String CARD_1 = "0500024453765740";
    static final String CARD_2 = "0500024453765741";

    protected Account account;
    protected Customer customer;

    @BeforeEach
    void givenAccountOne() {
        account = Account.from(accountRecord());
        customer = Customer.from(customerRecord("USA"));
        given(xrefs.findFirstByAcctIdOrderByCardNumAsc(1L)).willReturn(Optional.of(xref(CARD_1)));
        given(xrefs.findByAcctIdOrderByCardNumAsc(1L)).willReturn(List.of(xref(CARD_1), xref(CARD_2)));
        given(accounts.findById(1L)).willAnswer(i -> Optional.of(account));
        given(customers.findById(1)).willAnswer(i -> Optional.of(customer));
        given(accounts.saveAndFlush(any())).willAnswer(i -> i.getArgument(0));
        given(customers.saveAndFlush(any())).willAnswer(i -> i.getArgument(0));
        given(accounts.lockVersion(1L)).willAnswer(i -> Optional.of(account.getVersion()));
        given(customers.lockVersion(1)).willAnswer(i -> Optional.of(customer.getVersion()));
    }

    static AccountRecord accountRecord() {
        return new AccountRecord(1L, AccountStatus.fromCode("Y"), new BigDecimal("1940.00"),
                new BigDecimal("20200.00"), new BigDecimal("10200.00"), "2014-11-20", "2025-05-20", "2025-05-20",
                new BigDecimal("0.00"), new BigDecimal("0.00"), "", "A000000000");
    }

    static CustomerRecord customerRecord(String country) {
        return new CustomerRecord(1, "Immanuel", "Madeline", "Kessler", "618 Deshaun Route", "Apt. 802",
                "Altenwerthshire", "NC", country, "27601", "(908)119-8310", "(212)693-8684", 20973888,
                "00000000000049368437", "1961-06-08", "0053581756", PrimaryCardHolder.fromCode("Y"), 704);
    }

    static CardXref xref(String cardNum) {
        return CardXref.from(new CardXrefRecord(cardNum, 1, 1L));
    }

    protected String user() {
        return bearer("USER0001", UserType.USER);
    }

    protected ResultActions view(String id) throws Exception {
        return mvc.perform(get(ACCOUNTS + "/" + id).header(HttpHeaders.AUTHORIZATION, user()));
    }

    /** The {@code updateForm} of the GET, as a client would start from it. */
    protected ObjectNode form() throws Exception {
        String body = view("1").andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
        return (ObjectNode) json.readTree(body).get("updateForm");
    }

    protected ResultActions update(String id, JsonNode body) throws Exception {
        return mvc.perform(put(ACCOUNTS + "/" + id).header(HttpHeaders.AUTHORIZATION, user())
                .contentType(MediaType.APPLICATION_JSON).content(json.writeValueAsString(body)));
    }

    /** Sets {@code path} ({@code a} or {@code a.b}) in the form. */
    static ObjectNode set(ObjectNode form, String path, String value) {
        String[] parts = path.split("\\.");
        ObjectNode target = parts.length == 1 ? form : (ObjectNode) form.get(parts[0]);
        target.put(parts[parts.length - 1], value);
        return form;
    }

    protected ResultActions change(String path, String value) throws Exception {
        return update("1", set(form(), path, value));
    }
}
