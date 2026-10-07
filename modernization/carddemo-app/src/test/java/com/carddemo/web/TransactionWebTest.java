package com.carddemo.web;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.account.Account;
import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountStatus;
import com.carddemo.card.CardXref;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.TransactionCategoryId;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.user.UserType;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Predicate;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.data.domain.Limit;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

/**
 * TRANSACT in memory behind the mocked {@code TransactionRepository} (the real keyset defaults run on top): 23
 * transactions, ids 1..23, so pages of 10, 10 and 3. ACCTDAT has account 1 (balance 194.00), 2 (0.00), 3 (-5.00)
 * and 4 (25.00, no card in CXACAIX); CARDXREF links card {@link #card(long)} to account n for n = 1..3. TRANTYPE
 * knows 01 and 02, TRANCATG 01/0001, 01/0002 and 02/0002.
 */
abstract class TransactionWebTest extends OnlineWebTest {

    static final String TRANSACTIONS = "/api/v1/transactions";
    static final String ADMIN = "ADMIN001";
    static final String USER = "USER0001";
    static final String LONG_DESCRIPTION = "Purchase at a store with a very long description";
    static final Set<String> TYPES = Set.of("01", "02");
    static final Set<TransactionCategoryId> CATEGORIES = Set.of(new TransactionCategoryId("01", 1),
            new TransactionCategoryId("01", 2), new TransactionCategoryId("02", 2));

    protected final TreeMap<String, Transaction> store = new TreeMap<>();
    protected final Map<Long, Account> accountStore = new HashMap<>();

    static String id(int i) {
        return String.format("%016d", i);
    }

    static String card(long account) {
        return String.format("41110000000000%02d", account);
    }

    static TransactionRecord record(int i) {
        return new TransactionRecord(id(i), "01", 1, "POS TERM", i == 1 ? LONG_DESCRIPTION : "Purchase " + i,
                i == 2 ? new BigDecimal("-120.00") : new BigDecimal("45.10"), 100 + i, "Merchant " + i, "Seattle",
                "98101", card(1), String.format("2022-06-%02d 10:11:12.000000", i), "2022-07-06 13:45:10.000000");
    }

    static Account account(long id, String balance) {
        Account account = Account.from(new AccountRecord(id, AccountStatus.fromCode("Y"), new BigDecimal(balance),
                new BigDecimal("20200.00"), new BigDecimal("10200.00"), "2014-11-20", "2025-05-20", "2025-05-20",
                new BigDecimal("0.00"), new BigDecimal("0.00"), "", "A000000000"));
        ReflectionTestUtils.setField(account, "version", 0L);
        return account;
    }

    static CardXref xref(long account) {
        return CardXref.from(new CardXrefRecord(card(account), (int) account, account));
    }

    @BeforeEach
    void givenTransactAcctdatAndXref() {
        store.clear();
        for (int i = 1; i <= 23; i++) {
            store.put(id(i), Transaction.from(record(i)));
        }
        given(transactions.findById(anyString()))
                .willAnswer(i -> Optional.ofNullable(store.get((String) i.getArgument(0))));
        given(transactions.findByTranIdGreaterThanEqualOrderByTranIdAsc(anyString(), any(Limit.class)))
                .willAnswer(i -> asc(t -> t.getTranId().compareTo(i.getArgument(0)) >= 0, i.getArgument(1)));
        given(transactions.findByTranIdGreaterThanOrderByTranIdAsc(anyString(), any(Limit.class)))
                .willAnswer(i -> asc(t -> t.getTranId().compareTo(i.getArgument(0)) > 0, i.getArgument(1)));
        given(transactions.findByTranIdLessThanOrderByTranIdDesc(anyString(), any(Limit.class)))
                .willAnswer(i -> desc(t -> t.getTranId().compareTo(i.getArgument(0)) < 0, i.getArgument(1)));
        given(transactions.browseFrom(anyString())).willCallRealMethod();
        given(transactions.nextPage(anyString())).willCallRealMethod();
        given(transactions.previousPage(anyString())).willCallRealMethod();
        given(transactions.findFirstByOrderByTranIdDesc())
                .willAnswer(i -> store.isEmpty() ? Optional.empty() : Optional.of(store.lastEntry().getValue()));
        given(transactions.lockIdAssignment(anyLong())).willReturn(1);
        given(transactions.saveAndFlush(any(Transaction.class))).willAnswer(i -> {
            Transaction t = i.getArgument(0);
            if (store.containsKey(t.getTranId())) {
                throw new DuplicateKeyException("duplicate key value violates unique constraint \"transaction_pk\"");
            }
            store.put(t.getTranId(), t);
            return t;
        });
        given(types.existsById(anyString())).willAnswer(i -> TYPES.contains((String) i.getArgument(0)));
        given(categories.existsById(any(TransactionCategoryId.class)))
                .willAnswer(i -> CATEGORIES.contains((TransactionCategoryId) i.getArgument(0)));

        accountStore.clear();
        accountStore.put(1L, account(1, "194.00"));
        accountStore.put(2L, account(2, "0.00"));
        accountStore.put(3L, account(3, "-5.00"));
        accountStore.put(4L, account(4, "25.00"));
        given(accounts.findById(anyLong()))
                .willAnswer(i -> Optional.ofNullable(accountStore.get((Long) i.getArgument(0))));
        given(accounts.lockVersion(anyLong())).willAnswer(i -> Optional.ofNullable(
                accountStore.get((Long) i.getArgument(0))).map(Account::getVersion));
        given(accounts.saveAndFlush(any(Account.class))).willAnswer(i -> {
            Account a = i.getArgument(0);
            ReflectionTestUtils.setField(a, "version", a.getVersion() + 1);
            return a;
        });
        for (long a = 1; a <= 3; a++) {
            given(xrefs.findFirstByAcctIdOrderByCardNumAsc(a)).willReturn(Optional.of(xref(a)));
            given(xrefs.findById(card(a))).willReturn(Optional.of(xref(a)));
        }
    }

    private List<Transaction> asc(Predicate<Transaction> where, Limit limit) {
        return store.values().stream().filter(where).limit(limit.max()).toList();
    }

    private List<Transaction> desc(Predicate<Transaction> where, Limit limit) {
        return store.values().stream().filter(where).sorted(Comparator.comparing(Transaction::getTranId).reversed())
                .limit(limit.max()).toList();
    }

    protected String admin() {
        return bearer(ADMIN, UserType.ADMIN);
    }

    protected String user() {
        return bearer(USER, UserType.USER);
    }

    protected JsonNode body(ResultActions result) throws Exception {
        return json.readTree(result.andReturn().getResponse().getContentAsString());
    }

    protected ResultActions list(String token, String... params) throws Exception {
        MockHttpServletRequestBuilder request = get(TRANSACTIONS).header(HttpHeaders.AUTHORIZATION, token);
        for (int i = 0; i < params.length; i += 2) {
            request.param(params[i], params[i + 1]);
        }
        return mvc.perform(request);
    }

    protected JsonNode page(String... params) throws Exception {
        return body(list(user(), params).andExpect(status().isOk()));
    }

    static List<String> idsOf(JsonNode page) {
        List<String> ids = new ArrayList<>();
        page.get("rows").forEach(r -> ids.add(r.get("tranId").asText()));
        return ids;
    }

    static List<String> ids(int from, int to) {
        List<String> ids = new ArrayList<>();
        for (int i = from; i <= to; i++) {
            ids.add(id(i));
        }
        return ids;
    }

    protected ResultActions select(List<String> tranIds, List<String> selections) throws Exception {
        List<Map<String, String>> rows = new ArrayList<>();
        for (int i = 0; i < tranIds.size(); i++) {
            Map<String, String> row = new LinkedHashMap<>();
            row.put("tranId", tranIds.get(i));
            row.put("selection", selections.get(i));
            rows.add(row);
        }
        return mvc.perform(post(TRANSACTIONS + "/selection").header(HttpHeaders.AUTHORIZATION, user())
                .contentType(MediaType.APPLICATION_JSON).content(json.writeValueAsString(Map.of("rows", rows))));
    }

    protected ResultActions view(String tranId, String... params) throws Exception {
        MockHttpServletRequestBuilder request = get(TRANSACTIONS + "/{id}", tranId)
                .header(HttpHeaders.AUTHORIZATION, user());
        for (int i = 0; i < params.length; i += 2) {
            request.param(params[i], params[i + 1]);
        }
        return mvc.perform(request);
    }

    /** A COTRN2A form that passes every edit for account 1, CONFIRM blank. */
    protected ObjectNode form() {
        ObjectNode form = json.createObjectNode();
        form.put("accountId", "1");
        form.put("cardNumber", "");
        form.put("typeCode", "01");
        form.put("categoryCode", "0001");
        form.put("source", "POS TERM");
        form.put("description", "Online purchase");
        form.put("amount", "-00000012.34");
        form.put("origDate", "2022-07-06");
        form.put("procDate", "2022-07-06");
        form.put("merchantId", "000000001");
        form.put("merchantName", "Corner Store");
        form.put("merchantCity", "Seattle");
        form.put("merchantZip", "98101");
        form.put("confirm", "");
        return form;
    }

    protected ResultActions add(JsonNode form, String... params) throws Exception {
        MockHttpServletRequestBuilder request = post(TRANSACTIONS).header(HttpHeaders.AUTHORIZATION, user())
                .contentType(MediaType.APPLICATION_JSON).content(json.writeValueAsString(form));
        for (int i = 0; i < params.length; i += 2) {
            request.param(params[i], params[i + 1]);
        }
        return mvc.perform(request);
    }

    protected ResultActions addWith(String field, String value) throws Exception {
        return add(form().put(field, value));
    }

    protected ResultActions pay(String accountId, String confirm, Long version, String... params) throws Exception {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("confirm", confirm);
        body.put("version", version);
        MockHttpServletRequestBuilder request = post("/api/v1/accounts/{id}/bill-payment", accountId)
                .header(HttpHeaders.AUTHORIZATION, user()).contentType(MediaType.APPLICATION_JSON)
                .content(json.writeValueAsString(body));
        for (int i = 0; i < params.length; i += 2) {
            request.param(params[i], params[i + 1]);
        }
        return mvc.perform(request);
    }
}
