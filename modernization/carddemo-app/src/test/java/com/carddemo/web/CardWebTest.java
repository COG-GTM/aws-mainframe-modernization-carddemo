package com.carddemo.web;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.card.Card;
import com.carddemo.card.CardRecord;
import com.carddemo.card.CardStatus;
import com.carddemo.user.UserType;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.function.Predicate;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.data.domain.Limit;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

/**
 * CARDDAT in memory behind the mocked {@code CardRepository} (the real keyset defaults run on top): cards 1..9 belong
 * to account 1 (a full page plus two), cards 10..16 to account 2 (exactly seven), card 17 to account 3. Unfiltered,
 * the 17 cards are pages of 7, 7 and 3.
 */
abstract class CardWebTest extends OnlineWebTest {

    static final String CARDS = "/api/v1/cards";
    static final String ADMIN = "ADMIN001";
    static final String USER = "USER0001";

    protected final TreeMap<String, Card> store = new TreeMap<>();

    static String pan(int i) {
        return String.format("4000%012d", i * 1_000_003L);
    }

    static long account(int i) {
        return i <= 9 ? 1L : i <= 16 ? 2L : 3L;
    }

    static String masked(int i) {
        String p = pan(i);
        return "************" + p.substring(12);
    }

    @BeforeEach
    void givenCarddat() {
        store.clear();
        for (int i = 1; i <= 17; i++) {
            Card card = Card.from(new CardRecord(pan(i), account(i), 100 + i, i == 1 ? "Aniya Von" : "Holder Name",
                    "2025-03-14", CardStatus.fromCode(i == 5 ? "N" : "Y")));
            ReflectionTestUtils.setField(card, "version", 0L);
            store.put(card.getCardNum(), card);
        }
        given(cards.findById(anyString())).willAnswer(i -> Optional.ofNullable(store.get((String) i.getArgument(0))));
        given(cards.findByCardNumGreaterThanEqualOrderByCardNumAsc(anyString(), any(Limit.class)))
                .willAnswer(i -> asc(c -> c.getCardNum().compareTo(i.getArgument(0)) >= 0, i.getArgument(1)));
        given(cards.findByCardNumGreaterThanOrderByCardNumAsc(anyString(), any(Limit.class)))
                .willAnswer(i -> asc(c -> c.getCardNum().compareTo(i.getArgument(0)) > 0, i.getArgument(1)));
        given(cards.findByCardNumLessThanOrderByCardNumDesc(anyString(), any(Limit.class)))
                .willAnswer(i -> desc(c -> c.getCardNum().compareTo(i.getArgument(0)) < 0, i.getArgument(1)));
        given(cards.findByAcctIdAndCardNumGreaterThanEqualOrderByCardNumAsc(anyLong(), anyString(), any(Limit.class)))
                .willAnswer(i -> asc(c -> c.getAcctId() == (long) i.getArgument(0)
                        && c.getCardNum().compareTo(i.getArgument(1)) >= 0, i.getArgument(2)));
        given(cards.findByAcctIdAndCardNumGreaterThanOrderByCardNumAsc(anyLong(), anyString(), any(Limit.class)))
                .willAnswer(i -> asc(c -> c.getAcctId() == (long) i.getArgument(0)
                        && c.getCardNum().compareTo(i.getArgument(1)) > 0, i.getArgument(2)));
        given(cards.findByAcctIdAndCardNumLessThanOrderByCardNumDesc(anyLong(), anyString(), any(Limit.class)))
                .willAnswer(i -> desc(c -> c.getAcctId() == (long) i.getArgument(0)
                        && c.getCardNum().compareTo(i.getArgument(1)) < 0, i.getArgument(2)));
        given(cards.browseFrom(anyString())).willCallRealMethod();
        given(cards.nextPage(anyString())).willCallRealMethod();
        given(cards.previousPage(anyString())).willCallRealMethod();
        given(cards.browseFrom(anyLong(), anyString())).willCallRealMethod();
        given(cards.nextPage(anyLong(), anyString())).willCallRealMethod();
        given(cards.previousPage(anyLong(), anyString())).willCallRealMethod();
        given(cards.findFirstByAcctIdOrderByCardNumAsc(anyLong())).willAnswer(i -> store.values().stream()
                .filter(c -> c.getAcctId() == (long) i.getArgument(0)).findFirst());
        given(cards.lockVersion(anyString()))
                .willAnswer(i -> Optional.ofNullable(store.get((String) i.getArgument(0))).map(Card::getVersion));
        given(cards.saveAndFlush(any(Card.class))).willAnswer(i -> {
            Card card = i.getArgument(0);
            ReflectionTestUtils.setField(card, "version", card.getVersion() + 1);
            return card;
        });
    }

    private List<Card> asc(Predicate<Card> where, Limit limit) {
        return store.values().stream().filter(where).limit(limit.max()).toList();
    }

    private List<Card> desc(Predicate<Card> where, Limit limit) {
        return store.values().stream().filter(where).sorted(Comparator.comparing(Card::getCardNum).reversed())
                .limit(limit.max()).toList();
    }

    protected String admin() {
        return bearer(ADMIN, UserType.ADMIN);
    }

    protected String user() {
        return bearer(USER, UserType.USER);
    }

    protected ResultActions list(String token, String... params) throws Exception {
        MockHttpServletRequestBuilder request = get(CARDS).header(HttpHeaders.AUTHORIZATION, token);
        for (int i = 0; i < params.length; i += 2) {
            request.param(params[i], params[i + 1]);
        }
        return mvc.perform(request);
    }

    protected JsonNode body(ResultActions result) throws Exception {
        return json.readTree(result.andReturn().getResponse().getContentAsString());
    }

    protected JsonNode page(String token, String... params) throws Exception {
        return body(list(token, params).andExpect(status().isOk()));
    }

    protected ResultActions select(String token, List<String> refs, List<String> actions) throws Exception {
        return select(token, null, refs, actions);
    }

    protected ResultActions select(String token, String accountId, List<String> refs, List<String> actions)
            throws Exception {
        List<Map<String, String>> rows = new ArrayList<>();
        for (int i = 0; i < refs.size(); i++) {
            Map<String, String> row = new LinkedHashMap<>();
            row.put("cardRef", refs.get(i));
            row.put("action", actions.get(i));
            rows.add(row);
        }
        return mvc.perform(post(CARDS + "/selection").header(HttpHeaders.AUTHORIZATION, token)
                .contentType(MediaType.APPLICATION_JSON).content(json.writeValueAsString(accountId == null ? Map.of("rows", rows)
                        : Map.of("accountId", accountId, "rows", rows))));
    }

    protected ResultActions view(String token, String cardNumber, String accountId, String... params)
            throws Exception {
        MockHttpServletRequestBuilder request = get(CARDS + "/" + cardNumber).header(HttpHeaders.AUTHORIZATION, token);
        if (accountId != null) {
            request.param("accountId", accountId);
        }
        for (int i = 0; i < params.length; i += 2) {
            request.param(params[i], params[i + 1]);
        }
        return mvc.perform(request);
    }

    /** The {@code updateForm} of the GET of card {@code i}, as a client would start from it. */
    protected ObjectNode form(int i) throws Exception {
        JsonNode detail = body(view(admin(), pan(i), String.valueOf(account(i))).andExpect(status().isOk()));
        return (ObjectNode) detail.get("updateForm");
    }

    protected ResultActions update(String token, String cardNumber, JsonNode body, String... params)
            throws Exception {
        MockHttpServletRequestBuilder request = put(CARDS + "/" + cardNumber).header(HttpHeaders.AUTHORIZATION, token)
                .contentType(MediaType.APPLICATION_JSON).content(json.writeValueAsString(body));
        for (int i = 0; i < params.length; i += 2) {
            request.param(params[i], params[i + 1]);
        }
        return mvc.perform(request);
    }

    /** Card 1 with {@code field} changed, ENTER ({@code confirm=false}) unless {@code confirm}. */
    protected ResultActions change(String field, String value, boolean confirm) throws Exception {
        ObjectNode form = form(1);
        form.put(field, value);
        form.put("confirm", confirm);
        return update(admin(), pan(1), form);
    }

    static List<String> refsOf(JsonNode page) {
        List<String> refs = new ArrayList<>();
        page.get("rows").forEach(r -> refs.add(r.get("cardRef").asText()));
        return refs;
    }

    static List<String> accountsOf(JsonNode page) {
        List<String> accounts = new ArrayList<>();
        page.get("rows").forEach(r -> accounts.add(r.get("accountId").asText()));
        return accounts;
    }

    static List<String> maskedOf(JsonNode page) {
        List<String> masked = new ArrayList<>();
        page.get("rows").forEach(r -> masked.add(r.get("cardNumber").asText()));
        return masked;
    }

    static List<String> maskedRange(int from, int to) {
        List<String> masked = new ArrayList<>();
        for (int i = from; i <= to; i++) {
            masked.add(masked(i));
        }
        return masked;
    }
}
