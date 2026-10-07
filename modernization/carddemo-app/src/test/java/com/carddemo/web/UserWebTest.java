package com.carddemo.web;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willAnswer;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;

import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRecord;
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
import org.springframework.dao.DuplicateKeyException;
import org.springframework.data.domain.Limit;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

/**
 * USRSEC in memory behind the mocked {@code UserSecurityRepository} (the real keyset defaults run on top): ADMIN001
 * and USER0001..USER0022, so pages of 10, 10 and 3 (page 1 ends at USER0009, page 2 at USER0019).
 */
abstract class UserWebTest extends OnlineWebTest {

    static final String USERS = "/api/v1/users";
    static final String ADMIN = "ADMIN001";

    protected final TreeMap<String, UserSecurity> store = new TreeMap<>();

    static String userId(int i) {
        return String.format("USER%04d", i);
    }

    static UserSecurity stored(String id, String first, String last, String password, UserType type) {
        UserSecurity user = UserSecurity.from(new UserSecurityRecord(id, first, last, password, type));
        ReflectionTestUtils.setField(user, "version", 0L);
        return user;
    }

    @BeforeEach
    void givenUsrsec() {
        store.clear();
        store.put(ADMIN, stored(ADMIN, "Ada", "Admin", "PASSWORD", UserType.ADMIN));
        for (int i = 1; i <= 22; i++) {
            store.put(userId(i), stored(userId(i), "First" + i, "Last" + i, "PASSWORD", UserType.USER));
        }
        given(users.findById(anyString()))
                .willAnswer(i -> Optional.ofNullable(store.get((String) i.getArgument(0))));
        given(users.existsById(anyString())).willAnswer(i -> store.containsKey((String) i.getArgument(0)));
        given(users.lockVersion(anyString())).willAnswer(i -> Optional.ofNullable(
                store.get((String) i.getArgument(0))).map(UserSecurity::getVersion));
        given(users.findByUsrIdGreaterThanEqualOrderByUsrIdAsc(anyString(), any(Limit.class)))
                .willAnswer(i -> asc(u -> u.getUsrId().compareTo(i.getArgument(0)) >= 0, i.getArgument(1)));
        given(users.findByUsrIdGreaterThanOrderByUsrIdAsc(anyString(), any(Limit.class)))
                .willAnswer(i -> asc(u -> u.getUsrId().compareTo(i.getArgument(0)) > 0, i.getArgument(1)));
        given(users.findByUsrIdLessThanOrderByUsrIdDesc(anyString(), any(Limit.class)))
                .willAnswer(i -> desc(u -> u.getUsrId().compareTo(i.getArgument(0)) < 0, i.getArgument(1)));
        given(users.browseFrom(anyString())).willCallRealMethod();
        given(users.nextPage(anyString())).willCallRealMethod();
        given(users.previousPage(anyString())).willCallRealMethod();
        given(users.saveAndFlush(any(UserSecurity.class))).willAnswer(i -> {
            UserSecurity u = i.getArgument(0);
            if (u.isNew()) {
                if (store.containsKey(u.getUsrId())) {
                    throw new DuplicateKeyException("duplicate key value violates unique constraint");
                }
                ReflectionTestUtils.setField(u, "version", 0L);
                ReflectionTestUtils.setField(u, "newRecord", false);
            } else {
                ReflectionTestUtils.setField(u, "version", u.getVersion() + 1);
            }
            store.put(u.getUsrId(), u);
            return u;
        });
        willAnswer(i -> store.remove(((UserSecurity) i.getArgument(0)).getUsrId()))
                .given(users).delete(any(UserSecurity.class));
    }

    private List<UserSecurity> asc(Predicate<UserSecurity> where, Limit limit) {
        return store.values().stream().filter(where).limit(limit.max()).toList();
    }

    private List<UserSecurity> desc(Predicate<UserSecurity> where, Limit limit) {
        return store.values().stream().filter(where).sorted(Comparator.comparing(UserSecurity::getUsrId).reversed())
                .limit(limit.max()).toList();
    }

    protected String admin() {
        return bearer(ADMIN, UserType.ADMIN);
    }

    protected String user() {
        return bearer("USER0001", UserType.USER);
    }

    protected JsonNode body(ResultActions result) throws Exception {
        return json.readTree(result.andReturn().getResponse().getContentAsString());
    }

    protected static MockHttpServletRequestBuilder params(MockHttpServletRequestBuilder request, String... params) {
        for (int i = 0; i < params.length; i += 2) {
            request.param(params[i], params[i + 1]);
        }
        return request;
    }

    protected ResultActions list(String... params) throws Exception {
        return mvc.perform(params(get(USERS).header(HttpHeaders.AUTHORIZATION, admin()), params));
    }

    static List<String> idsOf(JsonNode page) {
        List<String> ids = new ArrayList<>();
        page.get("rows").forEach(r -> ids.add(r.get("userId").asText()));
        return ids;
    }

    static List<String> users(int from, int to) {
        List<String> ids = new ArrayList<>();
        for (int i = from; i <= to; i++) {
            ids.add(userId(i));
        }
        return ids;
    }

    protected ResultActions select(List<String> ids, List<String> selections) throws Exception {
        List<Map<String, String>> rows = new ArrayList<>();
        for (int i = 0; i < ids.size(); i++) {
            Map<String, String> row = new LinkedHashMap<>();
            row.put("userId", ids.get(i));
            row.put("selection", selections.get(i));
            rows.add(row);
        }
        return mvc.perform(post(USERS + "/selection").header(HttpHeaders.AUTHORIZATION, admin())
                .contentType(MediaType.APPLICATION_JSON).content(json.writeValueAsString(Map.of("rows", rows))));
    }

    /** A COUSR1A form that passes every edit. */
    protected ObjectNode addForm() {
        return json.createObjectNode().put("firstName", "Jane").put("lastName", "Doe").put("userId", "JDOE01")
                .put("password", "SECRET12").put("userType", "U");
    }

    protected ResultActions add(JsonNode form) throws Exception {
        return mvc.perform(post(USERS).header(HttpHeaders.AUTHORIZATION, admin())
                .contentType(MediaType.APPLICATION_JSON).content(json.writeValueAsString(form)));
    }

    protected ResultActions fetch(String id, String... params) throws Exception {
        return mvc.perform(params(get(USERS + "/{id}", id).header(HttpHeaders.AUTHORIZATION, admin()), params));
    }

    /** The stored values of {@code id} as a COUSR2A form with its version. */
    protected ObjectNode updateForm(String id) {
        UserSecurity u = store.get(id);
        ObjectNode form = json.createObjectNode().put("firstName", u.getFirstName()).put("lastName", u.getLastName())
                .put("password", u.getPassword()).put("userType", u.getUsrType().code());
        form.put("version", u.getVersion());
        return form;
    }

    protected ResultActions update(String id, JsonNode form, String... params) throws Exception {
        return mvc.perform(params(put(USERS + "/{id}", id).header(HttpHeaders.AUTHORIZATION, admin())
                .contentType(MediaType.APPLICATION_JSON).content(json.writeValueAsString(form)), params));
    }

    protected ResultActions remove(String id, String... params) throws Exception {
        return mvc.perform(params(delete(USERS + "/{id}", id).header(HttpHeaders.AUTHORIZATION, admin()), params));
    }
}
