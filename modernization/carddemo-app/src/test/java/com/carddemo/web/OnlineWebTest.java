package com.carddemo.web;

import static org.mockito.BDDMockito.given;

import com.carddemo.account.AccountRepository;
import com.carddemo.account.online.AccountLookup;
import com.carddemo.account.online.AccountUpdateService;
import com.carddemo.batch.report.TransactionReportEdits;
import com.carddemo.batch.report.TransactionReportLauncher;
import com.carddemo.batch.report.TransactionReportService;
import com.carddemo.card.CardRepository;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.card.online.CardBrowse;
import com.carddemo.card.online.CardLookup;
import com.carddemo.card.online.CardUpdateService;
import com.carddemo.common.online.CsdInstalledPrograms;
import com.carddemo.common.time.ClockConfiguration;
import com.carddemo.common.time.ClockProperties;
import com.carddemo.customer.CustomerRepository;
import com.carddemo.transaction.TransactionCategoryRepository;
import com.carddemo.transaction.TransactionRepository;
import com.carddemo.transaction.TransactionTypeRepository;
import com.carddemo.transaction.online.BillPaymentService;
import com.carddemo.transaction.online.TransactionAddEdits;
import com.carddemo.transaction.online.TransactionAddService;
import com.carddemo.transaction.online.TransactionBrowse;
import com.carddemo.transaction.online.TransactionIds;
import com.carddemo.transaction.online.TransactionLookup;
import com.carddemo.user.UserPasswords;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRecord;
import com.carddemo.user.UserSecurityRepository;
import com.carddemo.user.UserType;
import com.carddemo.user.admin.UserAddService;
import com.carddemo.user.admin.UserDeleteService;
import com.carddemo.user.admin.UserListBrowse;
import com.carddemo.user.admin.UserLookup;
import com.carddemo.user.admin.UserUpdateService;
import com.carddemo.user.menu.MenuCatalog;
import com.carddemo.user.menu.MenuService;
import com.carddemo.user.signon.SignOnService;
import com.carddemo.web.account.AccountController;
import com.carddemo.web.card.CardController;
import com.carddemo.web.card.CardReferences;
import com.carddemo.web.menu.MenuController;
import com.carddemo.web.report.TransactionReportController;
import com.carddemo.web.security.JwtProperties;
import com.carddemo.web.security.SecurityConfiguration;
import com.carddemo.web.security.TokenService;
import com.carddemo.web.signon.SignOnController;
import com.carddemo.web.transaction.BillPaymentController;
import com.carddemo.web.transaction.TransactionController;
import com.carddemo.web.user.UserAdminController;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

/**
 * MockMvc slice of the online API with the real security chain, sign-on/menu services and the USRSEC repository
 * mocked. The business clock is pinned so screen headers are deterministic.
 */
@WebMvcTest(controllers = {SignOnController.class, MenuController.class, AccountController.class,
        CardController.class, TransactionController.class, BillPaymentController.class, UserAdminController.class,
        TransactionReportController.class})
@Import({SecurityConfiguration.class, TokenService.class, SignOnService.class, UserPasswords.class, MenuService.class, MenuCatalog.class,
        CsdInstalledPrograms.class, ScreenHeaders.class, ClockConfiguration.class, AccountLookup.class,
        AccountUpdateService.class, CardBrowse.class, CardLookup.class, CardUpdateService.class, CardReferences.class,
        TransactionBrowse.class, TransactionLookup.class, TransactionAddEdits.class, TransactionIds.class,
        TransactionAddService.class, BillPaymentService.class, UserListBrowse.class, UserLookup.class,
        UserAddService.class, UserUpdateService.class, UserDeleteService.class, TransactionReportEdits.class,
        TransactionReportService.class})
@EnableConfigurationProperties({JwtProperties.class, OnlineProperties.class, ClockProperties.class})
@ActiveProfiles("test")
@TestPropertySource(properties = {"carddemo.clock.fixed=2022-07-06T13:45:10", "carddemo.online.applid=CARDDEMO",
        "carddemo.online.sysid=CDMO"})
public abstract class OnlineWebTest {

    public static final String LOGIN = "/api/v1/auth/login";
    public static final String MENU = "/api/v1/menu";

    @Autowired
    protected MockMvc mvc;

    @Autowired
    protected TokenService tokens;

    @Autowired
    protected ObjectMapper json;

    @MockBean
    protected UserSecurityRepository users;

    @MockBean
    protected AccountRepository accounts;

    @MockBean
    protected CustomerRepository customers;

    @MockBean
    protected CardXrefRepository xrefs;

    @MockBean
    protected CardRepository cards;

    @MockBean
    protected TransactionRepository transactions;

    @MockBean
    protected TransactionTypeRepository types;

    @MockBean
    protected TransactionCategoryRepository categories;

    @MockBean
    protected TransactionReportLauncher reportLauncher;

    protected void givenUser(String userId, String password, UserType type) {
        given(users.findById(userId)).willReturn(Optional.of(
                UserSecurity.from(new UserSecurityRecord(userId, "First", "Last", password, type))));
    }

    /** A token for {@code userId}; an ADMIN is also still an administrator in the mocked USRSEC (ADR-0023 check). */
    protected String bearer(String userId, UserType type) {
        if (type == UserType.ADMIN) {
            given(users.findUsrTypeByUsrId(userId)).willReturn(Optional.of(UserType.ADMIN));
        }
        return "Bearer " + tokens.issue(userId, type).value();
    }

    protected MockHttpServletRequestBuilder login(String userId, String password) throws Exception {
        Map<String, String> body = new LinkedHashMap<>();
        body.put("userId", userId);
        body.put("password", password);
        return MockMvcRequestBuilders.post(LOGIN).contentType(MediaType.APPLICATION_JSON)
                .content(json.writeValueAsString(body));
    }

    protected MockHttpServletRequestBuilder select(String menu, String option, UserType as) throws Exception {
        Map<String, String> body = new LinkedHashMap<>();
        body.put("option", option);
        return MockMvcRequestBuilders.post(MENU + "/" + menu + "/selection")
                .header(HttpHeaders.AUTHORIZATION, bearer(as == UserType.ADMIN ? "ADMIN001" : "USER0001", as))
                .contentType(MediaType.APPLICATION_JSON).content(json.writeValueAsString(body));
    }
}
