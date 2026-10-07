package com.carddemo.web;

import static org.hamcrest.Matchers.nullValue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.user.UserType;
import com.carddemo.user.menu.MenuCatalog;
import com.carddemo.user.menu.MenuDefinition;
import com.carddemo.user.menu.MenuOption;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Primary;
import org.springframework.http.HttpHeaders;

/**
 * COMEN01C rules that depend on table content the shipped {@code COMEN02Y} lacks: an admin-only row (R-9) and a
 * {@code DUMMY} row (R-11). The program logic is table-driven, so it is exercised with a variant table.
 */
@Import(MainMenuTableVariantTest.VariantCatalog.class)
class MainMenuTableVariantTest extends OnlineWebTest {

    @TestConfiguration
    static class VariantCatalog {

        @Bean
        @Primary
        MenuCatalog variantCatalog() {
            return new MenuCatalog(new MenuDefinition("main", "COMEN01C", "CM00", "COMEN01", "COMEN1A", 12, true,
                    List.of(new MenuOption(1, "Account View", "COACTVWC", UserType.USER),
                            new MenuOption(2, "User List (Security)", "COUSR00C", UserType.ADMIN),
                            new MenuOption(3, "Account Statements", "DUMMY001", UserType.USER))),
                    MenuCatalog.ADMIN);
        }
    }

    @Test
    void R9_anAdminOnlyOptionIsRejectedForAUserWithTheExactMessage() throws Exception {
        mvc.perform(select("main", "2", UserType.USER))
                .andExpect(status().isForbidden())
                .andExpect(jsonPath("$.code").value("NOTAUTH"))
                .andExpect(jsonPath("$.field").value("option"))
                .andExpect(jsonPath("$.option").value("02"))
                .andExpect(jsonPath("$.message").value("No access - Admin Only option..."));
    }

    @Test
    void R9_theSameOptionTransfersForAnAdministrator() throws Exception {
        mvc.perform(select("main", "2", UserType.ADMIN))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation.toProgram").value("COUSR00C"));
    }

    @Test
    void R9_theMenuFlagsAdminOnlyOptions() throws Exception {
        mvc.perform(get(MENU + "/main").header(HttpHeaders.AUTHORIZATION, bearer("USER0001", UserType.USER)))
                .andExpect(jsonPath("$.options[1].adminOnly").value(true))
                .andExpect(jsonPath("$.options[0].adminOnly").value(false));
    }

    @Test
    void R11_aDummyRowAnswersComingSoonWithTheFirstWordOfItsName() throws Exception {
        mvc.perform(select("main", "3", UserType.USER))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation").value(nullValue()))
                .andExpect(jsonPath("$.message").value("This option Accountis coming soon ..."))
                .andExpect(jsonPath("$.messageColor").value("GREEN"));
    }
}
