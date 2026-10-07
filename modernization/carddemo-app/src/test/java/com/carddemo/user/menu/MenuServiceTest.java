package com.carddemo.user.menu;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.online.CsdInstalledPrograms;
import com.carddemo.common.online.InstalledPrograms;
import com.carddemo.common.online.MessageColor;
import com.carddemo.user.UserType;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/** Unit level of the COMEN01C/COADM01C selection logic, including branches the shipped tables cannot reach. */
class MenuServiceTest {

    private final MenuService service = new MenuService(new MenuCatalog(), new CsdInstalledPrograms());

    @ParameterizedTest
    @CsvSource(value = {"'1 ',01", "' 1',01", "1,01", "01,01", "'  ',00", "NULL,00", "'',00", "a,0a", "'a ',0a",
        "1a,1a", "' a',0a", "123,12"}, nullValues = "NULL")
    void COMEN01C_R7_normalisationKeepsUpToTheLastNonSpaceAndZeroFills(String input, String expected) {
        assertThat(MenuService.normalizeOption(input)).isEqualTo(expected);
    }

    @Test
    void COMEN01C_R9_adminOnlyRowIsRejectedForAUserOnly() {
        MenuDefinition main = new MenuDefinition("main", "COMEN01C", "CM00", "COMEN01", "COMEN1A", 12, true,
                List.of(new MenuOption(1, "User List (Security)", "COUSR00C", UserType.ADMIN)));
        assertThat(service.select(main, UserType.USER, "1")).isEqualTo(new MenuSelection.Rejected("01",
                MenuRejection.ADMIN_ONLY, "No access - Admin Only option..."));
        assertThat(service.select(main, UserType.ADMIN, "1")).isInstanceOf(MenuSelection.Transfer.class);
    }

    @Test
    void COADM01C_hasNoUserTypeCheck() {
        assertThat(service.select(MenuCatalog.ADMIN, UserType.USER, "1")).isInstanceOf(MenuSelection.Transfer.class);
    }

    @Test
    void COMEN01C_R10_installedPendingAuthorizationViewTransfers() {
        InstalledPrograms withAuth = id -> "COPAUS0C".equals(id) ? Optional.of("CPVS") : Optional.empty();
        MenuSelection selection = new MenuService(new MenuCatalog(), withAuth)
                .select(MenuCatalog.MAIN, UserType.USER, "11");
        assertThat(selection).isEqualTo(new MenuSelection.Transfer("11", MenuCatalog.MAIN.option(11), "CPVS",
                "COMEN01C", "CM00"));
    }

    @Test
    void COMEN01C_R10_notInstalledMessageUsesTheNameUpToTwoSpaces() {
        assertThat(service.select(MenuCatalog.MAIN, UserType.USER, "11")).isEqualTo(new MenuSelection.Info("11",
                "This option Pending Authorization View is not installed...", MessageColor.RED));
    }

    @Test
    void COMEN01C_R11_dummyRowUsesTheFirstWordWithoutASpace() {
        MenuDefinition main = new MenuDefinition("main", "COMEN01C", "CM00", "COMEN01", "COMEN1A", 12, true,
                List.of(new MenuOption(1, "Statements", "DUMMY", UserType.USER)));
        assertThat(service.select(main, UserType.USER, "1")).isEqualTo(new MenuSelection.Info("01",
                "This option Statementsis coming soon ...", MessageColor.GREEN));
    }

    @Test
    void COADM01C_R10_dummyRowAnswersNotInstalled() {
        MenuDefinition admin = new MenuDefinition("admin", "COADM01C", "CA00", "COADM01", "COADM1A", 12, false,
                List.of(new MenuOption(1, "Reserved", "DUMMY002", null)));
        assertThat(service.select(admin, UserType.ADMIN, "1")).isEqualTo(new MenuSelection.Info("01",
                "This option is not installed ...", MessageColor.GREEN));
    }

    @Test
    void aTableLargerThanTheMapIsRefused() {
        List<MenuOption> thirteen = java.util.stream.IntStream.rangeClosed(1, 13)
                .mapToObj(n -> new MenuOption(n, "X", "COSGN00C", UserType.USER)).toList();
        org.assertj.core.api.Assertions.assertThatIllegalArgumentException().isThrownBy(() ->
                new MenuDefinition("main", "COMEN01C", "CM00", "COMEN01", "COMEN1A", 12, true, thirteen));
    }
}
