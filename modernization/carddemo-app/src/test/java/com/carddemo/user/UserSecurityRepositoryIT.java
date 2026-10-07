package com.carddemo.user;

import static com.carddemo.support.BrowseAssertions.assertFullBrowse;
import static com.carddemo.support.BrowseAssertions.sorted;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.Versions;
import com.carddemo.common.data.KeysetPage;
import com.carddemo.support.PostgresRepositoryTest;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.ObjectOptimisticLockingFailureException;

/** USRSEC access paths, including the COUSR00C ten-row browse. */
class UserSecurityRepositoryIT extends PostgresRepositoryTest {

    /** Mixed case, digits, embedded space and '~': VSAM orders by byte value, which en_US collation would not. */
    static final List<String> EXTRA_IDS = List.of("a", "a b", "ab", "B", "Z0", "b", "USER0011", "USER0012",
            "ADMIN00A", "0ZZZZZZZ", "~TILDE", "zz", "USER001");

    @Autowired
    UserSecurityRepository users;

    List<String> allIds;

    @BeforeEach
    void load() {
        loadSamples(Dataset.USRSEC);
        EXTRA_IDS.forEach(id -> users.save(UserSecurity.from(new UserSecurityRecord(id, "First", "Last", "PASSWORD",
                UserType.USER))));
        flushAndClear();
        allIds = new ArrayList<>(List.of("ADMIN001", "ADMIN002", "ADMIN003", "ADMIN004", "ADMIN005", "USER0001",
                "USER0002", "USER0003", "USER0004", "USER0005"));
        allIds.addAll(EXTRA_IDS);
        allIds = sorted(allIds);
    }

    @Test
    void readsByKeyWithLevelEightyEightTypeStoredAsItsCode() {
        UserSecurity admin = users.findById("ADMIN001").orElseThrow();
        assertThat(admin.getUsrType()).isEqualTo(UserType.ADMIN);
        assertThat(users.findById("USER0001").orElseThrow().getUsrType()).isEqualTo(UserType.USER);
        assertThat(admin.getVersion()).isZero();
        assertThat(users.findById("NOSUCH")).isEmpty();
        assertThat(jdbc.queryForObject("select usr_type from user_security where usr_id = 'ADMIN001'",
                String.class)).isEqualTo("A");
    }

    @Test
    void browsesTenUsersPerScreenInVsamKeyOrder() {
        assertThat(allIds).hasSize(23).startsWith("0ZZZZZZZ", "ADMIN001").endsWith("ab", "b", "zz", "~TILDE");
        assertFullBrowse(allIds, UserSecurityRepository.COUSR00C_SCREEN_ROWS, UserSecurity::getUsrId,
                users::browseFrom, users::nextPage, users::previousPage);
    }

    @Test
    void startbrIsGreaterOrEqual() {
        KeysetPage<UserSecurity> exact = users.browseFrom("USER0001");
        assertThat(exact.first().getUsrId()).isEqualTo("USER0001");
        KeysetPage<UserSecurity> between = users.browseFrom("USER00");
        assertThat(between.rows()).extracting(UserSecurity::getUsrId)
                .containsExactlyElementsOf(allIds.stream().filter(id -> id.compareTo("USER00") >= 0).limit(10)
                        .toList());
        assertThat(users.browseFrom("~~~~~~~~").rows()).isEmpty();
    }

    @Test
    void rewriteBumpsTheVersionAndStaleVersionsAreRejected() {
        UserSecurity user = users.findById("USER0001").orElseThrow();
        UserSecurityRecord changed = new UserSecurityRecord("USER0001", "New", "Name", "NEWPASS", UserType.ADMIN);
        user.update(changed);
        users.saveAndFlush(user);
        entityManager.clear();

        UserSecurity reread = users.findById("USER0001").orElseThrow();
        assertThat(reread.toRecord()).isEqualTo(changed);
        assertThat(reread.getVersion()).isEqualTo(1);
        assertThatThrownBy(() -> Versions.requireCurrent(UserSecurity.class, "USER0001", 0, reread.getVersion()))
                .isInstanceOf(ObjectOptimisticLockingFailureException.class);

        jdbc.update("update user_security set version = version + 1 where usr_id = 'USER0001'");
        reread.update(new UserSecurityRecord("USER0001", "Lost", "Update", "NEWPASS", UserType.USER));
        assertThatThrownBy(() -> users.saveAndFlush(reread))
                .isInstanceOf(ObjectOptimisticLockingFailureException.class);
    }

    @Test
    void updateCannotChangeTheKey() {
        UserSecurity user = users.findById("USER0001").orElseThrow();
        assertThatThrownBy(() -> user.update(new UserSecurityRecord("USER0002", "a", "b", "c", UserType.USER)))
                .isInstanceOf(IllegalArgumentException.class);
    }
}
