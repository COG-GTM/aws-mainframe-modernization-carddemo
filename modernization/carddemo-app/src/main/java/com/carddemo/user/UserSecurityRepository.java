package com.carddemo.user;

import com.carddemo.common.data.KeysetPage;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Limit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

/**
 * USRSEC access paths: {@code READ} by SEC-USR-ID (COSGN00C, COUSR02C, COUSR03C), {@code WRITE}/{@code REWRITE}/
 * {@code DELETE} via {@link JpaRepository}, and the COUSR00C browse ({@code STARTBR}/{@code READNEXT}/
 * {@code READPREV}, ten users per screen) as keyset queries on {@code usr_id} (ADR-0011).
 */
public interface UserSecurityRepository extends JpaRepository<UserSecurity, String> {

    /** COUSR00C {@code WS-IDX > 10}: users per screen. */
    int COUSR00C_SCREEN_ROWS = 10;

    /** {@code STARTBR GTEQ} + {@code READNEXT}: the start key itself is included. */
    List<UserSecurity> findByUsrIdGreaterThanEqualOrderByUsrIdAsc(String startUsrId, Limit limit);

    /** PF8: {@code STARTBR} at the last user shown, one {@code READNEXT} skipped, then the following users. */
    List<UserSecurity> findByUsrIdGreaterThanOrderByUsrIdAsc(String lastUsrIdShown, Limit limit);

    /** PF7: {@code STARTBR} at the first user shown, one {@code READPREV} skipped, then descending key order. */
    List<UserSecurity> findByUsrIdLessThanOrderByUsrIdDesc(String firstUsrIdShown, Limit limit);

    /** ENTER / first display; {@code startUsrId} is {@code ""} (LOW-VALUES) when no user id was typed. */
    default KeysetPage<UserSecurity> browseFrom(String startUsrId) {
        return KeysetPage.forward(l -> findByUsrIdGreaterThanEqualOrderByUsrIdAsc(startUsrId, l), COUSR00C_SCREEN_ROWS);
    }

    default KeysetPage<UserSecurity> nextPage(String lastUsrIdShown) {
        return KeysetPage.forward(l -> findByUsrIdGreaterThanOrderByUsrIdAsc(lastUsrIdShown, l), COUSR00C_SCREEN_ROWS);
    }

    default KeysetPage<UserSecurity> previousPage(String firstUsrIdShown) {
        return KeysetPage.backward(l -> findByUsrIdLessThanOrderByUsrIdDesc(firstUsrIdShown, l), COUSR00C_SCREEN_ROWS);
    }

    /**
     * {@code READ ... UPDATE} (COUSR02C/COUSR03C): row lock + current version, re-checked against the version the
     * client was shown before the REWRITE/DELETE (ADR-0010).
     */
    @Query(value = "select version from user_security where usr_id = :usrId for update", nativeQuery = true)
    Optional<Long> lockVersion(@Param("usrId") String usrId);
}
