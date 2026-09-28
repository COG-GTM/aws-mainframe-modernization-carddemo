package com.carddemo.user;

import com.carddemo.common.KeysetPager;
import com.carddemo.common.PageQuery;
import com.carddemo.common.PageResponse;
import java.util.Map;
import java.util.Optional;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;

@Repository
public class UserRepository {

    private static final RowMapper<UserRecord> MAPPER = (rs, n) -> new UserRecord(
            rs.getString("user_id").strip(), rs.getString("first_name"), rs.getString("last_name"),
            rs.getString("password_hash"), rs.getString("user_type"), rs.getLong("version"));

    private final JdbcClient jdbc;
    private final KeysetPager pager;

    public UserRepository(JdbcClient jdbc, KeysetPager pager) {
        this.jdbc = jdbc;
        this.pager = pager;
    }

    public Optional<UserRecord> findById(String userId) {
        return jdbc.sql("SELECT * FROM user_security WHERE user_id = :id").param("id", userId).query(MAPPER)
                .optional();
    }

    public PageResponse<UserRecord> page(PageQuery query) {
        return pager.page("SELECT * FROM user_security", "user_id", null, Map.of(), query, MAPPER,
                UserRecord::userId);
    }

    public void insert(UserRecord user) {
        jdbc.sql("""
                INSERT INTO user_security (user_id, first_name, last_name, password_hash, user_type, version)
                VALUES (:id, :first, :last, :hash, :type, 0)""")
                .param("id", user.userId()).param("first", user.firstName()).param("last", user.lastName())
                .param("hash", user.passwordHash()).param("type", user.userType())
                .update();
    }

    public int update(UserRecord user, long expectedVersion) {
        return jdbc.sql("""
                UPDATE user_security SET first_name = :first, last_name = :last, password_hash = :hash,
                       user_type = :type, version = version + 1
                 WHERE user_id = :id AND version = :version""")
                .param("id", user.userId()).param("first", user.firstName()).param("last", user.lastName())
                .param("hash", user.passwordHash()).param("type", user.userType())
                .param("version", expectedVersion)
                .update();
    }

    public int delete(String userId) {
        return jdbc.sql("DELETE FROM user_security WHERE user_id = :id").param("id", userId).update();
    }
}
