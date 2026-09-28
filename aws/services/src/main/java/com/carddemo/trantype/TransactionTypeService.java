package com.carddemo.trantype;

import com.carddemo.common.ApiException;
import com.carddemo.common.ErrorCode;
import com.carddemo.common.KeysetPager;
import com.carddemo.common.LegacyMessages;
import com.carddemo.common.PageQuery;
import com.carddemo.common.PageResponse;
import com.carddemo.common.Text;
import com.carddemo.common.ValidationErrors;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COTRTLIC (list/update/delete) and COTRTUPC (maintenance) over TRANSACTION_TYPE / TRANSACTION_TYPE_CATEGORY. */
@Service
public class TransactionTypeService {

    static final int DEFAULT_PAGE_SIZE = 7;
    static final String LIST_PROGRAM = "COTRTLIC";
    static final String MAINT_PROGRAM = "COTRTUPC";

    public record TransactionType(String typeCd, String description, long version) {
    }

    public record TransactionCategory(String typeCd, int catCd, String description) {
    }

    public record CreateRequest(String typeCd, String description) {
    }

    public record UpdateRequest(String description, Long version) {
    }

    private static final RowMapper<TransactionType> MAPPER = (rs, n) -> new TransactionType(
            rs.getString("type_cd"), rs.getString("description"), rs.getLong("version"));

    private final JdbcClient jdbc;
    private final KeysetPager pager;

    public TransactionTypeService(JdbcClient jdbc, KeysetPager pager) {
        this.jdbc = jdbc;
        this.pager = pager;
    }

    @Transactional(readOnly = true)
    public PageResponse<TransactionType> list(String typeCdIn, String descriptionIn, String startKey,
            String direction, Integer pageSize) {
        List<String> filters = new ArrayList<>();
        Map<String, Object> params = new HashMap<>();
        if (!Text.isBlank(typeCdIn)) {
            filters.add("type_cd = :typeCd");
            params.put("typeCd", normalizeTypeCd(typeCdIn, LIST_PROGRAM));
        }
        if (!Text.isBlank(descriptionIn)) {
            filters.add("UPPER(description) LIKE :desc");
            params.put("desc", "%" + descriptionIn.strip().toUpperCase() + "%");
        }
        PageQuery query = PageQuery.of(startKey, direction, pageSize, DEFAULT_PAGE_SIZE, LIST_PROGRAM);
        PageResponse<TransactionType> page = pager.page("SELECT * FROM transaction_type", "type_cd",
                String.join(" AND ", filters), params, query, MAPPER, TransactionType::typeCd);
        String message = null;
        if (page.items().isEmpty()) {
            message = query.startKey() == null ? "No records found for this search condition."
                    : query.direction() == PageQuery.Direction.NEXT ? "No more pages to display"
                            : "No previous pages to display";
        }
        return new PageResponse<>(page.items(), page.firstKey(), page.lastKey(), page.hasNext(), page.hasPrev(),
                message);
    }

    @Transactional(readOnly = true)
    public TransactionType get(String typeCdIn) {
        String typeCd = normalizeTypeCd(typeCdIn, MAINT_PROGRAM);
        return find(typeCd).orElseThrow(() -> notFound());
    }

    @Transactional(readOnly = true)
    public List<TransactionCategory> categories(String typeCdIn) {
        String typeCd = normalizeTypeCd(typeCdIn, MAINT_PROGRAM);
        find(typeCd).orElseThrow(() -> notFound());
        return jdbc.sql("SELECT type_cd, cat_cd, description FROM transaction_category WHERE type_cd = :t "
                + "ORDER BY cat_cd")
                .param("t", typeCd)
                .query((rs, n) -> new TransactionCategory(rs.getString("type_cd"), rs.getInt("cat_cd"),
                        rs.getString("description")))
                .list();
    }

    @Transactional
    public TransactionType create(CreateRequest request) {
        if (request == null) {
            throw new ApiException(ErrorCode.INVALID_REQUEST, "No input received", MAINT_PROGRAM);
        }
        String typeCd = normalizeTypeCd(request.typeCd(), MAINT_PROGRAM);
        String description = validateDescription(request.description());
        try {
            jdbc.sql("INSERT INTO transaction_type (type_cd, description, version) VALUES (:t, :d, 0)")
                    .param("t", typeCd).param("d", description).update();
        } catch (DuplicateKeyException ex) {
            throw new ApiException(ErrorCode.DUPLICATE, "Error inserting record into: TRANSACTION_TYPE "
                    + "- duplicate key " + typeCd, MAINT_PROGRAM);
        }
        return new TransactionType(typeCd, description, 0);
    }

    @Transactional
    public TransactionType update(String typeCdIn, UpdateRequest request) {
        String typeCd = normalizeTypeCd(typeCdIn, MAINT_PROGRAM);
        if (request == null) {
            throw new ApiException(ErrorCode.INVALID_REQUEST, "No input received", MAINT_PROGRAM);
        }
        String description = validateDescription(request.description());
        if (request.version() == null) {
            throw ApiException.validation(MAINT_PROGRAM, "version", "version is required");
        }
        TransactionType current = jdbc.sql("SELECT * FROM transaction_type WHERE type_cd = :t FOR UPDATE")
                .param("t", typeCd).query(MAPPER).optional().orElseThrow(() -> notFound());
        if (current.description().strip().equalsIgnoreCase(description)) {
            throw ApiException.businessRule(MAINT_PROGRAM, LegacyMessages.NO_CHANGE);
        }
        if (current.version() != request.version()) {
            throw ApiException.concurrentUpdate(MAINT_PROGRAM);
        }
        jdbc.sql("UPDATE transaction_type SET description = :d, version = version + 1 WHERE type_cd = :t")
                .param("d", description).param("t", typeCd).update();
        return new TransactionType(typeCd, description, current.version() + 1);
    }

    @Transactional
    public void delete(String typeCdIn, Long version) {
        String typeCd = normalizeTypeCd(typeCdIn, MAINT_PROGRAM);
        TransactionType current = jdbc.sql("SELECT * FROM transaction_type WHERE type_cd = :t FOR UPDATE")
                .param("t", typeCd).query(MAPPER).optional().orElseThrow(() -> notFound());
        if (version != null && current.version() != version) {
            throw ApiException.concurrentUpdate(MAINT_PROGRAM);
        }
        try {
            jdbc.sql("DELETE FROM transaction_type WHERE type_cd = :t").param("t", typeCd).update();
        } catch (DataIntegrityViolationException ex) {
            throw new ApiException(ErrorCode.INTEGRITY_VIOLATION,
                    "Please delete associated child records first: TRANSACTION_TYPE " + typeCd, MAINT_PROGRAM);
        }
    }

    private java.util.Optional<TransactionType> find(String typeCd) {
        return jdbc.sql("SELECT * FROM transaction_type WHERE type_cd = :t").param("t", typeCd).query(MAPPER)
                .optional();
    }

    private static ApiException notFound() {
        return ApiException.notFound(MAINT_PROGRAM, "No record found for this key in database");
    }

    /** 1210-EDIT-TRANTYPE: 1245-EDIT-NUM-REQD on 2 characters, then zero padded. */
    static String normalizeTypeCd(String value, String program) {
        if (Text.isBlank(value)) {
            throw ApiException.validation(program, "typeCd", "Tran Type code must be supplied.");
        }
        String v = value.strip();
        if (v.length() > 2 || !Text.isDigits(v)) {
            throw ApiException.validation(program, "typeCd", "Tran Type code must be numeric.");
        }
        if (Text.isAllZero(v)) {
            throw ApiException.validation(program, "typeCd", "Tran Type code must not be zero.");
        }
        return Text.leftPadZeros(v, 2);
    }

    /** 1230-EDIT-ALPHANUM-REQD on the description. */
    private static String validateDescription(String value) {
        ValidationErrors errors = new ValidationErrors(MAINT_PROGRAM);
        if (Text.isBlank(value)) {
            errors.add("description", "Transaction Desc must be supplied.");
        } else if (!value.strip().matches("[A-Za-z0-9 ]+")) {
            errors.add("description", "Transaction Desc can have numbers or alphabets only.");
        } else if (value.strip().length() > 50) {
            errors.add("description", "Transaction Desc can not be longer than 50 characters.");
        }
        errors.throwIfAny();
        return value.strip();
    }
}
