package com.carddemo.account;

import com.carddemo.account.AccountDtos.CardRef;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;

/** ACCTDAT, CUSTDAT and the CXACAIX alternate index of CARDXREF. */
@Repository
public class AccountRepository {

    static final RowMapper<AccountRecord> ACCOUNT = (rs, n) -> new AccountRecord(
            rs.getLong("acct_id"), rs.getString("active_status"), rs.getBigDecimal("curr_bal"),
            rs.getBigDecimal("credit_limit"), rs.getBigDecimal("cash_credit_limit"), date(rs, "open_date"),
            date(rs, "expiration_date"), date(rs, "reissue_date"), rs.getBigDecimal("curr_cyc_credit"),
            rs.getBigDecimal("curr_cyc_debit"), rs.getString("addr_zip"), rs.getString("group_id"),
            rs.getLong("version"));

    static final RowMapper<CustomerRecord> CUSTOMER = (rs, n) -> new CustomerRecord(
            rs.getInt("cust_id"), rs.getString("first_name"), rs.getString("middle_name"), rs.getString("last_name"),
            rs.getString("addr_line_1"), rs.getString("addr_line_2"), rs.getString("addr_line_3"),
            rs.getString("addr_state_cd"), rs.getString("addr_country_cd"), rs.getString("addr_zip"),
            rs.getString("phone_num_1"), rs.getString("phone_num_2"), rs.getString("ssn"),
            rs.getString("govt_issued_id"), date(rs, "dob"), rs.getString("eft_account_id"),
            rs.getString("pri_card_holder_ind"), (Integer) rs.getObject("fico_credit_score", Integer.class),
            rs.getLong("version"));

    private final JdbcClient jdbc;

    public static RowMapper<AccountRecord> accountMapper() {
        return ACCOUNT;
    }

    public AccountRepository(JdbcClient jdbc) {
        this.jdbc = jdbc;
    }

    public record XrefRow(String cardNum, int custId, long acctId) {
    }

    public Optional<XrefRow> findFirstXrefByAccount(long acctId) {
        return jdbc.sql("SELECT card_num, cust_id, acct_id FROM card_xref WHERE acct_id = :id "
                + "ORDER BY card_num COLLATE \"C\" LIMIT 1")
                .param("id", acctId)
                .query((rs, n) -> new XrefRow(rs.getString("card_num"), rs.getInt("cust_id"), rs.getLong("acct_id")))
                .optional();
    }

    public List<CardRef> findCardsByAccount(long acctId) {
        return jdbc.sql("SELECT card_num, active_status FROM card WHERE acct_id = :id ORDER BY card_num COLLATE \"C\"")
                .param("id", acctId)
                .query((rs, n) -> new CardRef(rs.getString("card_num"), rs.getString("active_status")))
                .list();
    }

    public Optional<AccountRecord> findAccount(long acctId) {
        return jdbc.sql("SELECT * FROM account WHERE acct_id = :id").param("id", acctId).query(ACCOUNT).optional();
    }

    public Optional<AccountRecord> lockAccountNoWait(long acctId) {
        return jdbc.sql("SELECT * FROM account WHERE acct_id = :id FOR UPDATE NOWAIT").param("id", acctId)
                .query(ACCOUNT).optional();
    }

    public Optional<CustomerRecord> findCustomer(int custId) {
        return jdbc.sql("SELECT * FROM customer WHERE cust_id = :id").param("id", custId).query(CUSTOMER)
                .optional();
    }

    public Optional<CustomerRecord> lockCustomerNoWait(int custId) {
        return jdbc.sql("SELECT * FROM customer WHERE cust_id = :id FOR UPDATE NOWAIT").param("id", custId)
                .query(CUSTOMER).optional();
    }

    public int updateAccount(AccountRecord a) {
        return jdbc.sql("""
                UPDATE account SET active_status = :status, curr_bal = :currBal, credit_limit = :creditLimit,
                       cash_credit_limit = :cashCreditLimit, open_date = :openDate, expiration_date = :expDate,
                       reissue_date = :reissueDate, curr_cyc_credit = :cycCredit, curr_cyc_debit = :cycDebit,
                       addr_zip = :addrZip, group_id = :groupId, version = version + 1
                 WHERE acct_id = :id AND version = :version""")
                .param("status", a.activeStatus()).param("currBal", a.currBal())
                .param("creditLimit", a.creditLimit()).param("cashCreditLimit", a.cashCreditLimit())
                .param("openDate", a.openDate()).param("expDate", a.expirationDate())
                .param("reissueDate", a.reissueDate()).param("cycCredit", a.currCycCredit())
                .param("cycDebit", a.currCycDebit()).param("addrZip", a.addrZip()).param("groupId", a.groupId())
                .param("id", a.acctId()).param("version", a.version())
                .update();
    }

    public int updateCustomer(CustomerRecord c) {
        return jdbc.sql("""
                UPDATE customer SET first_name = :first, middle_name = :middle, last_name = :last,
                       addr_line_1 = :l1, addr_line_2 = :l2, addr_line_3 = :l3, addr_state_cd = :state,
                       addr_country_cd = :country, addr_zip = :zip, phone_num_1 = :p1, phone_num_2 = :p2,
                       ssn = :ssn, govt_issued_id = :govt, dob = :dob, eft_account_id = :eft,
                       pri_card_holder_ind = :pri, fico_credit_score = :fico, version = version + 1
                 WHERE cust_id = :id AND version = :version""")
                .param("first", c.firstName()).param("middle", c.middleName()).param("last", c.lastName())
                .param("l1", c.addrLine1()).param("l2", c.addrLine2()).param("l3", c.addrLine3())
                .param("state", c.addrStateCd()).param("country", c.addrCountryCd()).param("zip", c.addrZip())
                .param("p1", c.phoneNum1()).param("p2", c.phoneNum2()).param("ssn", c.ssn())
                .param("govt", c.govtIssuedId()).param("dob", c.dob()).param("eft", c.eftAccountId())
                .param("pri", c.priCardHolderInd()).param("fico", c.ficoCreditScore())
                .param("id", c.custId()).param("version", c.version())
                .update();
    }

    private static LocalDate date(ResultSet rs, String column) throws SQLException {
        Date d = rs.getDate(column);
        return d == null ? null : d.toLocalDate();
    }
}
