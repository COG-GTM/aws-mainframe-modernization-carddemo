package com.carddemo.batch.posttran.domain;

import com.carddemo.batch.posttran.cobol.Pic;
import com.carddemo.batch.posttran.cobol.ZonedDecimal;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.math.BigDecimal;

/** ACCTFILE KSDS (ACCTDATA.VSAM.KSDS), ACCOUNT-RECORD from CVACT01Y, key ACCT-ID 9(11). */
@Entity
@Table(name = "account")
public class Account {

    public static final int LENGTH = 300;

    @Id
    @Column(name = "acct_id")
    private long acctId;
    @Column(name = "active_status", length = 1, nullable = false)
    private String activeStatus;
    @Column(name = "curr_bal", precision = 12, scale = 2, nullable = false)
    private BigDecimal currBal;
    @Column(name = "credit_limit", precision = 12, scale = 2, nullable = false)
    private BigDecimal creditLimit;
    @Column(name = "cash_credit_limit", precision = 12, scale = 2, nullable = false)
    private BigDecimal cashCreditLimit;
    @Column(name = "open_date", length = 10, nullable = false)
    private String openDate;
    /** ACCT-EXPIRAION-DATE (sic). */
    @Column(name = "expiration_date", length = 10, nullable = false)
    private String expirationDate;
    @Column(name = "reissue_date", length = 10, nullable = false)
    private String reissueDate;
    @Column(name = "curr_cyc_credit", precision = 12, scale = 2, nullable = false)
    private BigDecimal currCycCredit;
    @Column(name = "curr_cyc_debit", precision = 12, scale = 2, nullable = false)
    private BigDecimal currCycDebit;
    @Column(name = "addr_zip", length = 10, nullable = false)
    private String addrZip;
    @Column(name = "group_id", length = 10, nullable = false)
    private String groupId;
    @Column(name = "filler", length = 178, nullable = false)
    private String filler = Pic.spaces(178);

    protected Account() {
    }

    public Account(long acctId) {
        this.acctId = acctId;
        this.activeStatus = "Y";
        this.currBal = BigDecimal.ZERO.setScale(2);
        this.creditLimit = BigDecimal.ZERO.setScale(2);
        this.cashCreditLimit = BigDecimal.ZERO.setScale(2);
        this.openDate = Pic.spaces(10);
        this.expirationDate = Pic.spaces(10);
        this.reissueDate = Pic.spaces(10);
        this.currCycCredit = BigDecimal.ZERO.setScale(2);
        this.currCycDebit = BigDecimal.ZERO.setScale(2);
        this.addrZip = Pic.spaces(10);
        this.groupId = Pic.spaces(10);
    }

    public static Account fromRecord(String r) {
        Account a = new Account(ZonedDecimal.decodeUnsigned(r.substring(0, 11)));
        a.activeStatus = r.substring(11, 12);
        a.currBal = ZonedDecimal.decodeSigned(r.substring(12, 24), 2);
        a.creditLimit = ZonedDecimal.decodeSigned(r.substring(24, 36), 2);
        a.cashCreditLimit = ZonedDecimal.decodeSigned(r.substring(36, 48), 2);
        a.openDate = r.substring(48, 58);
        a.expirationDate = r.substring(58, 68);
        a.reissueDate = r.substring(68, 78);
        a.currCycCredit = ZonedDecimal.decodeSigned(r.substring(78, 90), 2);
        a.currCycDebit = ZonedDecimal.decodeSigned(r.substring(90, 102), 2);
        a.addrZip = r.substring(102, 112);
        a.groupId = r.substring(112, 122);
        a.filler = r.substring(122, 300);
        return a;
    }

    public String toRecord() {
        return ZonedDecimal.encodeUnsigned(acctId, 11) + Pic.x(activeStatus, 1)
                + ZonedDecimal.encodeSigned(currBal, 10, 2) + ZonedDecimal.encodeSigned(creditLimit, 10, 2)
                + ZonedDecimal.encodeSigned(cashCreditLimit, 10, 2) + Pic.x(openDate, 10)
                + Pic.x(expirationDate, 10) + Pic.x(reissueDate, 10)
                + ZonedDecimal.encodeSigned(currCycCredit, 10, 2) + ZonedDecimal.encodeSigned(currCycDebit, 10, 2)
                + Pic.x(addrZip, 10) + Pic.x(groupId, 10) + Pic.x(filler, 178);
    }

    public long getAcctId() { return acctId; }
    public BigDecimal getCurrBal() { return currBal; }
    public void setCurrBal(BigDecimal currBal) { this.currBal = currBal; }
    public BigDecimal getCreditLimit() { return creditLimit; }
    public void setCreditLimit(BigDecimal creditLimit) { this.creditLimit = creditLimit; }
    public String getExpirationDate() { return expirationDate; }
    public void setExpirationDate(String expirationDate) { this.expirationDate = Pic.x(expirationDate, 10); }
    public BigDecimal getCurrCycCredit() { return currCycCredit; }
    public void setCurrCycCredit(BigDecimal currCycCredit) { this.currCycCredit = currCycCredit; }
    public BigDecimal getCurrCycDebit() { return currCycDebit; }
    public void setCurrCycDebit(BigDecimal currCycDebit) { this.currCycDebit = currCycDebit; }
}
