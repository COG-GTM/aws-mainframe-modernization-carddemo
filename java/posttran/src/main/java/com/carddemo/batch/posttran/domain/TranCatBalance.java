package com.carddemo.batch.posttran.domain;

import com.carddemo.batch.posttran.cobol.Pic;
import com.carddemo.batch.posttran.cobol.ZonedDecimal;
import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.math.BigDecimal;

/** TCATBALF KSDS (TCATBALF.VSAM.KSDS), TRAN-CAT-BAL-RECORD from CVTRA01Y. */
@Entity
@Table(name = "tran_cat_balance")
public class TranCatBalance {

    public static final int LENGTH = 50;

    @EmbeddedId
    private TranCatBalanceKey key;
    @Column(name = "tran_cat_bal", precision = 11, scale = 2, nullable = false)
    private BigDecimal balance;
    /** Bytes 29-50; the shipped seed data has zeros here, so it is carried through verbatim. */
    @Column(name = "filler", length = 22, nullable = false)
    private String filler = Pic.spaces(22);

    protected TranCatBalance() {
    }

    public TranCatBalance(TranCatBalanceKey key, BigDecimal balance, String filler) {
        this.key = key;
        this.balance = balance;
        this.filler = Pic.x(filler, 22);
    }

    public static TranCatBalance fromRecord(String r) {
        TranCatBalanceKey key = new TranCatBalanceKey(ZonedDecimal.decodeUnsigned(r.substring(0, 11)),
                r.substring(11, 13), (int) ZonedDecimal.decodeUnsigned(r.substring(13, 17)));
        return new TranCatBalance(key, ZonedDecimal.decodeSigned(r.substring(17, 28), 2), r.substring(28, 50));
    }

    public String toRecord() {
        return key.asRecordKey() + ZonedDecimal.encodeSigned(balance, 9, 2) + Pic.x(filler, 22);
    }

    public TranCatBalanceKey getKey() { return key; }
    public BigDecimal getBalance() { return balance; }
    public void setBalance(BigDecimal balance) { this.balance = balance; }
    public String getFiller() { return filler; }
}
