package com.carddemo.transaction.domain;

import com.carddemo.transaction.cobol.Pic;
import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import java.math.BigDecimal;

/**
 * TRAN-CAT-BAL-RECORD (copybook CVTRA01Y / FD-TRAN-CAT-BAL-RECORD in CBTRN02C, RECLN 50) -
 * TCATBALF VSAM KSDS keyed by account + type + category.
 */
@Entity
@Table(name = "TCATBALF")
public class TransactionCategoryBalance {

    @EmbeddedId
    private TransactionCategoryBalanceId id;
    @Column(name = "TRAN_CAT_BAL", precision = 11, scale = 2, nullable = false)
    private BigDecimal balance;             // PIC S9(09)V99

    protected TransactionCategoryBalance() {
    }

    /** {@code INITIALIZE TRAN-CAT-BAL-RECORD} + key moves. */
    public TransactionCategoryBalance(TransactionCategoryBalanceId id) {
        this.id = id;
        this.balance = Pic.s9(BigDecimal.ZERO, 9, 2);
    }

    public TransactionCategoryBalanceId getId() { return id; }
    public BigDecimal getBalance() { return balance; }
    public void setBalance(BigDecimal v) { this.balance = Pic.s9(v, 9, 2); }
}
