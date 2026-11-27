package com.carddemo.data.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Index;
import jakarta.persistence.Table;

/** Posted transaction (VSAM KSDS TRANSACT, RECLN 350; AIX on TRAN-PROC-TS). Copybook CVTRA05Y. */
@Entity
@Table(name = "transactions", indexes = {
        @Index(name = "ix_transactions_proc_ts", columnList = "proc_ts"),
        @Index(name = "ix_transactions_card_num", columnList = "card_num")
})
public class Transaction extends AbstractTransaction {
}
