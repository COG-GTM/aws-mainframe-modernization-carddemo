package com.carddemo.data.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Index;
import jakarta.persistence.Table;

/** Daily transaction awaiting posting (sequential DALYTRAN, RECLN 350). Copybook CVTRA06Y. */
@Entity
@Table(name = "daily_transactions", indexes = {
        @Index(name = "ix_daily_transactions_card_num", columnList = "card_num")
})
public class DailyTransaction extends AbstractTransaction {
}
