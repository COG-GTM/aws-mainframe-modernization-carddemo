package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

import java.io.Serializable;
import java.util.Objects;

/** IMS key of a PAUTDTL1 child segment: parent account + PA-AUTHORIZATION-KEY. */
@Embeddable
public class PendingAuthorizationDetailId implements Serializable {

    /** PA-ACCT-ID of the parent PAUTSUM0 segment */
    @Column(name = "acct_id")
    private Long accountId;

    /** PA-AUTH-DATE-9C PIC S9(05) COMP-3 */
    @Column(name = "auth_date_key")
    private Integer authDateKey;

    /** PA-AUTH-TIME-9C PIC S9(09) COMP-3 */
    @Column(name = "auth_time_key")
    private Long authTimeKey;

    public PendingAuthorizationDetailId() {
    }

    public PendingAuthorizationDetailId(Long accountId, Integer authDateKey, Long authTimeKey) {
        this.accountId = accountId;
        this.authDateKey = authDateKey;
        this.authTimeKey = authTimeKey;
    }

    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }

    public Integer getAuthDateKey() {
        return authDateKey;
    }

    public void setAuthDateKey(Integer authDateKey) {
        this.authDateKey = authDateKey;
    }

    public Long getAuthTimeKey() {
        return authTimeKey;
    }

    public void setAuthTimeKey(Long authTimeKey) {
        this.authTimeKey = authTimeKey;
    }

    @Override
    public boolean equals(Object o) {
        return this == o || (o instanceof PendingAuthorizationDetailId that && Objects.equals(accountId, that.accountId) && Objects.equals(authDateKey, that.authDateKey) && Objects.equals(authTimeKey, that.authTimeKey));
    }

    @Override
    public int hashCode() {
        return Objects.hash(accountId, authDateKey, authTimeKey);
    }

    @Override
    public String toString() {
        return "PendingAuthorizationDetailId[" + accountId + "," + authDateKey + "," + authTimeKey + "]";
    }
}
