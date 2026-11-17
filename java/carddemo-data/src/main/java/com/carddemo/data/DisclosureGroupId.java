package com.carddemo.data;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import java.io.Serializable;

/** Primary key of {@link DisclosureGroup}. */
@Embeddable
public record DisclosureGroupId(
        @Column(name = "acct_group_id", nullable = false, length = 10) String acctGroupId,
        @Column(name = "type_cd", nullable = false, length = 2) String typeCd,
        @Column(name = "cat_cd", nullable = false) Integer catCd) implements Serializable {
}
