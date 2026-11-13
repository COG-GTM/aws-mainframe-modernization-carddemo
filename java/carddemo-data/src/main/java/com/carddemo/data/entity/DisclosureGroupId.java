package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

import java.io.Serializable;
import java.util.Objects;

/** DIS-GROUP-KEY group of CVTRA02Y (VSAM KSDS DISCGRP key, 16 bytes). */
@Embeddable
public class DisclosureGroupId implements Serializable {

    /** DIS-ACCT-GROUP-ID PIC X(10) */
    @Column(name = "group_id", length = 10)
    private String groupId;

    /** DIS-TRAN-TYPE-CD PIC X(02) */
    @Column(name = "type_cd", length = 2)
    private String typeCode;

    /** DIS-TRAN-CAT-CD PIC 9(04) */
    @Column(name = "cat_cd")
    private Integer categoryCode;

    public DisclosureGroupId() {
    }

    public DisclosureGroupId(String groupId, String typeCode, Integer categoryCode) {
        this.groupId = groupId;
        this.typeCode = typeCode;
        this.categoryCode = categoryCode;
    }

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public Integer getCategoryCode() {
        return categoryCode;
    }

    public void setCategoryCode(Integer categoryCode) {
        this.categoryCode = categoryCode;
    }

    @Override
    public boolean equals(Object o) {
        return this == o || (o instanceof DisclosureGroupId that && Objects.equals(groupId, that.groupId) && Objects.equals(typeCode, that.typeCode) && Objects.equals(categoryCode, that.categoryCode));
    }

    @Override
    public int hashCode() {
        return Objects.hash(groupId, typeCode, categoryCode);
    }

    @Override
    public String toString() {
        return "DisclosureGroupId[" + groupId + "," + typeCode + "," + categoryCode + "]";
    }
}
