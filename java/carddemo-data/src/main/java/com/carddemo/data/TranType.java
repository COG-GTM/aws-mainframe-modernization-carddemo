package com.carddemo.data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.util.Objects;

/** Transaction type, TRANTYPE (CVTRA03Y). */
@Entity
@Table(name = "tran_types")
public class TranType {

    @Id
    @Column(name = "type_cd", nullable = false, length = 2)
    private String typeCd;

    @Column(name = "description", nullable = false, length = 50)
    private String description;

    public TranType() {
    }

    public String getTypeCd() {
        return typeCd;
    }

    public void setTypeCd(String typeCd) {
        this.typeCd = typeCd;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @Override
    public boolean equals(Object other) {
        return this == other || other instanceof TranType that && typeCd != null && typeCd.equals(that.typeCd);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(typeCd);
    }

    @Override
    public String toString() {
        return "TranType[typeCd=" + typeCd + "]";
    }
}
