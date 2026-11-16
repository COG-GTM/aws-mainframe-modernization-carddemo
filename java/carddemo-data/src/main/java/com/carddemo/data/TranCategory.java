package com.carddemo.data;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.util.Objects;

/** Transaction category, TRANCATG (CVTRA04Y). */
@Entity
@Table(name = "tran_categories")
public class TranCategory {

    @EmbeddedId
    private TranCategoryId id;

    @Column(name = "description", nullable = false, length = 50)
    private String description;

    public TranCategory() {
    }

    public TranCategoryId getId() {
        return id;
    }

    public void setId(TranCategoryId id) {
        this.id = id;
    }
    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof TranCategory that && id != null && id.equals(that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }

    @Override
    public String toString() {
        return "TranCategory[id=" + id + "]";
    }
}
