package com.carddemo.transactiontype.domain;

/**
 * Normalised list filters (COTRTLIC 1220-EDIT-TYPECD / 1230-EDIT-DESC).
 * A null member means "filter not supplied".
 */
public record ListFilter(String typeCode, String descriptionPattern) {

    public static final ListFilter NONE = new ListFilter(null, null);
}
