package com.carddemo.programs.cotrtlic.gateway;

/**
 * Host variables bound into the C-TR-TYPE-FORWARD / C-TR-TYPE-BACKWARD cursors and the 9100 COUNT.
 *
 * @param startKey       :WS-START-KEY X(2)
 * @param editTypeFlag   :WS-EDIT-TYPE-FLAG X(1) ('1' = type filter active)
 * @param typeCdFilter   :WS-TYPE-CD-FILTER X(2)
 * @param editDescFlag   :WS-EDIT-DESC-FLAG X(1) ('1' = description filter active)
 * @param typeDescFilter :WS-TYPE-DESC-FILTER X(52), used as LIKE TRIM(:WS-TYPE-DESC-FILTER)
 */
public record TypeFilter(String startKey, char editTypeFlag, String typeCdFilter, char editDescFlag,
        String typeDescFilter) {
}
