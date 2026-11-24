package com.carddemo.common.web;

import java.util.List;

/**
 * One screen-full of a CICS STARTBR/READNEXT/READPREV browse. The first/last keys replace the
 * CDEMO-*-FIRST/LAST key fields kept in the COMMAREA between PF7/PF8 presses, so the API stays stateless.
 */
public record KeysetPage<T>(List<T> items, int pageNumber, String firstKey, String lastKey, boolean hasNextPage,
                            boolean hasPreviousPage, String message) {
}
