package com.carddemo.common;

/** Keyset paging parameters replacing STARTBR/READNEXT/READPREV browsing. */
public record PageQuery(String startKey, Direction direction, int pageSize) {

    public static final int MAX_PAGE_SIZE = 100;

    public enum Direction {
        NEXT, PREV
    }

    public static PageQuery of(String startKey, String direction, Integer pageSize, int defaultPageSize,
            String legacyProgram) {
        Direction dir;
        if (Text.isBlank(direction) || direction.equalsIgnoreCase("next")) {
            dir = Direction.NEXT;
        } else if (direction.equalsIgnoreCase("prev")) {
            dir = Direction.PREV;
        } else {
            throw ApiException.validation(legacyProgram, "direction", "direction must be next or prev");
        }
        int size = pageSize == null ? defaultPageSize : pageSize;
        if (size < 1 || size > MAX_PAGE_SIZE) {
            throw ApiException.validation(legacyProgram, "pageSize", "pageSize must be between 1 and " + MAX_PAGE_SIZE);
        }
        return new PageQuery(Text.isBlank(startKey) ? null : startKey.strip(), dir, size);
    }
}
