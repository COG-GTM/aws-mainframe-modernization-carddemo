package com.carddemo.programs.coactupc.support;

/** Sequential cursor over a fixed-width record image laid out per a copybook. */
public final class FixedWidth {

    private final String image;
    private int position;

    public FixedWidth(String image, int recordLength) {
        this.image = Cobol.pad(image, recordLength);
    }

    public String next(int length) {
        String field = image.substring(position, position + length);
        position += length;
        return field;
    }

    public void skip(int length) {
        position += length;
    }
}
