package com.carddemo.recordio.layout;

import com.carddemo.recordio.codec.FixedWidthRecord;

/** {@code app/cpy/CVTRA02Y.cpy}, RECLN 50. */
public final class DisclosureGroupLayout implements RecordLayout<DisclosureGroup> {

    public static final DisclosureGroupLayout INSTANCE = new DisclosureGroupLayout();

    static final int GROUP_ID = 0;  // X(10)
    static final int TYPE_CD = 10;  // X(02)
    static final int CAT_CD = 12;   // 9(04)
    static final int INT_RATE = 16; // S9(04)V99
    static final int FILLER = 22;   // X(28)

    private DisclosureGroupLayout() {
    }

    @Override
    public int length() {
        return DisclosureGroup.LENGTH;
    }

    @Override
    public DisclosureGroup decode(FixedWidthRecord r) {
        return new DisclosureGroup(
                r.text(GROUP_ID, 10), r.text(TYPE_CD, 2), (int) r.unsignedInt(CAT_CD, 4), r.zoned(INT_RATE, 6, 2, true));
    }

    @Override
    public void encodeInto(FixedWidthRecord r, DisclosureGroup g) {
        r.setText(GROUP_ID, 10, g.accountGroupId());
        r.setText(TYPE_CD, 2, g.typeCode());
        r.setUnsignedInt(CAT_CD, 4, g.categoryCode());
        r.setZoned(INT_RATE, 6, 2, true, g.interestRate());
    }
}
