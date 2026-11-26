package com.carddemo.programs.paudblod.ims;

import com.carddemo.programs.paudblod.copybook.CobolFields;

import java.math.BigDecimal;
import java.nio.charset.Charset;

/** Segment search arguments declared in PAUDBLOD WORKING-STORAGE. */
public sealed interface Ssa {

    String ROOT_SEGMENT = "PAUTSUM0";
    String CHILD_SEGMENT = "PAUTDTL1";

    String segmentName();

    /** SSA exactly as laid out in WORKING-STORAGE. */
    byte[] toBytes(Charset cs);

    /**
     * 01 ROOT-QUAL-SSA: {@code PAUTSUM0(ACCNTID EQ<packed key>)}, 26 bytes.
     * QUAL-SSA-KEY-VALUE is PIC S9(11) COMP-3.
     */
    record RootQualSsa(BigDecimal qualSsaKeyValue) implements Ssa {
        public static final int LENGTH = 26;
        public static final String QUAL_SSA_KEY_FIELD = "ACCNTID ";
        public static final String QUAL_SSA_REL_OPER = "EQ";

        @Override
        public String segmentName() {
            return ROOT_SEGMENT;
        }

        @Override
        public byte[] toBytes(Charset cs) {
            byte[] b = new byte[LENGTH];
            CobolFields.encodeText(ROOT_SEGMENT, 8, b, 0, cs);
            CobolFields.encodeText("(", 1, b, 8, cs);
            CobolFields.encodeText(QUAL_SSA_KEY_FIELD, 8, b, 9, cs);
            CobolFields.encodeText(QUAL_SSA_REL_OPER, 2, b, 17, cs);
            CobolFields.encodePacked(qualSsaKeyValue, 11, 0, b, 19);
            CobolFields.encodeText(")", 1, b, 25, cs);
            return b;
        }
    }

    /** 01 ROOT-UNQUAL-SSA: {@code 'PAUTSUM0 '}. */
    record RootUnqualSsa() implements Ssa {
        @Override
        public String segmentName() {
            return ROOT_SEGMENT;
        }

        @Override
        public byte[] toBytes(Charset cs) {
            return (ROOT_SEGMENT + " ").getBytes(cs);
        }
    }

    /** 01 CHILD-UNQUAL-SSA: {@code 'PAUTDTL1 '}. */
    record ChildUnqualSsa() implements Ssa {
        @Override
        public String segmentName() {
            return CHILD_SEGMENT;
        }

        @Override
        public byte[] toBytes(Charset cs) {
            return (CHILD_SEGMENT + " ").getBytes(cs);
        }
    }
}
