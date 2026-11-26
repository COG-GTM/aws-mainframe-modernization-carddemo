package com.carddemo.programs.cbact01c.copybook;

import com.carddemo.programs.cbact01c.support.Pic;

import java.util.Arrays;
import java.util.Optional;

/**
 * Copybook CODATECN: {@code 01 CODATECN-REC}, the parameter block passed to the COBDATFT date-format routine
 * (assembler DSECT COCDATFT in app/maclib). WORKING-STORAGE item with no VALUE clauses: starts as spaces and
 * keeps whatever COBDATFT left in it between calls.
 */
public final class CodatecnRec {
    public static final int TYPE_LENGTH = 1;
    public static final int INP_DATE_LENGTH = 20;
    public static final int OUTTYPE_LENGTH = 1;
    public static final int OUT_DATE_LENGTH = 20;
    public static final int ERROR_MSG_LENGTH = 38;
    public static final int LENGTH = TYPE_LENGTH + INP_DATE_LENGTH + OUTTYPE_LENGTH + OUT_DATE_LENGTH + ERROR_MSG_LENGTH;

    /** 88-levels on CODATECN-TYPE. */
    public enum InputType {
        /** 88 YYYYMMDD-IN VALUE "1". */
        YYYYMMDD_IN("1"),
        /** 88 YYYY-MM-DD-IN VALUE "2". */
        YYYY_MM_DD_IN("2");

        public final String value;

        InputType(String value) {
            this.value = value;
        }

        public static Optional<InputType> of(String v) {
            return Arrays.stream(values()).filter(t -> t.value.equals(v)).findFirst();
        }
    }

    /** 88-levels on CODATECN-OUTTYPE. */
    public enum OutputType {
        /** 88 YYYY-MM-DD-OP VALUE "1". */
        YYYY_MM_DD_OP("1"),
        /** 88 YYYYMMDD-OP VALUE "2". */
        YYYYMMDD_OP("2");

        public final String value;

        OutputType(String value) {
            this.value = value;
        }

        public static Optional<OutputType> of(String v) {
            return Arrays.stream(values()).filter(t -> t.value.equals(v)).findFirst();
        }
    }

    // CODATECN-IN-REC
    private String codatecnType = Pic.spaces(TYPE_LENGTH);
    private String codatecnInpDate = Pic.spaces(INP_DATE_LENGTH);
    // CODATECN-OUT-REC
    private String codatecnOuttype = Pic.spaces(OUTTYPE_LENGTH);
    private String codatecn0utDate = Pic.spaces(OUT_DATE_LENGTH);
    private String codatecnErrorMsg = Pic.spaces(ERROR_MSG_LENGTH);

    public String codatecnType() {
        return codatecnType;
    }

    public void setCodatecnType(String v) {
        codatecnType = Pic.x(v, TYPE_LENGTH);
    }

    public boolean isYyyymmddIn() {
        return InputType.YYYYMMDD_IN.value.equals(codatecnType);
    }

    public boolean isYyyyMmDdIn() {
        return InputType.YYYY_MM_DD_IN.value.equals(codatecnType);
    }

    public String codatecnInpDate() {
        return codatecnInpDate;
    }

    public void setCodatecnInpDate(String v) {
        codatecnInpDate = Pic.x(v, INP_DATE_LENGTH);
    }

    public String codatecnOuttype() {
        return codatecnOuttype;
    }

    public void setCodatecnOuttype(String v) {
        codatecnOuttype = Pic.x(v, OUTTYPE_LENGTH);
    }

    public boolean isYyyyMmDdOp() {
        return OutputType.YYYY_MM_DD_OP.value.equals(codatecnOuttype);
    }

    public boolean isYyyymmddOp() {
        return OutputType.YYYYMMDD_OP.value.equals(codatecnOuttype);
    }

    /** CODATECN-0UT-DATE (the copybook spells it with a zero). */
    public String codatecn0utDate() {
        return codatecn0utDate;
    }

    public void setCodatecn0utDate(String v) {
        codatecn0utDate = Pic.x(v, OUT_DATE_LENGTH);
    }

    public String codatecnErrorMsg() {
        return codatecnErrorMsg;
    }

    public void setCodatecnErrorMsg(String v) {
        codatecnErrorMsg = Pic.x(v, ERROR_MSG_LENGTH);
    }

    // CODATECN-1INP REDEFINES CODATECN-INP-DATE (YYYYMMDD input)
    public String codatecn1Yyyy() {
        return codatecnInpDate.substring(0, 4);
    }

    public String codatecn1InpMm() {
        return codatecnInpDate.substring(4, 6);
    }

    public String codatecn1Dd() {
        return codatecnInpDate.substring(6, 8);
    }

    public String codatecn1Fil() {
        return codatecnInpDate.substring(8, 20);
    }

    // CODATECN-2INP REDEFINES CODATECN-INP-DATE (YYYY-MM-DD input)
    public String codatecn1OYyyy() {
        return codatecnInpDate.substring(0, 4);
    }

    public String codatecn1IS1() {
        return codatecnInpDate.substring(4, 5);
    }

    public String codatecn2InpMm() {
        return codatecnInpDate.substring(5, 7);
    }

    public String codatecn1IS2() {
        return codatecnInpDate.substring(7, 8);
    }

    /** CODATECN-2YY: despite the name, the DD part of a YYYY-MM-DD input. */
    public String codatecn2Yy() {
        return codatecnInpDate.substring(8, 10);
    }

    public String codatecn2Fil() {
        return codatecnInpDate.substring(10, 20);
    }

    // CODATECN-1OUT REDEFINES CODATECN-0UT-DATE (YYYY-MM-DD output)
    public String codatecn1OutYyyy() {
        return codatecn0utDate.substring(0, 4);
    }

    public String codatecn1OS1() {
        return codatecn0utDate.substring(4, 5);
    }

    public String codatecn1OMm() {
        return codatecn0utDate.substring(5, 7);
    }

    public String codatecn1OS2() {
        return codatecn0utDate.substring(7, 8);
    }

    public String codatecn1ODd() {
        return codatecn0utDate.substring(8, 10);
    }

    public String codatecn1OFil() {
        return codatecn0utDate.substring(10, 20);
    }

    // CODATECN-2OUT REDEFINES CODATECN-0UT-DATE (YYYYMMDD output)
    public String codatecn2OYyyy() {
        return codatecn0utDate.substring(0, 4);
    }

    public String codatecn2OMm() {
        return codatecn0utDate.substring(4, 6);
    }

    public String codatecn2ODd() {
        return codatecn0utDate.substring(6, 8);
    }

    public String codatecn2OFil() {
        return codatecn0utDate.substring(8, 20);
    }

    /** The 80-byte parameter block as laid out in storage. */
    public String asText() {
        return codatecnType + codatecnInpDate + codatecnOuttype + codatecn0utDate + codatecnErrorMsg;
    }
}
