package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.gateway.DateValidationResult;
import com.carddemo.programs.coactupc.gateway.DateValidator;
import com.carddemo.programs.coactupc.support.Cobol;
import com.carddemo.programs.coactupc.support.NumericFunctions;
import com.carddemo.programs.coactupc.support.ZonedDecimal;
import java.math.BigDecimal;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

/**
 * CSUTLDPY - procedure-division copybook with the CCYYMMDD date edits.
 * {@code PERFORM EDIT-DATE-CCYYMMDD THRU EDIT-DATE-CCYYMMDD-EXIT} falls through EDIT-YEAR-CCYY,
 * EDIT-MONTH, EDIT-DAY, EDIT-DAY-MONTH-YEAR and EDIT-DATE-LE; the GO TOs inside each paragraph only
 * skip to that paragraph's exit, so all three parts are always edited.
 */
public final class DateEditProcedures {

    private static final LocalDate LILIAN_BASE = LocalDate.of(1600, 12, 31);

    private DateEditProcedures() {
    }

    // EDIT-DATE-CCYYMMDD THRU EDIT-DATE-CCYYMMDD-EXIT
    public static void editDateCcyymmdd(DateEditWorkArea wa, EditMessages ctx, DateValidator validator) {
        wa.setDateInvalid();
        editYearCcyy(wa, ctx);
        editMonth(wa, ctx);
        editDay(wa, ctx);
        if (!editDayMonthYear(wa, ctx)) {
            return;
        }
        editDateLe(wa, ctx, validator);
        // unnamed sentence after EDIT-DATE-LE-EXIT: reached on both the success and the GO TO path
        wa.setDateValid();
    }

    // EDIT-YEAR-CCYY
    static void editYearCcyy(DateEditWorkArea wa, EditMessages ctx) {
        wa.yearFlg = DateEditWorkArea.FLG_NOT_OK;
        if (Cobol.isLowValuesOrSpaces(wa.ccyy())) {
            ctx.setInputError();
            wa.yearFlg = DateEditWorkArea.FLG_BLANK;
            message(ctx, " : Year must be supplied.");
            return;
        }
        if (!Cobol.isNumeric(wa.ccyy())) {
            ctx.setInputError();
            wa.yearFlg = DateEditWorkArea.FLG_NOT_OK;
            message(ctx, " must be 4 digit number.");
            return;
        }
        if (!(wa.isThisCentury() || wa.isLastCentury())) {
            ctx.setInputError();
            wa.yearFlg = DateEditWorkArea.FLG_NOT_OK;
            message(ctx, " : Century is not valid.");
            return;
        }
        wa.yearFlg = DateEditWorkArea.FLG_ISVALID;
    }

    // EDIT-MONTH
    static void editMonth(DateEditWorkArea wa, EditMessages ctx) {
        wa.monthFlg = DateEditWorkArea.FLG_NOT_OK;
        if (Cobol.isLowValuesOrSpaces(wa.mm())) {
            ctx.setInputError();
            wa.monthFlg = DateEditWorkArea.FLG_BLANK;
            message(ctx, " : Month must be supplied.");
            return;
        }
        if (!wa.isValidMonth()) {
            ctx.setInputError();
            wa.monthFlg = DateEditWorkArea.FLG_NOT_OK;
            message(ctx, ": Month must be a number between 1 and 12.");
            return;
        }
        if (NumericFunctions.testNumval(wa.mm()) == 0) {
            wa.setMm(twoDigits(NumericFunctions.numval(wa.mm())));
        } else {
            ctx.setInputError();
            wa.monthFlg = DateEditWorkArea.FLG_NOT_OK;
            message(ctx, ": Month must be a number between 1 and 12.");
            return;
        }
        wa.monthFlg = DateEditWorkArea.FLG_ISVALID;
    }

    // EDIT-DAY
    static void editDay(DateEditWorkArea wa, EditMessages ctx) {
        wa.dayFlg = DateEditWorkArea.FLG_ISVALID;
        if (Cobol.isLowValuesOrSpaces(wa.dd())) {
            ctx.setInputError();
            wa.dayFlg = DateEditWorkArea.FLG_BLANK;
            message(ctx, " : Day must be supplied.");
            return;
        }
        if (NumericFunctions.testNumval(wa.dd()) == 0) {
            wa.setDd(twoDigits(NumericFunctions.numval(wa.dd())));
        } else {
            ctx.setInputError();
            wa.dayFlg = DateEditWorkArea.FLG_NOT_OK;
            message(ctx, ":day must be a number between 1 and 31.");
            return;
        }
        if (!wa.isValidDay()) {
            ctx.setInputError();
            wa.dayFlg = DateEditWorkArea.FLG_NOT_OK;
            message(ctx, ":day must be a number between 1 and 31.");
            return;
        }
        wa.dayFlg = DateEditWorkArea.FLG_ISVALID;
    }

    /** EDIT-DAY-MONTH-YEAR; returns false where the COBOL does GO TO EDIT-DATE-CCYYMMDD-EXIT. */
    static boolean editDayMonthYear(DateEditWorkArea wa, EditMessages ctx) {
        if (!wa.is31DayMonth() && wa.isDay31()) {
            ctx.setInputError();
            wa.dayFlg = DateEditWorkArea.FLG_NOT_OK;
            wa.monthFlg = DateEditWorkArea.FLG_NOT_OK;
            message(ctx, ":Cannot have 31 days in this month.");
            return false;
        }
        if (wa.isFebruary() && wa.isDay30()) {
            ctx.setInputError();
            wa.dayFlg = DateEditWorkArea.FLG_NOT_OK;
            wa.monthFlg = DateEditWorkArea.FLG_NOT_OK;
            message(ctx, ":Cannot have 30 days in this month.");
            return false;
        }
        if (wa.isFebruary() && wa.isDay29()) {
            int divBy = wa.yyN() == 0 ? 400 : 4;
            int remainder = wa.ccyyN() % divBy;
            if (remainder != 0) {
                ctx.setInputError();
                wa.dayFlg = DateEditWorkArea.FLG_NOT_OK;
                wa.monthFlg = DateEditWorkArea.FLG_NOT_OK;
                wa.yearFlg = DateEditWorkArea.FLG_NOT_OK;
                message(ctx, ":Not a leap year.Cannot have 29 days in this month.");
                return false;
            }
        }
        return wa.isDateValid();
    }

    // EDIT-DATE-LE
    static void editDateLe(DateEditWorkArea wa, EditMessages ctx, DateValidator validator) {
        wa.initializeValidationResult();
        wa.dateFormat = "YYYYMMDD";
        DateValidationResult r = validator.validate(wa.editDateCcyymmdd, wa.dateFormat);
        wa.severity = Cobol.unsignedDisplay(r.severity(), 4);
        wa.msgNo = Cobol.unsignedDisplay(r.msgNo(), 4);
        wa.result = Cobol.pad(r.result(), 15);
        wa.date = Cobol.pad(wa.editDateCcyymmdd, 10);
        wa.dateFmt = Cobol.pad(wa.dateFormat, 10);
        if (r.severity() != 0) {
            ctx.setInputError();
            wa.dayFlg = DateEditWorkArea.FLG_NOT_OK;
            wa.monthFlg = DateEditWorkArea.FLG_NOT_OK;
            wa.yearFlg = DateEditWorkArea.FLG_NOT_OK;
            if (ctx.isReturnMsgOff()) {
                ctx.stringIntoReturnMsg(Cobol.trim(ctx.editVariableName()), " validation error Sev code: ",
                        wa.severity, " Message code: ", wa.msgNo);
            }
            return;
        }
        if (!ctx.isInputError()) {
            wa.dayFlg = DateEditWorkArea.FLG_ISVALID;
        }
    }

    // EDIT-DATE-OF-BIRTH
    public static void editDateOfBirth(DateEditWorkArea wa, EditMessages ctx, LocalDate today) {
        wa.currentDateYyyymmdd = String.format("%04d%02d%02d", today.getYear(), today.getMonthValue(),
                today.getDayOfMonth());
        wa.editDateBinary = integerOfDate(wa.ccyymmddN());
        wa.currentDateBinary = integerOfDate(Cobol.unsignedValue(wa.currentDateYyyymmdd));
        if (wa.currentDateBinary > wa.editDateBinary) {
            return;
        }
        ctx.setInputError();
        wa.dayFlg = DateEditWorkArea.FLG_NOT_OK;
        wa.monthFlg = DateEditWorkArea.FLG_NOT_OK;
        wa.yearFlg = DateEditWorkArea.FLG_NOT_OK;
        message(ctx, ":cannot be in the future ");
    }

    /** FUNCTION INTEGER-OF-DATE: day number with 1601-01-01 = 1; 0 for an invalid date. */
    public static int integerOfDate(long yyyymmdd) {
        int year = (int) (yyyymmdd / 10000);
        int month = (int) (yyyymmdd / 100 % 100);
        int day = (int) (yyyymmdd % 100);
        if (year < 1601) {
            return 0;
        }
        try {
            return (int) ChronoUnit.DAYS.between(LILIAN_BASE, LocalDate.of(year, month, day));
        } catch (DateTimeException e) {
            return 0;
        }
    }

    private static void message(EditMessages ctx, String text) {
        if (ctx.isReturnMsgOff()) {
            ctx.stringIntoReturnMsg(Cobol.trim(ctx.editVariableName()), text);
        }
    }

    /** COMPUTE PIC 9(2) = NUMVAL(...): integer part, unsigned, high-order truncation. */
    private static String twoDigits(BigDecimal value) {
        BigDecimal v = ZonedDecimal.truncate(value, 2, 0).abs();
        return Cobol.unsignedDisplay(v.longValue(), 2);
    }
}
