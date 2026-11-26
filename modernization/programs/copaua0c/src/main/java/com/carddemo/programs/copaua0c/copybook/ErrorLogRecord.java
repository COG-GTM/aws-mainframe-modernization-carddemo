package com.carddemo.programs.copaua0c.copybook;

import static com.carddemo.programs.copaua0c.support.Cobol.alpha;
import static com.carddemo.programs.copaua0c.support.Cobol.spaces;

/**
 * CCPAUERY - 01 ERROR-LOG-RECORD (122 bytes, written to TD queue CSSL).
 * Mutable: COPAUA0C never re-initialises it, so fields not set by an error path keep
 * the value from the previous error.
 */
public final class ErrorLogRecord {

    public static final int RECORD_LENGTH = 122;

    /** 05 ERR-LEVEL 88-levels. */
    public enum Level {
        LOG('L'), INFO('I'), WARNING('W'), CRITICAL('C');

        private final char code;

        Level(char code) { this.code = code; }

        public char code() { return code; }
    }

    /** 05 ERR-SUBSYSTEM 88-levels. */
    public enum Subsystem {
        APP('A'), CICS('C'), IMS('I'), DB2('D'), MQ('M'), FILE('F');

        private final char code;

        Subsystem(char code) { this.code = code; }

        public char code() { return code; }
    }

    private String date = spaces(6);          // ERR-DATE        PIC X(06)
    private String time = spaces(6);          // ERR-TIME        PIC X(06)
    private String application = spaces(8);   // ERR-APPLICATION PIC X(08)
    private String program = spaces(8);       // ERR-PROGRAM     PIC X(08)
    private String location = spaces(4);      // ERR-LOCATION    PIC X(04)
    private char level = ' ';                 // ERR-LEVEL       PIC X(01)
    private char subsystem = ' ';             // ERR-SUBSYSTEM   PIC X(01)
    private String code1 = spaces(9);         // ERR-CODE-1      PIC X(09)
    private String code2 = spaces(9);         // ERR-CODE-2      PIC X(09)
    private String message = spaces(50);      // ERR-MESSAGE     PIC X(50)
    private String eventKey = spaces(20);     // ERR-EVENT-KEY   PIC X(20)

    public String date() { return date; }
    public void date(String v) { date = alpha(v, 6); }
    public String time() { return time; }
    public void time(String v) { time = alpha(v, 6); }
    public String application() { return application; }
    public void application(String v) { application = alpha(v, 8); }
    public String program() { return program; }
    public void program(String v) { program = alpha(v, 8); }
    public String location() { return location; }
    public void location(String v) { location = alpha(v, 4); }
    public char levelCode() { return level; }
    public void level(Level v) { level = v.code(); }
    public char subsystemCode() { return subsystem; }
    public void subsystem(Subsystem v) { subsystem = v.code(); }
    public String code1() { return code1; }
    public void code1(String v) { code1 = alpha(v, 9); }
    public String code2() { return code2; }
    public void code2(String v) { code2 = alpha(v, 9); }
    public String message() { return message; }
    public void message(String v) { message = alpha(v, 50); }
    public String eventKey() { return eventKey; }
    public void eventKey(String v) { eventKey = alpha(v, 20); }

    public boolean isLog() { return level == Level.LOG.code(); }
    public boolean isInfo() { return level == Level.INFO.code(); }
    public boolean isWarning() { return level == Level.WARNING.code(); }
    public boolean isCritical() { return level == Level.CRITICAL.code(); }
    public boolean isApp() { return subsystem == Subsystem.APP.code(); }
    public boolean isCics() { return subsystem == Subsystem.CICS.code(); }
    public boolean isIms() { return subsystem == Subsystem.IMS.code(); }
    public boolean isDb2() { return subsystem == Subsystem.DB2.code(); }
    public boolean isMq() { return subsystem == Subsystem.MQ.code(); }
    public boolean isFile() { return subsystem == Subsystem.FILE.code(); }

    public ErrorLogRecord copy() {
        ErrorLogRecord c = new ErrorLogRecord();
        c.date = date;
        c.time = time;
        c.application = application;
        c.program = program;
        c.location = location;
        c.level = level;
        c.subsystem = subsystem;
        c.code1 = code1;
        c.code2 = code2;
        c.message = message;
        c.eventKey = eventKey;
        return c;
    }

    /** Fixed-width 122-byte image as written to CSSL. */
    public String toFixed() {
        return date + time + application + program + location + level + subsystem
                + code1 + code2 + message + eventKey;
    }
}
