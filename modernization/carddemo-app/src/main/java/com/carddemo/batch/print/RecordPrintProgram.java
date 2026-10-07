package com.carddemo.batch.print;

import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.file.FileStatusException;
import java.util.Optional;

/**
 * {@code CBACT02C} (card print), {@code CBACT03C} (xref print) and {@code CBCUS01C} (customer print): read the KSDS
 * sequentially and {@code DISPLAY} each record, once from the main loop and, in CBACT03C/CBCUS01C, once more from
 * {@code 1000-*-GET-NEXT} (the copy in CBACT02C is commented out). I/O errors take the programs'
 * {@code 9910-DISPLAY-IO-STATUS} + {@code 9999-ABEND-PROGRAM} path.
 */
public final class RecordPrintProgram {

    public enum Program {
        CBACT02C("CARDFILE", 1, "ERROR OPENING CARDFILE", "ERROR READING CARDFILE", "ERROR CLOSING CARDFILE"),
        CBACT03C("XREFFILE", 2, "ERROR OPENING XREFFILE", "ERROR READING XREFFILE", "ERROR CLOSING XREFFILE"),
        CBCUS01C("CUSTFILE", 2, "ERROR OPENING CUSTFILE", "ERROR READING CUSTOMER FILE",
                "ERROR CLOSING CUSTOMER FILE");

        private final String ddname;
        private final int displaysPerRecord;
        private final String openError;
        private final String readError;
        private final String closeError;

        Program(String ddname, int displaysPerRecord, String openError, String readError, String closeError) {
            this.ddname = ddname;
            this.displaysPerRecord = displaysPerRecord;
            this.openError = openError;
            this.readError = readError;
            this.closeError = closeError;
        }

        public String ddname() {
            return ddname;
        }
    }

    private final Program program;
    private final KsdsInput input;
    private final Sysout sysout;

    public RecordPrintProgram(Program program, KsdsInput input, Sysout sysout) {
        this.program = program;
        this.input = input;
        this.sysout = sysout;
    }

    public ProgramCounts run() {
        sysout.display("START OF EXECUTION OF PROGRAM " + program.name());
        try {
            input.open();
        } catch (FileStatusException e) {
            throw sysout.ioAbend(program.openError, e);
        }
        long read = 0;
        while (true) {
            Optional<FixedWidthRecord> record;
            try {
                record = input.readNext();
            } catch (FileStatusException e) {
                throw sysout.ioAbend(program.readError, e);
            }
            if (record.isEmpty()) {
                break;
            }
            read++;
            for (int i = 0; i < program.displaysPerRecord; i++) {
                sysout.display(record.get().text());
            }
        }
        try {
            input.close();
        } catch (FileStatusException e) {
            throw sysout.ioAbend(program.closeError, e);
        }
        sysout.display("END OF EXECUTION OF PROGRAM " + program.name());
        return new ProgramCounts(read, 0);
    }
}
