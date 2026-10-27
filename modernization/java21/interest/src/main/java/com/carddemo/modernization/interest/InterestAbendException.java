package com.carddemo.modernization.interest;

import java.util.List;

/** INT-19: Carries DISPLAY lines and the file status for the program's abend (app/cbl/CBACT04C.cbl:628-648). */
public final class InterestAbendException extends RuntimeException {
    private final List<String> displayLines;
    private final String fileStatus;

    public InterestAbendException(List<String> displayLines, String fileStatus) {
        super(String.join(System.lineSeparator(), displayLines));
        this.displayLines = List.copyOf(displayLines);
        this.fileStatus = fileStatus;
    }

    public List<String> displayLines() {
        return displayLines;
    }

    public String fileStatus() {
        return fileStatus;
    }
}
