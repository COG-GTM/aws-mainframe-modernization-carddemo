package com.carddemo.programs.cbcus01c;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

final class Fixtures {

    /** Sample customer master shipped with CardDemo (repo root app/data/ASCII). */
    static final Path CUSTDATA = Path.of(System.getProperty("user.dir"))
            .resolve("../../../app/data/ASCII/custdata.txt").normalize();

    private Fixtures() {
    }

    static List<String> custdataLines() throws IOException {
        return Files.readAllLines(CUSTDATA, StandardCharsets.US_ASCII);
    }

    /** Builds a valid 500-byte CVCUS01Y image with the given id and first name. */
    static String customer(String id, String firstName) {
        return new com.carddemo.programs.cbcus01c.copybook.CustomerRecord(Long.parseLong(id), firstName, "", "",
                "", "", "", "", "", "", "", "", 0, "", "", "", "", 0, "").toFixedWidth();
    }
}
