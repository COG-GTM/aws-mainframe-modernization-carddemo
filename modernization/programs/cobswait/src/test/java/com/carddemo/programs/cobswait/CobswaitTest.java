package com.carddemo.programs.cobswait;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cobswait.copybook.MvswaitTime;
import java.io.InputStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class CobswaitTest {

    private final List<MvswaitTime> calls = new ArrayList<>();
    private final MvsWait recorder = t -> {
        calls.add(t);
        return 0;
    };

    private Cobswait program(String sysin, AlphanumericToNumericMove move) {
        return new Cobswait(new LineSysinReader(new StringReader(sysin)), recorder, move);
    }

    /** Fixture: the instream SYSIN card from app/jcl/WAITSTEP.jcl (36 seconds). */
    @ParameterizedTest
    @EnumSource(AlphanumericToNumericMove.class)
    void waitstepJclCardWaits3600Centiseconds(AlphanumericToNumericMove move) throws Exception {
        try (InputStream in = getClass().getResourceAsStream("/fixtures/WAITSTEP.SYSIN")) {
            Cobswait p = new Cobswait(LineSysinReader.of(in), recorder, move);
            assertEquals(Cobswait.RC_OK, p.run());
            assertEquals("00003600", p.parmValue().value());
            assertEquals(List.of(new MvswaitTime(3600)), calls);
            assertEquals(36_000L, calls.get(0).millis());
        }
    }

    @Test
    void acceptReadsOnlyFirstSysinRecord() {
        Cobswait p = program("00000010\n00009999\n", AlphanumericToNumericMove.IBM_ENTERPRISE);
        p.acceptParmValue();
        assertEquals("00000010", p.parmValue().value());
    }

    @Test
    void acceptAtEndOfSysinLeavesParmValueSpaces() {
        Cobswait p = program("", AlphanumericToNumericMove.IBM_ENTERPRISE);
        assertEquals(Cobswait.RC_OK, p.run());
        assertEquals("        ", p.parmValue().value());
        assertEquals(List.of(MvswaitTime.ZERO), calls);
    }

    @Test
    void moveConvertsParmValueToBinary() {
        Cobswait p = program("00000250", AlphanumericToNumericMove.IBM_ENTERPRISE);
        p.acceptParmValue();
        p.moveParmValueToMvswaitTime();
        assertEquals(250L, p.mvswaitTime().centiseconds());
        assertTrue(calls.isEmpty());
    }

    @Test
    void callPassesMvswaitTimeOnce() {
        Cobswait p = program("00000001", AlphanumericToNumericMove.GNUCOBOL);
        p.run();
        assertEquals(List.of(new MvswaitTime(1)), calls);
    }

    @Test
    void stopRunReturnsMvswaitR15AsReturnCode() {
        Cobswait p = new Cobswait(() -> Optional.of("00000001"), t -> 0,
                AlphanumericToNumericMove.IBM_ENTERPRISE);
        assertEquals(0, p.run());
    }

    @Test
    void invalidParmAbendsS0c7BeforeCallOnIbm() {
        Cobswait p = program("0000.100", AlphanumericToNumericMove.IBM_ENTERPRISE);
        assertThrows(DataExceptionAbend.class, p::run);
        assertTrue(calls.isEmpty());
    }

    @Test
    void invalidParmWaitsZeroOnGnuCobol() {
        Cobswait p = program("0000.100", AlphanumericToNumericMove.GNUCOBOL);
        assertEquals(Cobswait.RC_OK, p.run());
        assertEquals(List.of(MvswaitTime.ZERO), calls);
    }
}
