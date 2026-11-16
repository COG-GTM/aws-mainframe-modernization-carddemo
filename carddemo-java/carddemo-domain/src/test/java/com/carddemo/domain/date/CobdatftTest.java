package com.carddemo.domain.date;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/** Expected values follow the moves in {@code app/asm/COBDATFT.asm} over the {@code CODATECN} layout. */
class CobdatftTest {

    private static final String BLANK_MSG = " ".repeat(Codatecn.ERROR_MSG_LENGTH);
    private static final String INVALID_INPUT_MSG = "INVALID INPUT" + " ".repeat(Codatecn.ERROR_MSG_LENGTH - 13);

    @Test
    void cbact01cReissueDateDashedToCompact() {
        Codatecn out = Cobdatft.call(Codatecn.request('2', "2025-03-14", '2'));

        assertThat(out.outDate()).isEqualTo("20250314            ");
        assertThat(out.errorMsg()).isEqualTo(BLANK_MSG);
        assertThat(out.inpDate()).isEqualTo("2025-03-14          ");
    }

    @Test
    void compactToDashed() {
        assertThat(Cobdatft.call(Codatecn.request('1', "20250314", '1')).outDate()).isEqualTo("2025-03-14          ");
    }

    @Test
    void outputTypeIsOnlyCheckedForTheOppositeFormat() {
        assertThat(Cobdatft.call(Codatecn.request('1', "20250314", ' ')).outDate()).isEqualTo("2025-03-14          ");
        assertThat(Cobdatft.call(Codatecn.request('2', "2025-03-14", 'X')).outDate()).isEqualTo("20250314            ");
    }

    @Test
    void sameFormatInAndOutIsInvalidInput() {
        Codatecn compact = Cobdatft.call(Codatecn.request('1', "20250314", '2'));
        Codatecn dashed = Cobdatft.call(Codatecn.request('2', "2025-03-14", '1'));

        assertThat(compact.errorMsg()).isEqualTo(INVALID_INPUT_MSG);
        assertThat(compact.outDate()).isBlank();
        assertThat(dashed.errorMsg()).isEqualTo(INVALID_INPUT_MSG);
        assertThat(dashed.outDate()).isBlank();
    }

    @Test
    void unknownInputTypeIsInvalidInput() {
        assertThat(Cobdatft.call(Codatecn.request('3', "2025-03-14", '2')).errorMsg()).isEqualTo(INVALID_INPUT_MSG);
        assertThat(Cobdatft.call(Codatecn.request(' ', "2025-03-14", '2')).errorMsg()).isEqualTo(INVALID_INPUT_MSG);
    }

    @Test
    void dashedInputWithType1IsNotRejectedBecauseTheClcComparesTwentyBytes() {
        assertThat(Cobdatft.call(Codatecn.request('1', "2025-03-14", '1')).outDate()).isEqualTo("2025--0-3-          ");
    }

    @Test
    void separatorsAndDigitsAreNotChecked() {
        assertThat(Cobdatft.call(Codatecn.request('2', "2025/AB/CD", '2')).outDate()).isEqualTo("2025ABCD            ");
    }

    @Test
    void unwrittenBytesKeepTheirPreviousValue() {
        Codatecn previous = new Codatecn('2', "2025-03-14", '2', "XXXXXXXXXXXXXXXXXXXX", "OLD MESSAGE");

        Codatecn ok = Cobdatft.call(previous);
        Codatecn err = Cobdatft.call(new Codatecn('2', "2025-03-14", '1', "XXXXXXXXXXXXXXXXXXXX", "OLD MESSAGE"));

        assertThat(ok.outDate()).isEqualTo("20250314XXXXXXXXXXXX");
        assertThat(ok.errorMsg()).startsWith("OLD MESSAGE");
        assertThat(err.outDate()).isEqualTo("XXXXXXXXXXXXXXXXXXXX");
        assertThat(err.errorMsg()).isEqualTo(INVALID_INPUT_MSG);
    }

    @Test
    void recordFieldsKeepTheirPicWidths() {
        Codatecn rec = new Codatecn('2', "2025-03-14-and-more-than-twenty", '2', null, null);

        assertThat(rec.inpDate()).hasSize(Codatecn.DATE_LENGTH).isEqualTo("2025-03-14-and-more-");
        assertThat(rec.outDate()).hasSize(Codatecn.DATE_LENGTH).isBlank();
        assertThat(rec.errorMsg()).hasSize(Codatecn.ERROR_MSG_LENGTH).isBlank();
    }
}
