package com.carddemo.common.data;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordFormatException;
import com.carddemo.user.UserSecurityRecord;
import com.carddemo.user.UserType;
import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

class CopybookRecordMapperTest {

    record TranType(@CobolField("TRAN-TYPE") String type, @CobolField("TRAN-TYPE-DESC") String desc) {
    }

    record Unannotated(@CobolField("TRAN-TYPE") String type, String desc) {
    }

    record Unknown(@CobolField("TRAN-TYPE") String type, @CobolField("NO-SUCH-FIELD") String desc) {
    }

    record Partial(@CobolField("TRAN-TYPE") String type) {
    }

    record Twice(@CobolField("TRAN-TYPE") String type, @CobolField("TRAN-TYPE") String again) {
    }

    record WrongType(@CobolField("TRAN-TYPE") String type, @CobolField("TRAN-TYPE-DESC") BigDecimal desc) {
    }

    record CategoryAsLong(@CobolField("TRAN-TYPE-CD") String type, @CobolField("TRAN-CAT-CD") long cat,
                          @CobolField("TRAN-CAT-TYPE-DESC") String desc) {
    }

    @Test
    void mapsTextTrimmingTrailingSpacesAndPadsOnWrite() {
        CopybookRecordMapper<TranType> mapper = CopybookRecordMapper.of(TranType.class, "CVTRA03Y");
        FixedWidthRecord record = mapper.toRecord(new TranType("01", " Purchase"), RecordEncoding.ASCII);

        assertThat(record.length()).isEqualTo(60);
        assertThat(record.text()).isEqualTo("01" + String.format("%-50s", " Purchase") + " ".repeat(8));
        assertThat(mapper.fromRecord(record)).isEqualTo(new TranType("01", " Purchase"));
        assertThat(mapper.fieldToComponent()).containsExactly(
                java.util.Map.entry("TRAN-TYPE", "type"), java.util.Map.entry("TRAN-TYPE-DESC", "desc"));
    }

    @Test
    void writeIntoLeavesFillerBytesUntouched() {
        CopybookRecordMapper<TranType> mapper = CopybookRecordMapper.of(TranType.class, "CVTRA03Y");
        FixedWidthRecord record = FixedWidthRecord.fromLine(mapper.layout(), "99" + "x".repeat(50) + "FILLER!!",
                RecordEncoding.ASCII);
        mapper.writeInto(new TranType("02", "Payment"), record);
        assertThat(record.text()).endsWith("FILLER!!").startsWith("02Payment ");
    }

    @Test
    void rejectsBindingsThatDoNotCoverTheCopybookExactly() {
        assertThatThrownBy(() -> CopybookRecordMapper.of(Unannotated.class, "CVTRA03Y"))
                .hasMessageContaining("Unannotated.desc has no @CobolField");
        assertThatThrownBy(() -> CopybookRecordMapper.of(Unknown.class, "CVTRA03Y"))
                .hasMessageContaining("NO-SUCH-FIELD is not a stored leaf of CVTRA03Y");
        assertThatThrownBy(() -> CopybookRecordMapper.of(Partial.class, "CVTRA03Y"))
                .hasMessageContaining("does not bind CVTRA03Y [TRAN-TYPE-DESC]");
        assertThatThrownBy(() -> CopybookRecordMapper.of(Twice.class, "CVTRA03Y"))
                .hasMessageContaining("TRAN-TYPE is bound twice");
    }

    @Test
    void rejectsJavaTypesThatDoNotMatchThePicture() {
        assertThatThrownBy(() -> CopybookRecordMapper.of(WrongType.class, "CVTRA03Y"))
                .hasMessageContaining("WrongType.desc (BigDecimal) cannot hold TRAN-TYPE-DESC");
        assertThatThrownBy(() -> CopybookRecordMapper.of(CategoryAsLong.class, "CVTRA04Y"))
                .hasMessageContaining("CategoryAsLong.cat (long) cannot hold TRAN-CAT-CD PIC 9999");
    }

    @Test
    void textLongerThanThePictureIsRejectedNotTruncated() {
        CopybookRecordMapper<TranType> mapper = CopybookRecordMapper.of(TranType.class, "CVTRA03Y");
        assertThatThrownBy(() -> mapper.toRecord(new TranType("123", "x"), RecordEncoding.ASCII))
                .isInstanceOf(RecordFormatException.class);
    }

    @Test
    void levelEightyEightCodesMapToEnumsAndUndefinedCodesAreRejected() {
        CopybookRecordMapper<UserSecurityRecord> mapper = UserSecurityRecord.MAPPER;
        UserSecurityRecord admin = new UserSecurityRecord("ADMIN001", "Ann", "Admin", "PASSWORD", UserType.ADMIN);
        FixedWidthRecord record = mapper.toRecord(admin, RecordEncoding.EBCDIC);
        assertThat(mapper.fromRecord(record)).isEqualTo(admin);

        record.setString(record.field("SEC-USR-TYPE"), "X");
        assertThatThrownBy(() -> mapper.fromRecord(record)).isInstanceOf(InvalidRequestException.class)
                .hasMessageContaining("UserType: undefined code 'X'");
    }
}
