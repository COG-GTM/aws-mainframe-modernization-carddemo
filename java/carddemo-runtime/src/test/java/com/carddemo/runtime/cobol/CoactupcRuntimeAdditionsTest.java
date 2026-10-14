package com.carddemo.runtime.cobol;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.copybook.AbendData;
import com.carddemo.common.copybook.AccountRecord;
import com.carddemo.common.copybook.CardDemoAsciiSeed;
import com.carddemo.common.copybook.CardXrefRecord;
import com.carddemo.common.copybook.CustomerRecord;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsDispatcher;
import com.carddemo.runtime.cics.CicsOutcome;
import com.carddemo.runtime.cics.CicsProgram;
import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cics.CicsResponse;
import com.carddemo.runtime.cics.PfKeys;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.FileResult;
import com.carddemo.runtime.vsam.KsdsFile;
import java.math.BigDecimal;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/** Runtime additions made for COACTUPC: NUMVAL(-C), numeric-edited MOVE, CEEDAYS, PF folding, backout, AIX. */
class CoactupcRuntimeAdditionsTest {

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {"'+      1,940.00'|1940.00", "' 12.5 '|12.5", "1234.56-|-1234.56",
        "12.50CR|-12.50", "12.50db|-12.50", "$1,000|1000", "-$ 5|-5", ".5|0.5", "007|7"})
    void numvalCValid(String in, String expected) {
        assertThat(CobolNumval.testNumvalC(in)).isZero();
        assertThat(CobolNumval.numvalC(in)).isEqualByComparingTo(expected);
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {"+-1", "''", "'   '", "1 2", "1.2.3", "12A", "-1-", ",100"})
    void numvalCInvalid(String in) {
        assertThat(CobolNumval.testNumvalC(in)).isNotZero();
        assertThatThrownBy(() -> CobolNumval.numvalC(in)).isInstanceOf(NumberFormatException.class);
    }

    @Test
    void numvalWithoutCurrencyRejectsCommasAndDollar() {
        assertThat(CobolNumval.testNumval(" 5")).isZero();
        assertThat(CobolNumval.numval("05")).isEqualByComparingTo("5");
        assertThat(CobolNumval.testNumval("1,000")).isNotZero();
        assertThat(CobolNumval.testNumval("$1")).isNotZero();
        assertThat(CobolNumval.testNumval("\0\0")).isNotZero();
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        "194.00|+ZZZ,ZZZ,ZZZ.99|'+        194.00'",
        "-1234567.89|+ZZZ,ZZZ,ZZZ.99|'-  1,234,567.89'",
        "0|+ZZZ,ZZZ,ZZZ.99|'+           .00'",
        "12345678901.5|+ZZZ,ZZZ,ZZZ.99|'+345,678,901.50'",
        "1.999|+ZZZ,ZZZ,ZZZ.99|'+          1.99'",
        "-5|-ZZ9.99|'-  5.00'",
        "5|-ZZ9.99|'   5.00'",
        "0|ZZZ9|'   0'",
        "0|ZZZZ|'    '",
        "12.5|$$$,$$9.99|'    $12.50'",
        "-5|999CR|'005CR'",
        "5|999CR|'005  '",
        "5|***9|'***5'",
        "-42|Z(4)9-|'   42-'",
    })
    void numericEdited(String value, String picture, String expected) {
        assertThat(NumericEdited.format(new BigDecimal(value), picture)).isEqualTo(expected);
    }

    @Test
    void integerOfDateAndCeedays() {
        assertThat(CobolDates.integerOfDate(16010101)).isEqualTo(1);
        assertThat(CobolDates.integerOfDate(20000101)).isEqualTo(145732);
        assertThat(CobolDates.integerOfDate(20230229)).isZero();
        assertThat(CobolDates.ceedays("20000101", "YYYYMMDD")).isEqualTo(new CobolDates.CeedaysResult(0, 0, 152385));
        assertThat(CobolDates.ceedays("15821015", "YYYYMMDD").lilian()).isEqualTo(1);
        assertThat(CobolDates.ceedays("2000-01-01", "YYYY-MM-DD").lilian()).isEqualTo(152385);
        assertThat(CobolDates.ceedays("20230229", "YYYYMMDD").msgNo()).isEqualTo(CobolDates.CEE2EC_BAD_DATE_VALUE);
        assertThat(CobolDates.ceedays("15821014", "YYYYMMDD").msgNo()).isEqualTo(CobolDates.CEE2EH_UNSUPPORTED_RANGE);
        assertThat(CobolDates.ceedays("2024AB01", "YYYYMMDD").msgNo()).isEqualTo(CobolDates.CEE2EO_NON_NUMERIC);
        assertThat(CobolDates.ceedays("240101", "YYMMDD").msgNo()).isEqualTo(CobolDates.CEE2EM_BAD_PICTURE);
        assertThat(CobolDates.ceedays("20230229", "YYYYMMDD").severity()).isEqualTo(3);
    }

    @Test
    void pfKeyFolding() {
        // CSSTRPFY: PF13-PF24 map to PFK01-PFK12
        assertThat(PfKeys.storePfKey(AidKey.ENTER)).isEqualTo("ENTER");
        assertThat(PfKeys.storePfKey(AidKey.PF3)).isEqualTo("PFK03");
        assertThat(PfKeys.storePfKey(AidKey.PF15)).isEqualTo("PFK03");
        assertThat(PfKeys.storePfKey(AidKey.PF24)).isEqualTo("PFK12");
        assertThat(PfKeys.storePfKey(AidKey.PA1)).isEqualTo("PA1  ");
        assertThat(PfKeys.storePfKey(AidKey.PA3, "PFK05")).isEqualTo("PFK05");
    }

    @Test
    void sharedCopybookLayouts() {
        assertThat(AccountRecord.LENGTH).isEqualTo(300);
        assertThat(CustomerRecord.LENGTH).isEqualTo(500);
        assertThat(CardXrefRecord.LENGTH).isEqualTo(50);
        assertThat(AbendData.LENGTH).isEqualTo(134);
        assertThat(new AbendData().asString()).isBlank();
        assertThat(AccountRecord.ACCT_GROUP_ID.offset()).isEqualTo(112);
        assertThat(CustomerRecord.CUST_FICO_CREDIT_SCORE.offset()).isEqualTo(329);
        List<String> accts = CardDemoAsciiSeed.records(AccountRecord.SEED_FILE, AccountRecord.LENGTH);
        assertThat(accts).hasSize(50);
        AccountRecord a = AccountRecord.of(accts.get(0));
        assertThat(AccountRecord.ACCT_ID.get(a).longValue()).isEqualTo(1);
        assertThat(AccountRecord.ACCT_CURR_BAL.get(a)).isEqualByComparingTo("194.00");
        assertThat(AccountRecord.ACCT_CREDIT_LIMIT.get(a)).isEqualByComparingTo("2020.00");
        CustomerRecord c = CustomerRecord.of(CardDemoAsciiSeed.records(CustomerRecord.SEED_FILE, 500).get(0));
        assertThat(CustomerRecord.CUST_FIRST_NAME.get(c).trim()).isEqualTo("Immanuel");
        assertThat(CustomerRecord.CUST_FICO_CREDIT_SCORE.get(c).intValue()).isEqualTo(274);
    }

    private static FileControl xrefFiles() {
        KsdsFile xref = KsdsFile.inMemory("CCXREF", 50, 0, 16);
        xref.load(CardDemoAsciiSeed.records(CardXrefRecord.SEED_FILE, 50));
        return new FileControl().register(xref).register(xref.path("CXACAIX", 25, 11, false));
    }

    @Test
    void alternateIndexPathRegistration() {
        FileControl fct = xrefFiles();
        FileResult r = fct.readPath("CXACAIX ", "00000000001");
        assertThat(r.resp()).isEqualTo(CicsResp.NORMAL);
        assertThat(PicX.fromBytes(r.data())).startsWith("9680294154603697000000001");
        assertThat(fct.readPath("CXACAIX", "00000000099").resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(fct.readPath("NOSUCH", "1").resp()).isEqualTo(CicsResp.FILENOTFOUND);
        assertThat(fct.path("CXACAIX")).isPresent();
        assertThat(fct.ksdsFiles()).hasSize(1);
    }

    private static String rec(KsdsFile f, String key) {
        return PicX.fromBytes(f.store().get(PicX.toBytes(key)).orElseThrow());
    }

    @Test
    void syncpointRollbackBacksOutRewrites() {
        KsdsFile f = KsdsFile.inMemory("TESTF", 10, 0, 3);
        f.load(List.of("001AAAAAAA", "002BBBBBBB"));
        FileControl fct = new FileControl().register(f);
        CicsContext ctx = CicsContext.builder().transid("TEST").fileControl(fct).build();

        assertThat(ctx.readForUpdate("TESTF", "001").isNormal()).isTrue();
        assertThat(ctx.rewrite("TESTF", "001XXXXXXX").isNormal()).isTrue();
        assertThat(ctx.write("TESTF", "003CCCCCCC").isNormal()).isTrue();
        assertThat(ctx.uncommittedChanges()).isEqualTo(2);
        ctx.syncpointRollback();
        assertThat(rec(f, "001")).isEqualTo("001AAAAAAA");
        assertThat(f.store().get(PicX.toBytes("003"))).isEmpty();
        assertThat(ctx.uncommittedChanges()).isZero();

        ctx.readForUpdate("TESTF", "002");
        ctx.rewrite("TESTF", "002YYYYYYY");
        ctx.syncpoint();
        ctx.syncpointRollback();
        assertThat(rec(f, "002")).isEqualTo("002YYYYYYY");
        // REWRITE without READ UPDATE: INVREQ, nothing logged
        assertThat(ctx.rewrite("TESTF", "002ZZZZZZZ").resp()).isEqualTo(CicsResp.INVREQ);
        assertThat(ctx.uncommittedChanges()).isZero();
    }

    @Test
    void abendBacksOutTheTask() {
        KsdsFile f = KsdsFile.inMemory("TESTF", 10, 0, 3);
        f.load(List.of("001AAAAAAA"));
        FileControl fct = new FileControl().register(f);
        CicsProgram p = new CicsProgram() {
            @Override
            public String programName() {
                return "TESTPGM";
            }

            @Override
            public java.util.Optional<String> transactionId() {
                return java.util.Optional.of("TEST");
            }

            @Override
            public void execute(CicsContext ctx) {
                ctx.readForUpdate("TESTF", "001");
                ctx.rewrite("TESTF", "001XXXXXXX");
                ctx.abend("9999");
            }
        };
        CicsResponse r = CicsDispatcher.run(p, CicsContext.builder().transid("TEST").fileControl(fct).build());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.ABEND);
        assertThat(rec(f, "001")).isEqualTo("001AAAAAAA");
    }
}
