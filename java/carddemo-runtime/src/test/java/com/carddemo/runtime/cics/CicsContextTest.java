package com.carddemo.runtime.cics;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.runtime.bms.SymbolicMap;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.KsdsFile;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;

class CicsContextTest {

    private static CicsProgram program(java.util.function.Consumer<CicsContext> body) {
        return new CicsProgram() {
            @Override
            public String programName() {
                return "TESTPGM ";
            }

            @Override
            public Optional<String> transactionId() {
                return Optional.of("TST1");
            }

            @Override
            public void execute(CicsContext ctx) {
                body.accept(ctx);
            }
        };
    }

    @Test
    void eibReflectsTransactionAidAndCommareaLength() {
        CicsContext ctx = CicsContext.builder().transid("TST1").aid(AidKey.PF7).commarea(new byte[] {1, 2, 3}).build();
        assertThat(ctx.eib().eibtrnid()).isEqualTo("TST1");
        assertThat(ctx.eib().eibaid()).isEqualTo(AidKey.PF7);
        assertThat(ctx.eib().eibcalen()).isEqualTo(3);
        assertThat(CicsContext.builder().build().eib().eibcalen()).isZero();
        assertThat(ctx.commarea()).containsExactly(1, 2, 3);
    }

    @Test
    void controlCommandsEndTheTaskWithTypedOutcomes() {
        CobolRecord ca = new CobolRecord(2);
        ca.move("OK");
        CicsResponse r = CicsDispatcher.run(program(c -> {
            c.xctl("NEXTPGM", ca);
            throw new AssertionError("XCTL must not return");
        }), CicsContext.builder().build());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("NEXTPGM ");
        assertThat(r.outcome().commarea()).containsExactly('O', 'K');

        r = CicsDispatcher.run(program(c -> c.returnTransid("TST1", ca)), CicsContext.builder().build());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(r.outcome().nextTransid()).isEqualTo("TST1");

        r = CicsDispatcher.run(program(c -> { }), CicsContext.builder().build());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN);

        r = CicsDispatcher.run(program(c -> c.abend("ABCD")), CicsContext.builder().build());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.ABEND);
        assertThat(r.outcome().abendCode()).isEqualTo("ABCD");
    }

    @Test
    void terminalIoAndMapfail() {
        CicsContext noInput = CicsContext.builder().build();
        SymbolicMap map = SymbolicMap.of("COSGN00", "COSGN0A");
        assertThat(noInput.receiveMap(map)).isEqualTo(CicsResp.MAPFAIL);
        assertThat(noInput.eib().eibresp()).isEqualTo(36);

        CicsContext ctx = CicsContext.builder().terminalInput(Map.of("USERID", "abc")).build();
        assertThat(ctx.receiveMap(map)).isEqualTo(CicsResp.NORMAL);
        assertThat(map.data("USERID")).isEqualTo("abc     ");
        ctx.sendMap(map, CicsContext.SendMapOption.ERASE, CicsContext.SendMapOption.CURSOR);
        ctx.sendText("hello", 8, true, true);
        CicsResponse r = new CicsResponse("X", CicsOutcome.returnToCics(), ctx.terminalOutputs(), ctx.eib());
        assertThat(r.lastMap().screen().value("USERID")).isEqualTo("abc     ");
        assertThat(r.lastText().text()).isEqualTo("hello   ");
        assertThat(r.terminalOutputs()).hasSize(2);
    }

    @Test
    void assignClockAndFileControlResp() {
        FileControl fct = new FileControl().register(KsdsFile.inMemory("F1", 4, 0, 2));
        fct.ksds("F1").orElseThrow().load(List.of("AAxx"));
        CicsContext ctx = CicsContext.builder().systemInfo(new SystemInfo("APPL1", "SYS1")).fileControl(fct)
                .clock(Clock.fixed(Instant.parse("2024-01-02T03:04:05Z"), ZoneOffset.UTC)).build();
        assertThat(ctx.assignApplid()).isEqualTo("APPL1   ");
        assertThat(ctx.assignSysid()).isEqualTo("SYS1");
        assertThat(ctx.currentDate()).startsWith("2024010203040500");
        assertThat(ctx.read("F1", "AA", 4).dataString()).isEqualTo("AAxx");
        assertThat(ctx.eib().eibresp()).isZero();
        assertThat(ctx.read("F1", "ZZ", 4).resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(ctx.eib().eibresp()).isEqualTo(13);
        assertThat(ctx.read("NOFILE", "AA", 4).resp()).isEqualTo(CicsResp.FILENOTFOUND);
        assertThat(ctx.eib().eibresp()).isEqualTo(12);
    }
}
