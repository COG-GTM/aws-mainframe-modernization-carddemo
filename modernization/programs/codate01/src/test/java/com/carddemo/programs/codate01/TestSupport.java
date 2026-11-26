package com.carddemo.programs.codate01;

import com.carddemo.programs.codate01.copybook.MqTriggerMessage;
import com.carddemo.programs.codate01.gateway.InMemoryCicsGateway;
import com.carddemo.programs.codate01.gateway.InMemoryMqGateway;
import com.carddemo.programs.codate01.gateway.RetrieveResult;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

final class TestSupport {

    static final String INPUT_QUEUE = "CARD.DEMO.REQUEST.DATE";
    static final LocalDateTime NOW = LocalDateTime.of(2026, 11, 26, 17, 22, 5, 123_000_000);

    final InMemoryMqGateway mq = new InMemoryMqGateway()
            .defineQueue(INPUT_QUEUE)
            .defineQueue(Codate01Program.REPLY_QUEUE_NAME)
            .defineQueue(Codate01Program.ERROR_QUEUE_NAME);
    final List<String> displayed = new ArrayList<>();
    InMemoryCicsGateway cics = new InMemoryCicsGateway(
            RetrieveResult.normal(MqTriggerMessage.forQueue(INPUT_QUEUE, "CDRD")), () -> NOW);

    Codate01Program program() {
        return new Codate01Program(mq, cics, displayed::add);
    }

    Codate01Result run() {
        return program().run();
    }

    static String pad(String s, int len) {
        return CobolFields.alnum(s, len);
    }

    /** Expected MQ-ERR-DISPLAY rendering. */
    static String errDisplay(String para, String msg, String cc, String rc, String queue) {
        return pad(para, 25) + "  " + pad(msg, 25) + "  " + cc + "  " + rc + "  " + pad(queue, 48);
    }
}
