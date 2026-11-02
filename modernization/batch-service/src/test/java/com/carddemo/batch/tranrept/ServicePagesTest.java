package com.carddemo.batch.tranrept;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.web.client.RestClient;

/** A row inserted between page requests shifts offset paging; the unload must not double-count it. */
class ServicePagesTest {

    record Row(String id) {
    }

    private static final ParameterizedTypeReference<ServicePages.Page<Row>> TYPE = new ParameterizedTypeReference<>() { };

    private HttpServer server;

    @AfterEach
    void stop() {
        server.stop(0);
    }

    @Test
    void retriesWhenARowIsInsertedBetweenPages() throws IOException {
        List<String> ids = new ArrayList<>(IntStream.rangeClosed(1, 201).mapToObj(i -> String.format("%04d", i)).toList());
        AtomicInteger requests = new AtomicInteger();
        RestClient client = serve(ids, requests, () -> {
            if (requests.get() == 2) {
                ids.add(0, "0000"); // inserted after page 0 of the first pass was served
            }
        });

        List<Row> rows = ServicePages.all(client, "/rows", TYPE, Row::id);

        assertThat(rows).extracting(Row::id).doesNotHaveDuplicates().hasSize(202).contains("0000", "0200");
        assertThat(requests.get()).isEqualTo(4); // first pass rejected, second pass consistent
    }

    @Test
    void failsWhenTheCollectionNeverHoldsStill() throws IOException {
        List<String> ids = new ArrayList<>(IntStream.rangeClosed(1, 201).mapToObj(i -> String.format("%04d", i)).toList());
        AtomicInteger requests = new AtomicInteger();
        RestClient client = serve(ids, requests, () -> ids.add(0, String.format("x%03d", requests.get())));

        assertThatThrownBy(() -> ServicePages.all(client, "/rows", TYPE, Row::id))
                .isInstanceOf(IllegalStateException.class).hasMessageContaining("no consistent unload");
    }

    private RestClient serve(List<String> ids, AtomicInteger requests, Runnable beforeEachResponse) throws IOException {
        server = HttpServer.create(new InetSocketAddress("localhost", 0), 0);
        server.createContext("/rows", exchange -> {
            requests.incrementAndGet();
            beforeEachResponse.run();
            int page = Integer.parseInt(exchange.getRequestURI().getQuery().replaceAll(".*page=(\\d+).*", "$1"));
            int size = ServicePages.PAGE_SIZE;
            String content = ids.subList(Math.min(page * size, ids.size()), Math.min((page + 1) * size, ids.size()))
                    .stream().map(id -> "{\"id\":\"" + id + "\"}").collect(Collectors.joining(","));
            byte[] body = ("{\"content\":[" + content + "],\"page\":" + page + ",\"totalElements\":" + ids.size()
                    + ",\"totalPages\":" + ((ids.size() + size - 1) / size) + "}").getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        server.start();
        return RestClient.create("http://localhost:" + server.getAddress().getPort());
    }
}
