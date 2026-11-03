package com.carddemo.testsupport.flow;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A stand-in for another CardDemo service at the HTTP boundary: a JDK HTTP server whose routes the flow
 * test backs with the loaded sample data, so the service under test runs its real REST client (URLs,
 * JSON, token forwarding) without the other service's process. Records every call with its
 * {@code Authorization} header.
 */
public final class ServiceStandIn implements AutoCloseable {

    public static final ObjectMapper JSON = new ObjectMapper().findAndRegisterModules()
            .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

    public record Request(String method, String path, Matcher match, Map<String, String> query, String body,
                          String authorization) {

        public String group(int i) {
            return match.group(i);
        }

        public <T> T body(Class<T> type) throws IOException {
            return JSON.readValue(body, type);
        }

        public int intParam(String name, int fallback) {
            return query.containsKey(name) ? Integer.parseInt(query.get(name)) : fallback;
        }
    }

    public record Response(int status, Object body) {

        public static Response ok(Object body) {
            return new Response(200, body);
        }

        public static Response notFound() {
            return new Response(404, Map.of("status", 404, "error", "Not Found", "message", "not found"));
        }
    }

    @FunctionalInterface
    public interface Handler {
        Response handle(Request request) throws Exception;
    }

    /** A recorded call: {@code METHOD /path} and the Authorization header it carried. */
    public record Call(String request, String authorization) {
    }

    private record Route(String method, Pattern path, Handler handler) {
    }

    private final HttpServer server;
    private final List<Route> routes = new CopyOnWriteArrayList<>();
    private final List<Call> calls = new CopyOnWriteArrayList<>();

    private ServiceStandIn(HttpServer server) {
        this.server = server;
    }

    public static ServiceStandIn start() {
        try {
            HttpServer server = HttpServer.create(new InetSocketAddress("localhost", 0), 0);
            ServiceStandIn standIn = new ServiceStandIn(server);
            server.createContext("/", standIn::dispatch);
            server.start();
            return standIn;
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    /** Adds a route; {@code pathRegex} must match the whole path, its groups are in {@link Request#group}. */
    public ServiceStandIn route(String method, String pathRegex, Handler handler) {
        routes.add(new Route(method, Pattern.compile(pathRegex), handler));
        return this;
    }

    public String url() {
        return "http://localhost:" + server.getAddress().getPort();
    }

    public List<Call> calls() {
        return List.copyOf(calls);
    }

    public void clearCalls() {
        calls.clear();
    }

    /** A {@code PageResponse} of {@code rows} for the request's {@code page}/{@code size} (default 0/20). */
    public static Map<String, Object> page(Request request, List<?> rows) {
        int page = request.intParam("page", 0);
        int size = request.intParam("size", 20);
        List<Object> content = new ArrayList<>(rows.subList(Math.min(page * size, rows.size()),
                Math.min((page + 1) * size, rows.size())));
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("content", content);
        body.put("page", page);
        body.put("size", size);
        body.put("totalElements", rows.size());
        body.put("totalPages", (rows.size() + size - 1) / size);
        return body;
    }

    @Override
    public void close() {
        server.stop(0);
    }

    private void dispatch(HttpExchange exchange) throws IOException {
        try (exchange) {
            String method = exchange.getRequestMethod();
            String path = exchange.getRequestURI().getPath();
            String authorization = exchange.getRequestHeaders().getFirst("Authorization");
            calls.add(new Call(method + " " + path, authorization));
            String body = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
            Response response = null;
            for (Route route : routes) {
                Matcher m = route.path().matcher(path);
                if (route.method().equals(method) && m.matches()) {
                    try {
                        response = route.handler().handle(new Request(method, path, m,
                                query(exchange.getRequestURI().getRawQuery()), body, authorization));
                    } catch (Exception ex) {
                        response = new Response(500, Map.of("status", 500, "message", String.valueOf(ex)));
                    }
                    break;
                }
            }
            if (response == null) {
                response = Response.notFound();
            }
            byte[] bytes = JSON.writeValueAsBytes(response.body());
            exchange.getResponseHeaders().add("Content-Type", "application/json");
            exchange.sendResponseHeaders(response.status(), bytes.length);
            exchange.getResponseBody().write(bytes);
        }
    }

    private static Map<String, String> query(String raw) {
        Map<String, String> query = new LinkedHashMap<>();
        if (raw != null && !raw.isEmpty()) {
            for (String pair : raw.split("&")) {
                String[] kv = pair.split("=", 2);
                query.put(URLDecoder.decode(kv[0], StandardCharsets.UTF_8),
                        kv.length > 1 ? URLDecoder.decode(kv[1], StandardCharsets.UTF_8) : "");
            }
        }
        return query;
    }
}
