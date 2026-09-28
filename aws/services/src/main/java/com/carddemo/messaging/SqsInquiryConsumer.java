package com.carddemo.messaging;

import com.carddemo.common.CardDemoProperties;
import com.carddemo.messaging.InquiryMessages.ErrorMessage;
import com.carddemo.messaging.InquiryMessages.InquiryRequest;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Function;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.SmartLifecycle;
import org.springframework.stereotype.Component;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.sqs.SqsClient;
import software.amazon.awssdk.services.sqs.model.Message;
import software.amazon.awssdk.services.sqs.model.MessageAttributeValue;
import software.amazon.awssdk.services.sqs.model.MessageSystemAttributeName;

/**
 * Long-polling SQS consumers replacing the MQ-triggered COACCT01 / CODATE01 transactions. Enabled with
 * {@code carddemo.messaging.enabled=true}; queues are provisioned by the infra session.
 */
@Component
@ConditionalOnProperty(prefix = "carddemo.messaging", name = "enabled", havingValue = "true")
public class SqsInquiryConsumer implements SmartLifecycle {

    private static final Logger LOG = LoggerFactory.getLogger(SqsInquiryConsumer.class);
    private static final DateTimeFormatter YYMMDD = DateTimeFormatter.ofPattern("yyMMdd");
    private static final DateTimeFormatter HHMMSS = DateTimeFormatter.ofPattern("HHmmss");
    private static final long RETRY_DELAY_MS = 5_000;

    private record Flow(String program, String requestQueue, String replyQueue,
            Function<InquiryRequest, Object> handler) {
    }

    private final CardDemoProperties properties;
    private final ObjectMapper objectMapper;
    private final Clock clock;
    private final List<Flow> flows;
    private volatile boolean running;
    private SqsClient sqs;
    private ExecutorService executor;

    public SqsInquiryConsumer(CardDemoProperties properties, InquiryService inquiries, ObjectMapper objectMapper,
            Clock clock) {
        this.properties = properties;
        this.objectMapper = objectMapper;
        this.clock = clock;
        CardDemoProperties.Aws aws = properties.aws();
        this.flows = List.of(
                new Flow("COACCT01", aws.queueName("acct-inquiry-request"), aws.queueName("acct-inquiry-reply"),
                        inquiries::accountInquiry),
                new Flow("CODATE01", aws.queueName("date-inquiry-request"), aws.queueName("date-inquiry-reply"),
                        inquiries::dateInquiry));
    }

    @Override
    public void start() {
        sqs = SqsClient.builder().region(Region.of(properties.aws().region())).build();
        executor = Executors.newFixedThreadPool(flows.size());
        running = true;
        flows.forEach(flow -> executor.submit(() -> poll(flow)));
    }

    @Override
    public void stop() {
        running = false;
        if (executor != null) {
            executor.shutdownNow();
        }
        if (sqs != null) {
            sqs.close();
        }
    }

    @Override
    public boolean isRunning() {
        return running;
    }

    private void poll(Flow flow) {
        String queueUrl = null;
        while (running) {
            try {
                if (queueUrl == null) {
                    queueUrl = queueUrl(flow.requestQueue());
                }
                String url = queueUrl;
                List<Message> messages = sqs.receiveMessage(b -> b.queueUrl(url).waitTimeSeconds(5)
                        .maxNumberOfMessages(10).visibilityTimeout(30).messageAttributeNames("All")
                        .messageSystemAttributeNames(MessageSystemAttributeName.ALL)).messages();
                for (Message message : messages) {
                    handle(flow, queueUrl, message);
                }
            } catch (RuntimeException ex) {
                if (running) {
                    LOG.error("SQS poll of {} failed", flow.requestQueue(), ex);
                    pauseAfterFailure();
                }
            }
        }
    }

    private void pauseAfterFailure() {
        try {
            Thread.sleep(RETRY_DELAY_MS);
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            running = false;
        }
    }

    private void handle(Flow flow, String queueUrl, Message message) {
        InquiryRequest request;
        try {
            request = objectMapper.readValue(message.body(), InquiryRequest.class);
        } catch (JsonProcessingException ex) {
            request = null;
        }
        if (request == null) {
            sendError(flow, null, "1000", "INVALID MESSAGE FORMAT", flow.requestQueue());
            delete(queueUrl, message);
            return;
        }
        MessageAttributeValue replyToAttr = message.messageAttributes().get("replyTo");
        String replyTo = replyToAttr == null ? flow.replyQueue() : replyToAttr.stringValue();
        if (!flow.replyQueue().equals(replyTo)) {
            sendError(flow, request.messageId(), "2000", "REPLY QUEUE NOT ALLOWED: " + replyTo, flow.requestQueue());
            delete(queueUrl, message);
            return;
        }
        Object reply = flow.handler().apply(request);
        send(replyTo, reply, request.messageId());
        delete(queueUrl, message);
    }

    private void sendError(Flow flow, UUID correlationId, String code, String text, String sourceQueue) {
        LocalDateTime now = LocalDateTime.now(clock);
        ErrorMessage error = new ErrorMessage("1", UUID.randomUUID(), correlationId, now.format(YYMMDD),
                now.format(HHMMSS), "CARDDEMO", flow.program(), "1000", "W", "M", code, "",
                text.length() > 50 ? text.substring(0, 50) : text, null, sourceQueue);
        send(properties.aws().queueName("error"), error, correlationId);
    }

    private void send(String queueName, Object body, UUID correlationId) {
        String json;
        try {
            json = objectMapper.writeValueAsString(body);
        } catch (JsonProcessingException ex) {
            throw new IllegalStateException(ex);
        }
        String url = queueUrl(queueName);
        sqs.sendMessage(b -> {
            b.queueUrl(url).messageBody(json);
            if (correlationId != null) {
                b.messageAttributes(Map.of("correlationId", MessageAttributeValue.builder().dataType("String")
                        .stringValue(correlationId.toString()).build()));
            }
        });
    }

    private void delete(String queueUrl, Message message) {
        sqs.deleteMessage(b -> b.queueUrl(queueUrl).receiptHandle(message.receiptHandle()));
    }

    private String queueUrl(String queueName) {
        return sqs.getQueueUrl(b -> b.queueName(queueName)).queueUrl();
    }
}
