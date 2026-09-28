package com.carddemo.report;

import com.carddemo.common.CardDemoProperties;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Map;
import java.util.UUID;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.sfn.SfnClient;
import software.amazon.awssdk.services.sfn.model.DescribeExecutionResponse;
import software.amazon.awssdk.services.sfn.model.ExecutionDoesNotExistException;
import software.amazon.awssdk.services.sfn.model.ExecutionStatus;
import software.amazon.awssdk.services.sqs.SqsClient;
import software.amazon.awssdk.services.sqs.model.MessageAttributeValue;

/** SQS publisher and Step Functions status lookup for the report flow (messaging.md section 6). */
@Configuration
public class AwsReportAdapters {

    static final String QUEUE = "report-request";

    @Bean(destroyMethod = "close")
    @ConditionalOnProperty(prefix = "carddemo.reports", name = "publisher", havingValue = "sqs")
    public SqsClient reportSqsClient(CardDemoProperties properties) {
        return SqsClient.builder().region(Region.of(properties.aws().region())).build();
    }

    @Bean
    @ConditionalOnProperty(prefix = "carddemo.reports", name = "publisher", havingValue = "sqs")
    public ReportPublisher sqsReportPublisher(SqsClient reportSqsClient, CardDemoProperties properties,
            ObjectMapper objectMapper) {
        String queueName = properties.aws().queueName(QUEUE);
        return request -> {
            String queueUrl = reportSqsClient.getQueueUrl(b -> b.queueName(queueName)).queueUrl();
            String body;
            try {
                body = objectMapper.writeValueAsString(request);
            } catch (JsonProcessingException ex) {
                throw new IllegalStateException("Cannot serialize report request", ex);
            }
            reportSqsClient.sendMessage(b -> b.queueUrl(queueUrl).messageBody(body)
                    .messageAttributes(Map.of("messageId", MessageAttributeValue.builder()
                            .dataType("String").stringValue(request.messageId().toString()).build())));
        };
    }

    @Bean(destroyMethod = "close")
    @ConditionalOnProperty(prefix = "carddemo.reports", name = "status", havingValue = "stepfunctions")
    public SfnClient reportSfnClient(CardDemoProperties properties) {
        return SfnClient.builder().region(Region.of(properties.aws().region())).build();
    }

    @Bean
    @ConditionalOnProperty(prefix = "carddemo.reports", name = "status", havingValue = "stepfunctions")
    public ReportStatusProvider stepFunctionsReportStatusProvider(SfnClient reportSfnClient,
            CardDemoProperties properties, ObjectMapper objectMapper) {
        String stateMachineArn = properties.reports().stateMachineArn();
        String executionPrefix = stateMachineArn.replace(":stateMachine:", ":execution:") + ":";
        return (UUID requestId) -> {
            DescribeExecutionResponse execution;
            try {
                execution = reportSfnClient.describeExecution(b -> b.executionArn(executionPrefix + requestId));
            } catch (ExecutionDoesNotExistException ex) {
                return new ReportStatusProvider.ReportStatus(ReportStatusProvider.Status.SUBMITTED, null);
            }
            ExecutionStatus status = execution.status();
            if (status == ExecutionStatus.RUNNING || status == ExecutionStatus.PENDING_REDRIVE) {
                return new ReportStatusProvider.ReportStatus(ReportStatusProvider.Status.RUNNING, null);
            }
            if (status == ExecutionStatus.SUCCEEDED) {
                String businessDate = businessDate(objectMapper, execution.input());
                String key = businessDate == null ? null
                        : "reports/tranrept/" + businessDate + "/" + requestId + ".txt";
                return new ReportStatusProvider.ReportStatus(ReportStatusProvider.Status.SUCCEEDED, key);
            }
            return new ReportStatusProvider.ReportStatus(ReportStatusProvider.Status.FAILED, null);
        };
    }

    private static String businessDate(ObjectMapper objectMapper, String input) {
        if (input == null) {
            return null;
        }
        try {
            String value = objectMapper.readTree(input).path("businessDate").asText(null);
            return value == null || value.isBlank() ? null : value;
        } catch (JsonProcessingException ex) {
            return null;
        }
    }
}
