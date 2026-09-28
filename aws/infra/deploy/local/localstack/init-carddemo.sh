#!/bin/sh
# LocalStack ready hook: S3 bucket + SQS queues/DLQs mirroring the CDK Messaging/Storage stacks
# (aws/contracts/messaging.md §1-2, batch.md §1.2) with the local prefix `carddemo-`.
set -eu
PREFIX="${SQS_QUEUE_PREFIX:-carddemo-}"
BUCKET="${S3_BUCKET:-carddemo-local}"

awslocal s3 mb "s3://${BUCKET}" || true
if [ -d /seed/ASCII ]; then awslocal s3 cp /seed/ASCII "s3://${BUCKET}/seed/ascii/" --recursive --quiet; fi
if [ -d /seed/EBCDIC ]; then awslocal s3 cp /seed/EBCDIC "s3://${BUCKET}/seed/ebcdic/" --recursive --quiet --exclude ".gitkeep"; fi

for q in pauth-request pauth-reply acct-inquiry-request acct-inquiry-reply date-inquiry-request date-inquiry-reply error report-request; do
  awslocal sqs create-queue --queue-name "${PREFIX}${q}-dlq" --attributes MessageRetentionPeriod=1209600 >/dev/null
  dlq_arn=$(awslocal sqs get-queue-attributes --queue-url "http://localhost:4566/000000000000/${PREFIX}${q}-dlq" \
    --attribute-names QueueArn --query Attributes.QueueArn --output text)
  case "$q" in *-reply) retention=60 ;; *) retention=345600 ;; esac
  cat > /tmp/attrs.json <<JSON
{"VisibilityTimeout":"30","ReceiveMessageWaitTimeSeconds":"5","MessageRetentionPeriod":"${retention}",
 "RedrivePolicy":"{\"deadLetterTargetArn\":\"${dlq_arn}\",\"maxReceiveCount\":\"5\"}"}
JSON
  awslocal sqs create-queue --queue-name "${PREFIX}${q}" --attributes file:///tmp/attrs.json >/dev/null
done
echo "carddemo: localstack resources ready"
touch /tmp/carddemo-ready
