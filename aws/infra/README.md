# CardDemo infrastructure (AWS CDK v2, TypeScript)

Deployable AWS infrastructure for the refactored CardDemo application. Synthesizes offline (no AWS credentials,
account-agnostic stacks) and is covered by CDK assertion tests.

## Stacks

All stacks are named `CardDemo-<env>-<Name>`; physical resources are named `carddemo-<env>-<component>`
(`contracts/conventions.md` §7).

| Stack | Contents |
|---|---|
| `Network` | VPC (`10.40.0.0/16`, 2-3 AZs) with public / private-with-egress / isolated subnets, NAT (default 1), S3 gateway endpoint, interface endpoints for SQS, ECR API + DKR, Secrets Manager, CloudWatch Logs, Step Functions, STS; security groups (ALB, services, batch, schema bootstrap, DB); rejected-traffic flow logs |
| `Storage` | Encrypted, versioned, SSL-only data bucket (`S3_BUCKET`; prefixes of `contracts/batch.md` §1.2: `seed/`, `input/`, rejects, `output/`, `reports/`, `statements/`, `backup/`, `export/`, `import/`, `extract/`, `runs/`) with GDG-style expiry on transient prefixes, IA transition for `backup/`, archival for `statements/`; server access log bucket |
| `Data` | Aurora PostgreSQL 16 Serverless v2 (0.5-4 ACU, isolated subnets, DB `carddemo`), credentials in Secrets Manager (`carddemo/<env>/db-credentials`), schema bootstrap custom resource applying `aws/db/schema.sql` (re-runs when the file hash changes; skipped with a warning when the file is absent) |
| `Messaging` | The 8 SQS queues of `contracts/messaging.md` (pauth, acct-inquiry, date-inquiry request/reply, error, report-request), each with a `-dlq`, `maxReceiveCount=5`, 14-day DLQ retention, 60 s reply retention, 30 s visibility, 5 s long polling, SSE, SSL-only |
| `Observability` | Log groups (services, batch, frontend), SNS alarm and batch-warning topics, DLQ-depth and error-queue alarms, Step Functions failed/timed-out/aborted alarms, AWS Batch `FAILED` event rule, dashboard |
| `Services` | ECR repos `online-services`, `frontend`; ECS Fargate service (port 8080, `/actuator/health`) behind an ALB; JWT secret; frontend on private S3 + CloudFront (OAC, SPA rewrite, `/api/*` to the ALB). The ALB forwards only requests carrying CloudFront's `X-CardDemo-Origin-Verify` header (value in Secrets Manager `carddemo/<env>/cloudfront-origin-verify`); direct ALB calls get 403 |
| `Batch` | ECR repos `batch`, `etl`; AWS Batch Fargate compute environment + job queue; one job definition per batch job (`--job=<name>`, 1 vCPU / 2 GiB, per-job IAM role); ETL job definition; Step Functions state machines per flow; EventBridge schedules and event chaining; SQS-triggered report dispatcher Lambda |

### Batch flows

State machine definitions are loaded from `aws/batch/aws/state-machine/<flow>.asl.json` when the batch session
provides them (placeholders `${JobQueueArn}`, `${JobDefinition_<job_with_underscores>}` (e.g.
`${JobDefinition_post_daily_transactions}`), `${DataBucket}`, `${WarningTopicArn}`, `${Partition}`, `${Region}`,
`${AccountId}`, `${EnvName}`, `${JobDefinitionPrefix}`, `${StateMachinePrefix}` are substituted; any other
`${...}` placeholder fails synth). Otherwise an equivalent definition is generated from
`lib/catalog/flows.ts` that implements `contracts/batch.md`: `submitJob.sync`, read `runs/<runId>/<job>.json`,
RC 0 continue, RC 4 publish a warning and continue, anything else fail; `Wait` states replace `COBSWAIT`.
Chained flows inherit `runId`, `businessDate` and `force` from the parent execution's output.

| Flow | Trigger | Jobs |
|---|---|---|
| `daily-cycle` | `cron(0 2 * * ? *)` | [purge-expired-authorizations], post-daily-transactions, Wait 36 s |
| `daily-backup` | after `daily-cycle` succeeds | backup-transactions, Wait |
| `monthly-interest` | after `daily-backup`; gated to the 1st of the month (or `force`) | calculate-interest, combine-transactions, Wait |
| `statements` | after `monthly-interest` (skipped when it was skipped) | create-statements, statement-pdf, Wait |
| `weekly-trantype-refresh` | `cron(0 3 ? * SAT *)` | maintain-transaction-types, extract-transaction-types |
| `weekly-disclosure-refresh` | after `weekly-trantype-refresh` | backup/load `disclosure_group`, Wait |
| `report` | SQS `report-request` via dispatcher | transaction-report |
| `extracts`, `export`, `import` | manual `StartExecution` | extract-accounts + print-* (parallel), export-customer-data, import-customer-data |

`lib/catalog/jobs.ts` lists every job with its S3 read/write prefixes and legacy JCL lineage; the S3 grants of
each job role are derived from it. `purge-expired-authorizations` (CBPAUP0C) is only created with
`-c enableAuthModule=true` because the IMS authorization sub-app is a replatform candidate.

### IAM

- Services task role: consume request queues, send only to the allowlisted reply queues + `error` + `report-request`, read `reports/` and `statements/`, describe executions of the report state machine.
- Batch job roles: one per job, S3 access limited to that job's prefixes plus `runs/*`; DB credentials injected by the execution role only.
- State machines: `batch:SubmitJob` on their own job definitions + the queue, `s3:GetObject` on `runs/*`, `sns:Publish` to the warning topic.
- Report dispatcher: consume `report-request`, `states:StartExecution` on the report state machine only.
- Schema bootstrap: read the DB secret only.

## Prerequisites

- Node.js >= 20 and npm (tooling versions are pinned in `package.json`)
- For deployment only: AWS CLI v2, credentials for the target account, Docker (image builds, Lambda bundling uses local esbuild)

```bash
cd aws/infra
npm ci
npm run build     # tsc --noEmit
npm run lint      # eslint
npm test          # jest + aws-cdk-lib/assertions
npm run synth     # cdk synth, offline
```

## Configuration

Context values (`cdk.json` or `-c key=value`):

| Key | Default | Notes |
|---|---|---|
| `envName` | `dev` | `prod` enables deletion protection, retention and an Aurora reader |
| `region` / `account` | `us-east-1` / unset | Unset account keeps stacks environment-agnostic |
| `azCount` | `3` | 2 or 3 |
| `natGateways` | `1` | `0` makes private subnets isolated (VPC endpoints only) |
| `imageTag` | `latest` | Tag used for all ECR images |
| `auroraMinAcu` / `auroraMaxAcu` | `0.5` / `4` | |
| `serviceDesiredCount` | `1` | |
| `certificateArn` | unset | ACM certificate for the ALB: enables HTTPS between CloudFront and the ALB (recommended for anything beyond dev; without it viewers still use HTTPS to CloudFront but the CloudFront-to-ALB hop is HTTP) |
| `albIngressCidr` | `0.0.0.0/0` | |
| `alarmEmail` | unset | Subscribes an email to the alarm topic |
| `schemaFile` | `../db/schema.sql` | |
| `stateMachineDir` | `../batch/aws/state-machine` | |
| `enableAuthModule` | `false` | Adds the authorization purge job/flow step |
| `batchCommandPrefix` | `["java","-jar","carddemo-batch.jar"]` | Prepended to `--job=<name>` |
| `dailyCycleCron` / `weeklyRefreshCron` | `cron(0 2 * * ? *)` / `cron(0 3 ? * SAT *)` | |

## Deploy (not performed by the migration sessions)

```bash
export AWS_REGION=us-east-1 ENV_NAME=dev
cd aws/infra && npm ci
npx cdk bootstrap aws://<account>/$AWS_REGION
# 1. Create repositories/data plane first so images can be pushed before the services start
npx cdk deploy -c envName=$ENV_NAME -c region=$AWS_REGION -c account=<account> \
  CardDemo-$ENV_NAME-Network CardDemo-$ENV_NAME-Storage CardDemo-$ENV_NAME-Data CardDemo-$ENV_NAME-Messaging CardDemo-$ENV_NAME-Observability
```

The ECR repositories live in the `Services` and `Batch` stacks; on a first deployment push images right after
those stacks create them (or deploy with `-c serviceDesiredCount=0` first, push, then redeploy with `1`):

```bash
npx cdk deploy -c envName=$ENV_NAME -c serviceDesiredCount=0 --all
./deploy/build-and-push.sh                       # online-services, frontend, batch, etl -> carddemo-<env>-<name>:latest
npx cdk deploy -c envName=$ENV_NAME --all        # service scales to the configured count
```

Frontend upload (static build from `aws/frontend`):

```bash
BUCKET=$(aws cloudformation describe-stacks --stack-name CardDemo-$ENV_NAME-Services --query "Stacks[0].Outputs[?OutputKey=='SiteBucketName'].OutputValue" --output text)
DIST=$(aws cloudformation describe-stacks --stack-name CardDemo-$ENV_NAME-Services --query "Stacks[0].Outputs[?OutputKey=='DistributionId'].OutputValue" --output text)
aws s3 sync ../frontend/dist "s3://$BUCKET/" --delete
aws cloudfront create-invalidation --distribution-id "$DIST" --paths '/*'
```

## Schema and ETL load

- Schema: applied automatically by the `Data` stack custom resource from `aws/db/schema.sql`; editing the file and redeploying re-applies it (the script must be idempotent).
- Seed data: upload the mainframe sample files and run the ETL job definition:

```bash
DATA_BUCKET=$(aws cloudformation describe-stacks --stack-name CardDemo-$ENV_NAME-Storage --query "Stacks[0].Outputs[?OutputKey=='DataBucketName'].OutputValue" --output text)
aws s3 sync ../../app/data/ASCII  "s3://$DATA_BUCKET/seed/ascii/"
aws s3 sync ../../app/data/EBCDIC "s3://$DATA_BUCKET/seed/ebcdic/" --exclude .gitkeep
aws batch submit-job --job-name etl-seed --job-queue carddemo-$ENV_NAME-batch-queue --job-definition carddemo-$ENV_NAME-etl
```

## Trigger the daily cycle

Runs automatically at 02:00 UTC. Manual run:

```bash
SM=$(aws cloudformation describe-stacks --stack-name CardDemo-$ENV_NAME-Batch --query "Stacks[0].Outputs[?OutputKey=='DailyCycleStateMachineArn'].OutputValue" --output text)
aws stepfunctions start-execution --state-machine-arn "$SM" --name daily-2022-06-10 \
  --input '{"runId":"daily-2022-06-10","businessDate":"2022-06-10","force":false}'
aws s3 ls "s3://$DATA_BUCKET/runs/daily-2022-06-10/"     # per-job RC JSON
```

A single job can be run with `aws batch submit-job --job-definition carddemo-$ENV_NAME-<job> --job-queue carddemo-$ENV_NAME-batch-queue --container-overrides 'command=[...]'`.

## Teardown

```bash
npx cdk destroy -c envName=$ENV_NAME --all
```

Non-prod stacks delete buckets (auto-delete objects), the database and log groups. `prod` retains the Aurora
cluster (snapshot), buckets and secrets; delete those manually. ECR repositories are emptied on destroy outside prod.

## Cost notes (us-east-1, idle dev environment, indicative)

| Item | Driver | Approx. |
|---|---|---|
| NAT gateway | 1 x hourly + data | ~$33/month (`-c natGateways=0` removes it; tasks then rely on VPC endpoints) |
| Interface VPC endpoints | 8 endpoints x AZs x hourly | ~$175/month with 3 AZs; `-c azCount=2` cuts ~1/3 |
| Aurora Serverless v2 | 0.5 ACU minimum, 24/7 | ~$45/month + storage |
| ALB | hourly + LCU | ~$17/month |
| Fargate service | 1 task (1 vCPU / 2 GiB) | ~$36/month |
| AWS Batch / Step Functions / Lambda | per run | cents per daily cycle |
| S3, SQS, CloudWatch, CloudFront | usage based | low at demo volumes |

## Layout

```
bin/carddemo.ts            CDK app entry
lib/app.ts                 stack wiring
lib/config.ts              context parsing + naming helpers
lib/*-stack.ts             stacks
lib/catalog/               queue, job and flow catalogues; default ASL generator
lambda/schema-bootstrap/   custom resource applying aws/db/schema.sql
lambda/report-dispatcher/  SQS report-request -> Step Functions StartExecution
test/                      jest + CDK assertions
deploy/build-and-push.sh   docker build + push to ECR
deploy/local/              postgres/localstack init scripts for aws/docker-compose.yml
```
