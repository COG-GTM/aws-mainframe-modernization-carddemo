import * as path from 'node:path';
import type { App } from 'aws-cdk-lib';

/** Deployment configuration, resolved from CDK context (`-c key=value` or cdk.json) with offline-safe defaults. */
export interface CardDemoConfig {
  readonly envName: string;
  readonly region: string;
  /** Undefined keeps the stacks account-agnostic so `cdk synth` needs no credentials. */
  readonly account?: string;
  readonly availabilityZones: string[];
  readonly vpcCidr: string;
  readonly natGateways: number;
  readonly imageTag: string;
  readonly auroraMinAcu: number;
  readonly auroraMaxAcu: number;
  readonly auroraReader: boolean;
  readonly deletionProtection: boolean;
  readonly retainData: boolean;
  readonly serviceDesiredCount: number;
  readonly logRetentionDays: number;
  readonly s3RetentionDays: number;
  readonly alarmEmail?: string;
  readonly certificateArn?: string;
  readonly albIngressCidr: string;
  /** Absolute path of the SQL file applied by the schema bootstrap custom resource. */
  readonly schemaFile: string;
  /** Absolute path of the directory holding `<flow>.asl.json` files produced by the batch session. */
  readonly stateMachineDir: string;
  readonly enableAuthModule: boolean;
  readonly batchCommandPrefix: string[];
  readonly batchMaxVcpus: number;
  readonly dailyCycleCron: string;
  readonly weeklyRefreshCron: string;
}

type ContextReader = { tryGetContext(key: string): unknown };

function str(ctx: ContextReader, key: string, fallback: string): string {
  const v = ctx.tryGetContext(key);
  return v === undefined || v === null || v === '' ? fallback : String(v);
}

function optStr(ctx: ContextReader, key: string): string | undefined {
  const v = ctx.tryGetContext(key);
  return v === undefined || v === null || v === '' ? undefined : String(v);
}

function num(ctx: ContextReader, key: string, fallback: number): number {
  const v = ctx.tryGetContext(key);
  if (v === undefined || v === null || v === '') return fallback;
  const n = Number(v);
  if (Number.isNaN(n)) throw new Error(`Context "${key}" must be a number, got "${String(v)}"`);
  return n;
}

function bool(ctx: ContextReader, key: string, fallback: boolean): boolean {
  const v = ctx.tryGetContext(key);
  if (v === undefined || v === null || v === '') return fallback;
  return v === true || v === 'true';
}

function strList(ctx: ContextReader, key: string, fallback: string[]): string[] {
  const v = ctx.tryGetContext(key);
  if (v === undefined || v === null || v === '') return fallback;
  if (Array.isArray(v)) return v.map(String);
  const s = String(v).trim();
  return s.startsWith('[') ? (JSON.parse(s) as unknown[]).map(String) : s.split(',').map((x) => x.trim());
}

export function loadConfig(scope: App | ContextReader, baseDir: string = path.resolve(__dirname, '..')): CardDemoConfig {
  const app: ContextReader = 'tryGetContext' in scope ? scope : scope.node;
  const envName = str(app, 'envName', 'dev');
  if (!/^[a-z][a-z0-9]{1,11}$/.test(envName)) {
    throw new Error(`envName "${envName}" must be 2-12 lower-case alphanumerics starting with a letter`);
  }
  const region = str(app, 'region', process.env.CDK_DEFAULT_REGION ?? 'us-east-1');
  const isProd = envName === 'prod';
  const auroraMinAcu = num(app, 'auroraMinAcu', 0.5);
  const auroraMaxAcu = num(app, 'auroraMaxAcu', 4);
  if (auroraMinAcu > auroraMaxAcu) throw new Error('auroraMinAcu must be <= auroraMaxAcu');
  const azCount = num(app, 'azCount', 3);
  if (azCount < 2 || azCount > 3) throw new Error('azCount must be 2 or 3');

  return {
    envName,
    region,
    account: optStr(app, 'account') ?? process.env.CDK_DEFAULT_ACCOUNT,
    availabilityZones: strList(app, 'availabilityZones', ['a', 'b', 'c'].slice(0, azCount).map((s) => `${region}${s}`)),
    vpcCidr: str(app, 'vpcCidr', '10.40.0.0/16'),
    natGateways: num(app, 'natGateways', 1),
    imageTag: str(app, 'imageTag', 'latest'),
    auroraMinAcu,
    auroraMaxAcu,
    auroraReader: bool(app, 'auroraReader', isProd),
    deletionProtection: bool(app, 'deletionProtection', isProd),
    retainData: bool(app, 'retainData', isProd),
    serviceDesiredCount: num(app, 'serviceDesiredCount', 1),
    logRetentionDays: num(app, 'logRetentionDays', 30),
    s3RetentionDays: num(app, 's3RetentionDays', 90),
    alarmEmail: optStr(app, 'alarmEmail'),
    certificateArn: optStr(app, 'certificateArn'),
    albIngressCidr: str(app, 'albIngressCidr', '0.0.0.0/0'),
    schemaFile: path.resolve(baseDir, str(app, 'schemaFile', '../db/schema.sql')),
    stateMachineDir: path.resolve(baseDir, str(app, 'stateMachineDir', '../batch/aws/state-machine')),
    enableAuthModule: bool(app, 'enableAuthModule', false),
    batchCommandPrefix: strList(app, 'batchCommandPrefix', ['java', '-jar', 'carddemo-batch.jar']),
    batchMaxVcpus: num(app, 'batchMaxVcpus', 16),
    dailyCycleCron: str(app, 'dailyCycleCron', 'cron(0 2 * * ? *)'),
    weeklyRefreshCron: str(app, 'weeklyRefreshCron', 'cron(0 3 ? * SAT *)'),
  };
}

/** `carddemo-<env>-<component>` (conventions.md §7). */
export function resourceName(cfg: Pick<CardDemoConfig, 'envName'>, component: string): string {
  return `carddemo-${cfg.envName}-${component}`;
}

/** Queue prefix handed to services as `SQS_QUEUE_PREFIX` (messaging.md §1). */
export function queuePrefix(cfg: Pick<CardDemoConfig, 'envName'>): string {
  return `carddemo-${cfg.envName}-`;
}
