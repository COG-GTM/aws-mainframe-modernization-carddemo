import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';
import { App } from 'aws-cdk-lib';
import { type CardDemoStacks, buildApp } from '../lib/app';

const cdkJsonContext = (JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'cdk.json'), 'utf8')) as { context: Record<string, unknown> }).context;

/** Builds the app offline with cdk.json context; asset bundling is skipped unless `bundle` is set. */
export function synthApp(context: Record<string, unknown> = {}, bundle = false): CardDemoStacks {
  const app = new App({
    context: {
      ...cdkJsonContext,
      schemaFile: path.join(os.tmpdir(), 'carddemo-infra-test-no-schema.sql'),
      stateMachineDir: path.join(os.tmpdir(), 'carddemo-infra-test-no-asl'),
      ...(bundle ? {} : { 'aws:cdk:bundling-stacks': [] }),
      ...context,
    },
  });
  return buildApp(app);
}

export function tempDir(prefix: string): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), prefix));
}
