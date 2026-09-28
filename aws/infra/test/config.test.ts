import { loadConfig, queuePrefix, resourceName } from '../lib/config';

const ctx = (values: Record<string, unknown>) => ({ tryGetContext: (k: string) => values[k] });

describe('loadConfig', () => {
  test('offline defaults', () => {
    const cfg = loadConfig(ctx({}));
    expect(cfg.envName).toBe('dev');
    expect(cfg.availabilityZones).toHaveLength(3);
    expect(cfg.auroraMinAcu).toBe(0.5);
    expect(cfg.deletionProtection).toBe(false);
    expect(cfg.schemaFile.endsWith('aws/db/schema.sql')).toBe(true);
    expect(cfg.stateMachineDir.endsWith('aws/batch/aws/state-machine')).toBe(true);
    expect(cfg.batchCommandPrefix).toEqual(['java', '-jar', 'carddemo-batch.jar']);
  });

  test('prod enables protection and reader', () => {
    const cfg = loadConfig(ctx({ envName: 'prod', region: 'eu-west-1', azCount: '2' }));
    expect(cfg.deletionProtection).toBe(true);
    expect(cfg.retainData).toBe(true);
    expect(cfg.auroraReader).toBe(true);
    expect(cfg.availabilityZones).toEqual(['eu-west-1a', 'eu-west-1b']);
  });

  test('string context values from -c are parsed', () => {
    const cfg = loadConfig(ctx({ enableAuthModule: 'true', batchCommandPrefix: 'java,-jar,app.jar', natGateways: '0' }));
    expect(cfg.enableAuthModule).toBe(true);
    expect(cfg.batchCommandPrefix).toEqual(['java', '-jar', 'app.jar']);
    expect(cfg.natGateways).toBe(0);
  });

  test('rejects invalid values', () => {
    expect(() => loadConfig(ctx({ envName: 'Dev_1' }))).toThrow(/envName/);
    expect(() => loadConfig(ctx({ azCount: 4 }))).toThrow(/azCount/);
    expect(() => loadConfig(ctx({ auroraMinAcu: 8, auroraMaxAcu: 2 }))).toThrow(/auroraMinAcu/);
    expect(() => loadConfig(ctx({ natGateways: 'x' }))).toThrow(/number/);
  });

  test('naming helpers', () => {
    expect(resourceName({ envName: 'dev' }, 'alb')).toBe('carddemo-dev-alb');
    expect(queuePrefix({ envName: 'qa' })).toBe('carddemo-qa-');
  });
});
