import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { GetSecretValueCommand, SecretsManagerClient } from '@aws-sdk/client-secrets-manager';
import { Client } from 'pg';

/** CloudFormation custom resource event (subset used here). */
interface CfnEvent {
  RequestType: 'Create' | 'Update' | 'Delete';
  PhysicalResourceId?: string;
  ResourceProperties: { SchemaHash: string; Schema: string };
}

const sm = new SecretsManagerClient({});

/** Applies the bundled schema.sql (idempotent DDL) to the Aurora cluster. Delete is a no-op (data is kept). */
export async function handler(event: CfnEvent): Promise<{ PhysicalResourceId: string; Data?: Record<string, string> }> {
  const schema = event.ResourceProperties.Schema;
  const physicalId = event.PhysicalResourceId ?? `carddemo-schema-${schema}`;
  if (event.RequestType === 'Delete') return { PhysicalResourceId: physicalId };
  if (!/^[a-z_][a-z0-9_]*$/.test(schema)) throw new Error(`Invalid schema name ${schema}`);

  const secretValue = await sm.send(new GetSecretValueCommand({ SecretId: process.env.DB_SECRET_ARN }));
  const secret = JSON.parse(secretValue.SecretString ?? '{}') as { username: string; password: string };
  const sql = readFileSync(join(__dirname, 'schema.sql'), 'utf8');

  const client = new Client({
    host: process.env.DB_HOST,
    port: Number(process.env.DB_PORT ?? '5432'),
    database: process.env.DB_NAME,
    user: secret.username,
    password: secret.password,
    ssl: true,
    connectionTimeoutMillis: 30000,
  });
  await client.connect();
  try {
    await client.query('BEGIN');
    await client.query(`CREATE SCHEMA IF NOT EXISTS ${schema}`);
    await client.query(`SET LOCAL search_path TO ${schema}, public`);
    await client.query(sql);
    await client.query('COMMIT');
  } catch (err) {
    await client.query('ROLLBACK');
    throw err;
  } finally {
    await client.end();
  }
  console.log(JSON.stringify({ msg: 'schema applied', schema, schemaHash: event.ResourceProperties.SchemaHash }));
  return { PhysicalResourceId: physicalId, Data: { SchemaHash: event.ResourceProperties.SchemaHash } };
}
