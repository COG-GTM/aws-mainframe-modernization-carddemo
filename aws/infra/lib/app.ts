import { type App, type Environment, Tags } from 'aws-cdk-lib';
import { BatchStack } from './batch-stack';
import { flowsFor } from './catalog/flows';
import { type CardDemoConfig, loadConfig } from './config';
import { DataStack } from './data-stack';
import { MessagingStack } from './messaging-stack';
import { NetworkStack } from './network-stack';
import { ObservabilityStack } from './observability-stack';
import { ServicesStack } from './services-stack';
import { StorageStack } from './storage-stack';

export interface CardDemoStacks {
  config: CardDemoConfig;
  network: NetworkStack;
  storage: StorageStack;
  data: DataStack;
  messaging: MessagingStack;
  observability: ObservabilityStack;
  services: ServicesStack;
  batch: BatchStack;
}

/** Builds all CardDemo stacks into `app`. Stack ids: `CardDemo-<env>-<Name>`. */
export function buildApp(app: App, config: CardDemoConfig = loadConfig(app)): CardDemoStacks {
  const env: Environment = { account: config.account, region: config.region };
  const id = (name: string) => `CardDemo-${config.envName}-${name}`;
  const flows = flowsFor(config.enableAuthModule);

  const network = new NetworkStack(app, id('Network'), { env, config });
  const storage = new StorageStack(app, id('Storage'), { env, config });
  const data = new DataStack(app, id('Data'), {
    env,
    config,
    vpc: network.vpc,
    dbSg: network.dbSg,
    bootstrapSg: network.bootstrapSg,
    appSubnets: network.appSubnets,
  });
  const messaging = new MessagingStack(app, id('Messaging'), { env, config });
  const observability = new ObservabilityStack(app, id('Observability'), { env, config, queues: messaging.queues, flows });
  const shared = {
    env,
    config,
    vpc: network.vpc,
    appSubnets: network.appSubnets,
    dbHost: data.cluster.clusterEndpoint.hostname,
    dbSecret: data.secret,
    dataBucket: storage.dataBucket,
    messaging,
  };
  const services = new ServicesStack(app, id('Services'), {
    ...shared,
    albSg: network.albSg,
    servicesSg: network.servicesSg,
    logGroup: observability.servicesLogGroup,
  });
  const batch = new BatchStack(app, id('Batch'), {
    ...shared,
    flows,
    batchSg: network.batchSg,
    logGroup: observability.batchLogGroup,
    warningTopic: observability.warningTopic,
  });

  // Explicit: services and batch must not start before the Data stack (incl. schema bootstrap) is complete.
  services.addDependency(data);
  batch.addDependency(data);

  Tags.of(app).add('Application', 'CardDemo');
  Tags.of(app).add('Environment', config.envName);
  Tags.of(app).add('ManagedBy', 'aws-cdk');

  return { config, network, storage, data, messaging, observability, services, batch };
}
