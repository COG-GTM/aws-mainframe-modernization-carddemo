import { defineConfig, devices } from '@playwright/test';

// End-to-end suite against the running docker compose stack (README "Web UI"): `docker compose up -d --build --wait`
// in modernization/, then `npm run e2e`. CARDDEMO_UI_URL overrides the default UI address.
export default defineConfig({
  testDir: './e2e',
  timeout: 90_000,
  expect: { timeout: 15_000 },
  fullyParallel: false,
  workers: 1,
  retries: 0,
  reporter: [['list'], ['html', { open: 'never' }]],
  use: {
    baseURL: process.env.CARDDEMO_UI_URL ?? `http://localhost:${process.env.CARDDEMO_UI_PORT ?? '8085'}`,
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure',
    acceptDownloads: true,
  },
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
});
