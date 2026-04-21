import { defineConfig } from '@playwright/test';

const isLocal = !process.env.BASE_URL || process.env.BASE_URL.includes('localhost');

export default defineConfig({
  testDir: './tests',
  reporter: [['html', { open: 'never' }], ['list']],
  use: {
    baseURL: process.env.BASE_URL ?? 'http://localhost:3000',
    screenshot: 'only-on-failure',
    trace: 'retain-on-failure',
  },
  expect: {
    timeout: 15_000,
  },
  webServer: isLocal
    ? [
        {
          command: 'npm run start',
          cwd: '../frontend',
          url: 'http://localhost:3000',
          reuseExistingServer: !process.env.CI,
        },
        {
          command: 'sbt "devServer/run 0"',
          cwd: '..',
          url: 'http://localhost:7000/healthcheck',
          reuseExistingServer: !process.env.CI,
          timeout: 180_000,
        },
      ]
    : [],
});
