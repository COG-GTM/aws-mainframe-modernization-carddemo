/// <reference types="vitest/config" />
import react from '@vitejs/plugin-react';
import { defineConfig } from 'vite';

// `npm run dev` proxies the API to the Spring Boot app (default http://localhost:8084, see README).
const apiTarget = process.env.CARDDEMO_API_URL ?? 'http://localhost:8084';

export default defineConfig({
  plugins: [react()],
  server: {
    port: 5173,
    // the BMS field-coverage test reads the symbolic maps from the repository
    fs: { allow: ['.', '../../app/cpy-bms'] },
    proxy: {
      '/api': { target: apiTarget, changeOrigin: true },
      '/v3': { target: apiTarget, changeOrigin: true },
      '/swagger-ui': { target: apiTarget, changeOrigin: true },
    },
  },
  preview: { port: 4173 },
  build: { sourcemap: true, target: 'es2022' },
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: ['./src/test/setup.ts'],
    include: ['src/**/*.test.{ts,tsx}'],
    css: false,
    restoreMocks: true,
  },
});
