/// <reference types="vitest/config" />
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

export default defineConfig({
  plugins: [react()],
  server: {
    port: 5173,
    host: true,
    // e.g. VITE_API_PROXY_TARGET=http://localhost:8080 with VITE_API_BASE_URL empty: same-origin /api/* like nginx/CloudFront
    proxy: process.env.VITE_API_PROXY_TARGET ? { '/api': process.env.VITE_API_PROXY_TARGET } : undefined,
  },
  preview: { port: 4173, host: true },
  build: { sourcemap: true, target: 'es2022' },
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: ['./src/test/setup.ts'],
    css: false,
    restoreMocks: true,
  },
});
