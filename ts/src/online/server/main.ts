/** `npm run online` - serves the CardDemo online application (seed data, in memory). */
import { createOnlineServer } from './http.js';

const port = Number(process.env['PORT'] ?? 8080);
// No authentication: listen on loopback only unless HOST is set explicitly.
const host = process.env['HOST'] ?? '127.0.0.1';
createOnlineServer().listen(port, host, () => {
  console.log(`CardDemo online: http://${host}:${port}/  (API: POST /api/transaction/CC00)`);
});
