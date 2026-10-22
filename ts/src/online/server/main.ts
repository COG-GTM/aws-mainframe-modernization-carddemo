/** `npm run online` - serves the CardDemo online application (seed data, in memory). */
import { createOnlineServer } from './http.js';

const port = Number(process.env['PORT'] ?? 8080);
createOnlineServer().listen(port, () => {
  console.log(`CardDemo online: http://localhost:${port}/  (API: POST /api/transaction/CC00)`);
});
