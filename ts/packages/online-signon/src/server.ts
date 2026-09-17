/**
 * Entry point: serves the signon and menu programs over HTTP.
 */

import { createApp } from "./http.js";

const port = Number(process.env["PORT"] ?? 3000);

createApp().listen(port, () => {
  process.stdout.write(`carddemo online-signon listening on port ${port}\n`);
});
