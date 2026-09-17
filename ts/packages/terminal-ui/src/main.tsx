/** Dev entry point: mounts the terminal against the mock backend. */

import { StrictMode } from "react";
import { createRoot } from "react-dom/client";

import { createMockScreenClient, initialScreen } from "./api/mock-backend.js";
import { Terminal } from "./components/Terminal.js";
import "./terminal.css";

const container = document.getElementById("root");
if (container === null) {
  throw new Error("the #root container is missing from index.html");
}

createRoot(container).render(
  <StrictMode>
    <Terminal client={createMockScreenClient()} initial={initialScreen()} />
  </StrictMode>,
);
