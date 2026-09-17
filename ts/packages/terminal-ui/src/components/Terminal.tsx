/**
 * The terminal session: holds the screen the program sent, what the operator
 * typed into it, and drives one screen exchange per AID key.
 */

import { useCallback, useState } from "react";
import type { JSX } from "react";

import type { ScreenClient, ScreenExchangeResponse } from "../api/client.js";
import { ScreenExchangeError } from "../api/client.js";
import { screenForProgram } from "../screens.js";
import type { AidKey } from "../terminal/aid.js";
import { TerminalScreen } from "./TerminalScreen.js";

export interface TerminalProps {
  readonly client: ScreenClient;
  /** The screen the session opens on, normally the signon map. */
  readonly initial: ScreenExchangeResponse;
}

export function Terminal({ client, initial }: TerminalProps): JSX.Element {
  const [response, setResponse] = useState<ScreenExchangeResponse>(initial);
  const [values, setValues] = useState<Record<string, string>>({ ...initial.screenFields });
  const [message, setMessage] = useState(initial.message);
  const [busy, setBusy] = useState(false);

  const screen = screenForProgram(response.program);

  const onFieldChange = useCallback((name: string, value: string): void => {
    setValues((current) => ({ ...current, [name]: value }));
  }, []);

  const onAidKey = useCallback(
    (aidKey: AidKey): void => {
      setBusy(true);
      client
        .exchange({
          program: response.program,
          screenFields: values,
          aidKey,
          state: response.state,
        })
        .then((next) => {
          setResponse(next);
          setValues({ ...next.screenFields });
          setMessage(next.message);
        })
        .catch((error: unknown) => {
          setMessage(
            error instanceof ScreenExchangeError || error instanceof Error
              ? error.message
              : "the screen exchange failed",
          );
        })
        .finally(() => {
          setBusy(false);
        });
    },
    [client, response.program, response.state, values],
  );

  return (
    <div className="bms-terminal">
      <TerminalScreen
        screen={screen}
        values={values}
        onFieldChange={onFieldChange}
        onAidKey={onAidKey}
        busy={busy}
      />
      <p className="bms-status" role="status">
        {busy ? "X SYSTEM" : message}
      </p>
    </div>
  );
}
