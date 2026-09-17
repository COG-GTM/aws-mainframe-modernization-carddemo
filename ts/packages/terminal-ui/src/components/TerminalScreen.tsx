/**
 * Renders one BMS map as a 24x80 green screen: literals where BMS put them,
 * inputs for the unprotected fields, and the AID keys the programs listen for.
 */

import { useEffect, useRef } from "react";
import type { CSSProperties, JSX, KeyboardEvent } from "react";

import type { BmsField, ScreenDefinition } from "../bms/types.js";
import type { AidKey } from "../terminal/aid.js";
import { aidKeyForKeyboardEvent } from "../terminal/aid.js";
import type { PlacedField, ScreenFields } from "../terminal/screen-model.js";
import { displayValue, initialCursorField, placeFields } from "../terminal/screen-model.js";

export interface TerminalScreenProps {
  readonly screen: ScreenDefinition;
  readonly values: ScreenFields;
  readonly onFieldChange: (name: string, value: string) => void;
  readonly onAidKey: (aidKey: AidKey) => void;
  /** Blocks input while a screen exchange is in flight, as `FREEKB` would. */
  readonly busy?: boolean;
}

function fieldClassName(field: BmsField): string {
  const classes = ["bms-field", `bms-color-${field.color ?? "default"}`, `bms-${field.intensity}`];
  if (field.highlight === "underline") {
    classes.push("bms-underline");
  }
  return classes.join(" ");
}

function gridStyle(placed: PlacedField): CSSProperties {
  return {
    gridRow: placed.field.row,
    gridColumn: `${String(placed.dataColumn)} / span ${String(placed.width)}`,
  };
}

export function TerminalScreen({
  screen,
  values,
  onFieldChange,
  onAidKey,
  busy = false,
}: TerminalScreenProps): JSX.Element {
  const containerRef = useRef<HTMLDivElement>(null);

  // The cursor goes to the `IC` field of a freshly sent map, and back there
  // once the exchange unlocks the keyboard.
  useEffect(() => {
    if (busy) {
      return;
    }
    const container = containerRef.current;
    if (container === null || container.contains(document.activeElement)) {
      return;
    }
    const cursorField = initialCursorField(screen);
    const target =
      cursorField === undefined
        ? container.querySelector("input")
        : container.querySelector<HTMLInputElement>(`input[name="${cursorField.name ?? ""}"]`);
    (target ?? container).focus();
  }, [screen, busy]);

  const handleKeyDown = (event: KeyboardEvent<HTMLDivElement>): void => {
    const aidKey = aidKeyForKeyboardEvent(event.key);
    if (aidKey === undefined) {
      return;
    }
    event.preventDefault();
    if (!busy) {
      onAidKey(aidKey);
    }
  };

  return (
    <div
      ref={containerRef}
      className="bms-screen"
      role="application"
      tabIndex={-1}
      aria-label={`${screen.mapset} ${screen.map}`}
      aria-busy={busy}
      onKeyDown={handleKeyDown}
      style={{
        gridTemplateColumns: `repeat(${String(screen.columns)}, 1ch)`,
        gridTemplateRows: `repeat(${String(screen.rows)}, var(--bms-row-height))`,
      }}
    >
      {placeFields(screen).map((placed) => {
        if (placed.width === 0) {
          return null;
        }

        const { field } = placed;
        if (placed.editable && field.name !== undefined) {
          return (
            <input
              key={placed.key}
              className={`${fieldClassName(field)} bms-input`}
              style={gridStyle(placed)}
              name={field.name}
              aria-label={field.name}
              type={field.intensity === "dark" ? "password" : "text"}
              inputMode={field.numeric ? "numeric" : "text"}
              maxLength={field.length}
              disabled={busy}
              value={displayValue(field, values)}
              onChange={(event) => {
                onFieldChange(field.name ?? "", event.target.value);
              }}
            />
          );
        }

        const text = field.intensity === "dark" ? "" : displayValue(field, values);
        return (
          <span
            key={placed.key}
            className={fieldClassName(field)}
            style={gridStyle(placed)}
            {...(field.name === undefined ? {} : { "data-field": field.name })}
          >
            {text}
          </span>
        );
      })}
    </div>
  );
}
