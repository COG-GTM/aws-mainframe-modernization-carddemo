export type { BmsField, BmsMapset, FieldColor, FieldHighlight, FieldIntensity, ScreenDefinition } from "./bms/types.js";
export { BmsParseError, joinContinuations, parseBms } from "./bms/parser.js";
export type {
  ScreenClient,
  ScreenExchangeRequest,
  ScreenExchangeResponse,
  ScreenState,
} from "./api/client.js";
export { ScreenExchangeError, createHttpScreenClient, parseScreenExchangeResponse } from "./api/client.js";
export {
  ADMIN_MENU_OPTIONS,
  MAIN_MENU_OPTIONS,
  createMockScreenClient,
  handleScreenExchange,
  initialScreen,
} from "./api/mock-backend.js";
export { SCREENS_BY_MAP, SCREENS_BY_MAPSET } from "./generated/index.js";
export { UnknownProgramError, findScreenForProgram, mapsetForProgram, screenForProgram } from "./screens.js";
export type { AidKey } from "./terminal/aid.js";
export { AID_KEYS, aidKeyForKeyboardEvent } from "./terminal/aid.js";
export type { PlacedField, ScreenFields } from "./terminal/screen-model.js";
export {
  displayValue,
  fieldKey,
  initialCursorField,
  initialValues,
  inputFields,
  placeFields,
  screenText,
} from "./terminal/screen-model.js";
export { Terminal } from "./components/Terminal.js";
export type { TerminalProps } from "./components/Terminal.js";
export { TerminalScreen } from "./components/TerminalScreen.js";
export type { TerminalScreenProps } from "./components/TerminalScreen.js";
