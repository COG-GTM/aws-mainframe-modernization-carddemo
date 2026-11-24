import { describe, expect, it } from 'vitest';

import { db2FormatTimestamp } from '../src/timestamp.ts';

describe('db2FormatTimestamp', () => {
  it('builds the 26-character DB2 layout with hundredths and the literal 0000 tail', () => {
    const ts = db2FormatTimestamp(new Date(2022, 6, 18, 9, 5, 4, 30));
    expect(ts).toBe('2022-07-18-09.05.04.030000');
    expect(ts).toHaveLength(26);
  });

  it('truncates milliseconds to hundredths', () => {
    expect(db2FormatTimestamp(new Date(2022, 0, 1, 0, 0, 0, 999))).toBe('2022-01-01-00.00.00.990000');
  });
});
