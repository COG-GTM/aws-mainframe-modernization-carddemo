/**
 * Equivalent of `9999-ABEND-PROGRAM` (`CBACT04C.cbl:628-632`): CEE3ABD with
 * abend code 999, timing 0 — immediate and non-retryable.
 */
export class AbendError extends Error {
  readonly abendCode = 999;

  constructor(message: string) {
    super(message);
    this.name = 'AbendError';
  }
}
