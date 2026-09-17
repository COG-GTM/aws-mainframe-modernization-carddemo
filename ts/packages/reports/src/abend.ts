/** `9999-ABEND-PROGRAM`: the batch programs abend on an unexpected file status. */
export class AbendError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "AbendError";
  }
}
