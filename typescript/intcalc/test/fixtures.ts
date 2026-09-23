import { fileURLToPath } from 'node:url';

/** `app/data/ASCII` in the CardDemo repository — the batch job's sample data. */
export const ASCII_DATA_DIR = fileURLToPath(new URL('../../../app/data/ASCII', import.meta.url));
