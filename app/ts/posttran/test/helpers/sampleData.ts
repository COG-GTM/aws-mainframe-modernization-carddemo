import { fileURLToPath } from 'node:url';

/** `app/data/ASCII` — the repository's ASCII copies of the mainframe datasets. */
export const SAMPLE_DATA_DIR = fileURLToPath(new URL('../../../../data/ASCII', import.meta.url));
