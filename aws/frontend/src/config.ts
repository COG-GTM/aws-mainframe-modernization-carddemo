export const API_BASE_URL = (import.meta.env.VITE_API_BASE_URL ?? '').replace(/\/+$/, '');
export const API_PREFIX = `${API_BASE_URL}/api/v1`;
export const USE_MOCKS = import.meta.env.VITE_USE_MOCKS === 'true';

export const TITLE01 = 'AWS Mainframe Modernization';
export const TITLE02 = 'CardDemo';
export const THANK_YOU = 'Thank you for using CCDA application... ';

export const PAGE_SIZE = { cards: 7, transactions: 10, users: 10 } as const;
