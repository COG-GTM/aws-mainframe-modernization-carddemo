import type { Role } from './api/types';

export interface ProgramPage {
  program: string;
  tranId: string;
  mapset: string;
  map: string;
  title: string;
  route: string;
  adminOnly?: boolean;
}

/** One page per BMS map in app/bms (docs/modernization/08-ui-map.md). Routing follows NavigationContext.toProgram. */
export const PROGRAMS: ProgramPage[] = [
  { program: 'COSGN00C', tranId: 'CC00', mapset: 'COSGN00', map: 'COSGN0A', title: 'Sign On', route: '/signon' },
  { program: 'COMEN01C', tranId: 'CM00', mapset: 'COMEN01', map: 'COMEN1A', title: 'Main Menu', route: '/menu' },
  { program: 'COADM01C', tranId: 'CA00', mapset: 'COADM01', map: 'COADM1A', title: 'Admin Menu', route: '/admin', adminOnly: true },
  { program: 'COACTVWC', tranId: 'CAVW', mapset: 'COACTVW', map: 'CACTVWA', title: 'View Account', route: '/accounts/view' },
  { program: 'COACTUPC', tranId: 'CAUP', mapset: 'COACTUP', map: 'CACTUPA', title: 'Update Account', route: '/accounts/update' },
  { program: 'COCRDLIC', tranId: 'CCLI', mapset: 'COCRDLI', map: 'CCRDLIA', title: 'List Credit Cards', route: '/cards' },
  { program: 'COCRDSLC', tranId: 'CCDL', mapset: 'COCRDSL', map: 'CCRDSLA', title: 'View Credit Card Detail', route: '/cards/view' },
  { program: 'COCRDUPC', tranId: 'CCUP', mapset: 'COCRDUP', map: 'CCRDUPA', title: 'Update Credit Card Details', route: '/cards/update' },
  { program: 'COTRN00C', tranId: 'CT00', mapset: 'COTRN00', map: 'COTRN0A', title: 'List Transactions', route: '/transactions' },
  { program: 'COTRN01C', tranId: 'CT01', mapset: 'COTRN01', map: 'COTRN1A', title: 'View Transaction', route: '/transactions/view' },
  { program: 'COTRN02C', tranId: 'CT02', mapset: 'COTRN02', map: 'COTRN2A', title: 'Add Transaction', route: '/transactions/add' },
  { program: 'COBIL00C', tranId: 'CB00', mapset: 'COBIL00', map: 'COBIL0A', title: 'Bill Payment', route: '/bill-payment' },
  { program: 'CORPT00C', tranId: 'CR00', mapset: 'CORPT00', map: 'CORPT0A', title: 'Transaction Reports', route: '/reports' },
  { program: 'COUSR00C', tranId: 'CU00', mapset: 'COUSR00', map: 'COUSR0A', title: 'List Users', route: '/admin/users', adminOnly: true },
  { program: 'COUSR01C', tranId: 'CU01', mapset: 'COUSR01', map: 'COUSR1A', title: 'Add User', route: '/admin/users/add', adminOnly: true },
  { program: 'COUSR02C', tranId: 'CU02', mapset: 'COUSR02', map: 'COUSR2A', title: 'Update User', route: '/admin/users/update', adminOnly: true },
  { program: 'COUSR03C', tranId: 'CU03', mapset: 'COUSR03', map: 'COUSR3A', title: 'Delete User', route: '/admin/users/delete', adminOnly: true },
];

const BY_PROGRAM = new Map(PROGRAMS.map((p) => [p.program, p]));

export function page(program: string): ProgramPage {
  const found = BY_PROGRAM.get(program);
  if (!found) throw new Error(`No page for ${program}`);
  return found;
}

export function routeFor(program: string | null | undefined): string | undefined {
  return program ? BY_PROGRAM.get(program)?.route : undefined;
}

export function menuProgram(role: Role | undefined): string {
  return role === 'ADMIN' ? 'COADM01C' : 'COMEN01C';
}
