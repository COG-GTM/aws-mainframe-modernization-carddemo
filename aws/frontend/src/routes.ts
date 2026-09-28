/** BMS map -> React route (aws/contracts/api.md §9). */
export const BMS_ROUTES = [
  { map: 'COSGN00 / COSGN0A', program: 'COSGN00C', tranId: 'CC00', route: '/login' },
  { map: 'COMEN01 / COMEN1A', program: 'COMEN01C', tranId: 'CM00', route: '/menu' },
  { map: 'COADM01 / COADM1A', program: 'COADM01C', tranId: 'CA00', route: '/admin' },
  { map: 'COACTVW / CACTVWA', program: 'COACTVWC', tranId: 'CAVW', route: '/accounts/view' },
  { map: 'COACTUP / CACTUPA', program: 'COACTUPC', tranId: 'CAUP', route: '/accounts/update' },
  { map: 'COCRDLI / CCRDLIA', program: 'COCRDLIC', tranId: 'CCLI', route: '/cards' },
  { map: 'COCRDSL / CCRDSLA', program: 'COCRDSLC', tranId: 'CCDL', route: '/cards/view' },
  { map: 'COCRDUP / CCRDUPA', program: 'COCRDUPC', tranId: 'CCUP', route: '/cards/update' },
  { map: 'COTRN00 / COTRN0A', program: 'COTRN00C', tranId: 'CT00', route: '/transactions' },
  { map: 'COTRN01 / COTRN1A', program: 'COTRN01C', tranId: 'CT01', route: '/transactions/view' },
  { map: 'COTRN02 / COTRN2A', program: 'COTRN02C', tranId: 'CT02', route: '/transactions/new' },
  { map: 'CORPT00 / CORPT0A', program: 'CORPT00C', tranId: 'CR00', route: '/reports' },
  { map: 'COBIL00 / COBIL0A', program: 'COBIL00C', tranId: 'CB00', route: '/bill-payment' },
  { map: 'COUSR00 / COUSR0A', program: 'COUSR00C', tranId: 'CU00', route: '/admin/users' },
  { map: 'COUSR01 / COUSR1A', program: 'COUSR01C', tranId: 'CU01', route: '/admin/users/new' },
  { map: 'COUSR02 / COUSR2A', program: 'COUSR02C', tranId: 'CU02', route: '/admin/users/:userId/edit' },
  { map: 'COUSR03 / COUSR3A', program: 'COUSR03C', tranId: 'CU03', route: '/admin/users/:userId/delete' },
] as const;

/** Menu targets served by this app; `:userId` screens are also reachable without a key. */
export const IMPLEMENTED_ROUTES: ReadonlySet<string> = new Set(
  BMS_ROUTES.map((r) => menuTarget(r.route)),
);

/** Menu options that take a key (`/admin/users/:userId/edit`) open the keyed screen with an empty key field. */
export function menuTarget(route: string): string {
  return route.replace('/:userId', '');
}
