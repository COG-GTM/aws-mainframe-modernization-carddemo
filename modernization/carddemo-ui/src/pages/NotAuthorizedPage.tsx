import { Screen } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { page } from '../programs';

const PAGE = page('COADM01C');

/** A USER token on an admin route: the API's NOTAUTH text (UserAdminMessages / COADM01C), nothing else rendered. */
export function NotAuthorizedPage() {
  const { back } = useProgramNav();
  return (
    <Screen
      page={PAGE}
      message={{ text: 'No access - Admin Only option...', kind: 'error' }}
      pfKeys={[{ key: 'F3', label: 'Exit', action: () => back(PAGE) }]}
    />
  );
}
