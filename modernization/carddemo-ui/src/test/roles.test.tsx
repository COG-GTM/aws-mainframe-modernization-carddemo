import { screen, waitFor } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { PROGRAMS } from '../programs';
import { HEADER, mockApi, problem, renderAt, signedIn } from './harness';

const ADMIN_ROUTES = PROGRAMS.filter((p) => p.adminOnly).map((p) => p.route);

describe('role gating of admin pages', () => {
  it('admin-only maps are COADM01 and COUSR00-03', () => {
    expect(PROGRAMS.filter((p) => p.adminOnly).map((p) => p.mapset)).toEqual(['COADM01', 'COUSR00', 'COUSR01', 'COUSR02', 'COUSR03']);
  });

  it.each(ADMIN_ROUTES)('USER on %s sees the NOTAUTH message and no admin call is made', async (route) => {
    signedIn('USER');
    const { calls } = mockApi({});
    renderAt(route);
    expect(await screen.findByTestId('message-line')).toHaveTextContent('No access - Admin Only option...');
    expect(screen.queryByRole('table')).not.toBeInTheDocument();
    expect(calls.filter((c) => c.path.startsWith('/users') || c.path.startsWith('/menu/admin'))).toHaveLength(0);
  });

  it('ADMIN on the user list gets the page and the API rows', async () => {
    signedIn('ADMIN');
    mockApi({
      'GET /users': {
        body: {
          header: HEADER,
          pageNumber: 1,
          pageSize: 10,
          rows: [{ row: 1, userId: 'ADMIN001', firstName: 'MARGARET', lastName: 'GOLD', userType: 'A' }],
          hasPreviousPage: false,
          hasNextPage: false,
          message: '',
          exit: null,
        },
      },
    });
    renderAt('/admin/users');
    expect(await screen.findByText('MARGARET')).toBeInTheDocument();
  });

  it('a 403 NOTAUTH from the API is shown in the message area', async () => {
    signedIn('USER');
    mockApi({ 'GET /cards': problem(403, 'NOTAUTH', 'A regular user can only list the cards of the account in context: supply accountId', 'accountId') });
    renderAt('/cards');
    // USER without an account in context: list is not requested until ENTER
    screen.getByRole('button', { name: /ENTER/ }).click();
    await waitFor(() =>
      expect(screen.getByTestId('message-line')).toHaveTextContent('A regular user can only list the cards of the account in context: supply accountId'),
    );
  });

  it('no session: any page redirects to sign-on', async () => {
    mockApi({ 'GET /auth/login': { body: { header: HEADER, message: '', cursor: 'userId' } } });
    renderAt('/accounts/view');
    await waitFor(() => expect(screen.getByTestId('location')).toHaveTextContent('/signon'));
  });
});
