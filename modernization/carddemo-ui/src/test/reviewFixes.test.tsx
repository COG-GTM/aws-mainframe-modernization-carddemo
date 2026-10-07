import { fireEvent, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it } from 'vitest';
import { HEADER, mockApi, problem, renderAt, signedIn } from './harness';

const user = (userId: string, version: number) => ({ userId, firstName: 'Ann', lastName: 'Lee', password: 'PASSWORD', userType: 'U', version });
const userScreen = (userId: string, version: number) => ({ header: HEADER, state: 'SHOW', user: user(userId, version), message: 'Press PF5 key to delete this user ...', exit: null });

describe('screen header', () => {
  it('shows the date and time of the API ScreenHeader', async () => {
    signedIn('USER');
    mockApi({ 'GET /menu/main': { body: { header: { ...HEADER, currentDate: '07/06/22', currentTime: '23:59:58' }, options: [], optionLines: [], message: '' } } });
    renderAt('/menu');
    expect(await screen.findByText('23:59:58')).toBeInTheDocument();
    expect(screen.getByText('07/06/22')).toBeInTheDocument();
  });
});

describe('list paging at the edges', () => {
  it('F8 past the last page keeps the rows and shows the API message', async () => {
    signedIn('USER');
    const rows = Array.from({ length: 3 }, (_, i) => ({ row: i + 1, tranId: String(i + 1).padStart(16, '0'), date: '07/06/22', description: 'x', amount: '+1.00' }));
    const page = (after: string | null) => ({
      header: HEADER, pageNumber: 1, pageSize: 10, rows: after ? [] : rows, hasPreviousPage: false, hasNextPage: !after,
      previousPage: null, nextPage: null, message: after ? 'You are at the bottom of the page...' : '', exit: null,
    });
    mockApi({ 'GET /transactions': (call) => ({ body: page(call.query.get('after')) }) });
    renderAt('/transactions');
    await screen.findByText('0000000000000003');
    fireEvent.keyDown(window, { key: 'F8' });
    expect(await screen.findByText('You are at the bottom of the page...')).toBeInTheDocument();
    expect(screen.getByText('0000000000000003')).toBeInTheDocument();
    expect(screen.getByLabelText('Select row 1')).toBeEnabled();
  });
});

describe('user id edited after a fetch', () => {
  it('COUSR03: F5 deletes the id on screen, never with the version of the previously fetched user', async () => {
    signedIn('ADMIN');
    const { calls } = mockApi({
      'DELETE /users/USER0001': { body: userScreen('USER0001', 3) },
      'DELETE /users/USER0002': problem(404, 'NOTFND', 'User ID NOT found...', 'userId'),
    });
    renderAt('/admin/users/delete');
    const id = screen.getByLabelText('Enter User ID');
    await userEvent.type(id, 'USER0001{Enter}');
    expect(await screen.findByDisplayValue('Ann')).toBeInTheDocument();
    await userEvent.clear(id);
    await userEvent.type(id, 'USER0002');
    expect(screen.queryByDisplayValue('Ann')).not.toBeInTheDocument();
    fireEvent.keyDown(window, { key: 'F5' });
    await screen.findByText('User ID NOT found...');
    const del = calls.filter((c) => c.query.get('confirm') === 'Y');
    expect(del).toHaveLength(1);
    expect(del[0].path).toBe('/users/USER0002');
    expect(del[0].query.get('version')).toBeNull();
  });

  it('COUSR02: an edited id drops the fetched record, so F5 fetches instead of saving another user', async () => {
    signedIn('ADMIN');
    const { calls } = mockApi({
      'GET /users/USER0001': { body: userScreen('USER0001', 3) },
      'GET /users/USER0002': { body: userScreen('USER0002', 5) },
    });
    renderAt('/admin/users/update');
    const id = screen.getByLabelText('Enter User ID');
    await userEvent.type(id, 'USER0001{Enter}');
    expect(await screen.findByDisplayValue('Ann')).toBeInTheDocument();
    await userEvent.clear(id);
    await userEvent.type(id, 'USER0002');
    expect(screen.queryByDisplayValue('Ann')).not.toBeInTheDocument();
    fireEvent.keyDown(window, { key: 'F5' });
    await waitFor(() => expect(calls.map((c) => `${c.method} ${c.path}`)).toContain('GET /users/USER0002'));
    expect(calls.some((c) => c.method === 'PUT')).toBe(false);
  });
});

describe('cursor on errors without a field', () => {
  it('focuses the first enterable field (account number) when the API error has field=null', async () => {
    signedIn('USER');
    mockApi({ 'GET /accounts/99999999999': problem(404, 'NOTFND', 'Did not find this account in account master file', null) });
    renderAt('/accounts/view');
    const acct = screen.getByLabelText('Account Number');
    await userEvent.type(acct, '99999999999');
    await userEvent.click(screen.getByRole('button', { name: /ENTER/ }));
    expect(await screen.findByText('Did not find this account in account master file')).toBeInTheDocument();
    await waitFor(() => expect(document.activeElement).toBe(acct));
  });
});
