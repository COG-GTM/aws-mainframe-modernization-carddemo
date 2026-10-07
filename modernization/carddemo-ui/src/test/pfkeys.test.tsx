import { fireEvent, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it } from 'vitest';
import { ctx, HEADER, mockApi, renderAt, signedIn } from './harness';

const card = (version: number, name = 'WARD JONES') => ({
  header: { ...HEADER, tranId: 'CCUP', programName: 'COCRDUPC' },
  infoMessage: 'Details of selected card shown above',
  message: '',
  accountId: '00000000027',
  cardNumber: '0683586198171516',
  cardRef: 'REF27',
  embossedName: name,
  expiryMonth: '07',
  expiryYear: '2025',
  activeStatus: 'Y',
  version,
  updateForm: { accountId: '00000000027', version, confirm: false, embossedName: name, activeStatus: 'Y', expiryMonth: '07', expiryYear: '2025' },
  exit: ctx('COCRDUPC', 'COCRDLIC'),
});

describe('PF keys', () => {
  it('F3 (key or button) returns to the calling program from the NavigationContext', async () => {
    signedIn('USER', { context: ctx('COMEN01C', 'COACTVWC') });
    mockApi({ 'GET /menu/main': { body: { header: HEADER, options: [], optionLines: [], message: '' } } });
    renderAt('/accounts/view');
    fireEvent.keyDown(window, { key: 'F3' });
    await waitFor(() => expect(screen.getByTestId('location')).toHaveTextContent('/menu'));
  });

  it('F3 falls back to the role menu when there is no caller', async () => {
    signedIn('ADMIN');
    mockApi({
      'GET /users': { body: { header: HEADER, pageNumber: 1, pageSize: 10, rows: [], hasPreviousPage: false, hasNextPage: false, message: '', exit: null } },
      'GET /menu/admin': { body: { header: HEADER, options: [], optionLines: [], message: '' } },
    });
    renderAt('/admin/users');
    await userEvent.click(await screen.findByRole('button', { name: /F3 Back/ }));
    await waitFor(() => expect(screen.getByTestId('location')).toHaveTextContent(/^\/admin$/));
  });

  it('card update: ENTER validates (confirm=false), F5 appears only once VALIDATED and commits (confirm=true)', async () => {
    signedIn('USER', { context: ctx('COCRDLIC', 'COCRDUPC', { acctId: 27 }), cardRef: 'REF27' });
    const { calls } = mockApi({
      'GET /cards/REF27': { body: card(0) },
      'PUT /cards/REF27': (call) => {
        const body = call.body as { confirm: boolean };
        return body.confirm
          ? { body: { header: HEADER, state: 'COMMITTED', updated: true, infoMessage: 'Changes committed to database', message: '', card: card(1, 'WARD B JONES') } }
          : { body: { header: HEADER, state: 'VALIDATED', updated: false, infoMessage: 'Changes validated.Press F5 to save', message: '', card: card(0) } };
      },
    });
    renderAt('/cards/update');
    const name = await screen.findByDisplayValue('WARD JONES');
    expect(screen.queryByRole('button', { name: /F5/ })).not.toBeInTheDocument();
    await userEvent.clear(name);
    await userEvent.type(name, 'Ward B Jones');
    await userEvent.keyboard('{Enter}');
    expect(await screen.findByText('Changes validated.Press F5 to save')).toBeInTheDocument();
    expect(calls.filter((c) => c.method === 'PUT')[0].body).toMatchObject({ confirm: false, version: 0, embossedName: 'Ward B Jones' });
    fireEvent.keyDown(window, { key: 'F5' });
    expect(await screen.findByText('Changes committed to database')).toBeInTheDocument();
    expect(calls.filter((c) => c.method === 'PUT')[1].body).toMatchObject({ confirm: true, version: 0 });
    expect(screen.queryByRole('button', { name: /F5/ })).not.toBeInTheDocument();
  });

  it('transaction add: F5 sends copyLast, F4 clears the form', async () => {
    signedIn('USER', { context: ctx('COMEN01C', 'COTRN02C') });
    const { calls } = mockApi({
      'POST /transactions': {
        body: {
          header: HEADER,
          state: 'VALIDATED',
          form: { accountId: '00000000010', cardNumber: '', typeCode: '01', categoryCode: '0001', source: 'POS TERM', description: 'Copied', amount: '+00000010.00', origDate: '2022-07-06', procDate: '2022-07-06', merchantId: '800000000', merchantName: 'M', merchantCity: 'C', merchantZip: '1', confirm: '' },
          transaction: null,
          message: 'Confirm to add this transaction...',
          exit: ctx('COTRN02C', 'COMEN01C'),
        },
      },
    });
    renderAt('/transactions/add');
    await userEvent.type(screen.getByLabelText('Enter Acct #'), '00000000010');
    fireEvent.keyDown(window, { key: 'F5' });
    expect(await screen.findByDisplayValue('Copied')).toBeInTheDocument();
    expect(calls[0].body).toMatchObject({ accountId: '00000000010', copyLast: true });
    await userEvent.click(screen.getByRole('button', { name: /F4 Clear/ }));
    expect(screen.queryByDisplayValue('Copied')).not.toBeInTheDocument();
    expect(screen.getByLabelText('Enter Acct #')).toHaveValue('');
  });

  it('F7/F8 page the transaction list with the keys of the first / last row', async () => {
    signedIn('USER');
    const rows = (start: number) =>
      Array.from({ length: 10 }, (_, i) => ({ row: i + 1, tranId: String(start + i).padStart(16, '0'), date: '07/06/22', description: 'x', amount: '+1.00' }));
    const { calls } = mockApi({
      'GET /transactions': (call) => ({
        body: {
          header: HEADER,
          pageNumber: call.query.get('after') ? 2 : 1,
          pageSize: 10,
          rows: rows(call.query.get('after') ? 11 : 1),
          hasPreviousPage: !!call.query.get('after'),
          hasNextPage: true,
          previousPage: null,
          nextPage: null,
          message: '',
          exit: null,
        },
      }),
    });
    renderAt('/transactions');
    await screen.findByText('0000000000000010');
    fireEvent.keyDown(window, { key: 'F8' });
    await screen.findByText('0000000000000020');
    expect(calls[1].query.get('after')).toBe('0000000000000010');
    expect(calls[1].query.get('page')).toBe('1');
    fireEvent.keyDown(window, { key: 'F7' });
    await waitFor(() => expect(calls).toHaveLength(3));
    expect(calls[2].query.get('before')).toBe('0000000000000011');
  });
});
