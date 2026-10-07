import { screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it } from 'vitest';
import { PROGRAMS, routeFor } from '../programs';
import { ctx, HEADER, mockApi, renderAt, signedIn } from './harness';

const menuScreen = {
  header: { ...HEADER, tranId: 'CM00', programName: 'COMEN01C' },
  menu: 'main',
  programId: 'COMEN01C',
  tranId: 'CM00',
  mapset: 'COMEN01',
  map: 'COMEN1A',
  options: [
    { number: 1, label: '01. Account View', name: 'Account View', programId: 'COACTVWC', adminOnly: false },
    { number: 5, label: '05. Credit Card Update', name: 'Credit Card Update', programId: 'COCRDUPC', adminOnly: false },
  ],
  optionLines: [],
  message: '',
};

describe('routing by NavigationContext.toProgram', () => {
  it('has one route per BMS map (17)', () => {
    expect(PROGRAMS).toHaveLength(17);
    expect(new Set(PROGRAMS.map((p) => p.route)).size).toBe(17);
    expect(new Set(PROGRAMS.map((p) => p.mapset)).size).toBe(17);
  });

  it('routes the menu selection to the page of the returned toProgram, not the option number', async () => {
    signedIn('USER');
    const { calls } = mockApi({
      'GET /menu/main': { body: menuScreen },
      // option 1 answered with COCRDUPC: the page must follow the API, not the option number
      'POST /menu/main/selection': {
        body: { option: '01', navigation: ctx('COMEN01C', 'COCRDUPC', { toTranId: 'CCUP' }), message: '', messageColor: 'DEFAULT' },
      },
    });
    renderAt('/menu');
    await userEvent.click(await screen.findByRole('button', { name: '01. Account View' }));
    await waitFor(() => expect(screen.getByTestId('location')).toHaveTextContent(routeFor('COCRDUPC')!));
    expect(screen.getByTestId('program')).toHaveTextContent('COCRDUPC');
    expect(calls.find((c) => c.path === '/menu/main/selection')?.body).toEqual({ option: '1' });
    expect(calls.every((c) => c.auth === 'Bearer test-token-USER')).toBe(true);
  });

  it('routes the sign-on response to the menu the API names and keeps the context in sessionStorage', async () => {
    mockApi({
      'GET /auth/login': { body: { header: HEADER, message: '', cursor: 'userId' } },
      'POST /auth/login': {
        body: {
          token: 'jwt',
          tokenType: 'Bearer',
          expiresAt: new Date(Date.now() + 3_600_000).toISOString(),
          userId: 'ADMIN001',
          role: 'ADMIN',
          userType: 'A',
          targetMenu: 'COADM01C',
          targetMenuUrl: '/api/v1/menu/admin',
          navigation: ctx('COSGN00C', 'COADM01C'),
        },
      },
      'GET /menu/admin': { body: { ...menuScreen, menu: 'admin', options: [] } },
    });
    renderAt('/signon');
    await userEvent.type(screen.getByLabelText('User ID'), 'admin001');
    await userEvent.type(screen.getByLabelText('Password'), 'PASSWORD');
    await userEvent.keyboard('{Enter}');
    await waitFor(() => expect(screen.getByTestId('location')).toHaveTextContent('/admin'));
    expect(JSON.parse(sessionStorage.getItem('carddemo.nav')!).context.toProgram).toBe('COADM01C');
    expect(JSON.parse(sessionStorage.getItem('carddemo.session')!).userId).toBe('ADMIN001');
  });

  it('hands the selected card to the detail page as its opaque cardRef', async () => {
    signedIn('USER', { context: ctx('COMEN01C', 'COCRDLIC', { acctId: 10 }) });
    const { calls } = mockApi({
      'GET /cards': {
        body: {
          header: HEADER,
          accountId: '00000000010',
          cardNumber: null,
          pageSize: 7,
          rows: [{ row: 1, accountId: '00000000010', cardNumber: '************1234', activeStatus: 'Y', cardRef: 'REF1' }],
          hasPreviousPage: false,
          hasNextPage: false,
          previousPage: null,
          nextPage: null,
          infoMessage: 'TYPE S FOR DETAIL, U TO UPDATE ANY RECORD',
          message: '',
          exit: ctx('COCRDLIC', 'COMEN01C'),
        },
      },
      'POST /cards/selection': {
        body: { header: HEADER, navigation: ctx('COCRDLIC', 'COCRDSLC', { acctId: 10 }), cardRef: 'REF1', next: '', infoMessage: '' },
      },
      'GET /cards/REF1': {
        body: {
          header: HEADER,
          infoMessage: '   Displaying requested details',
          message: '',
          accountId: '00000000010',
          cardNumber: '0500024453765740',
          cardRef: 'REF1',
          embossedName: 'X',
          expiryMonth: '03',
          expiryYear: '2025',
          activeStatus: 'Y',
          version: 0,
          updateForm: null,
          exit: ctx('COCRDSLC', 'COCRDLIC'),
        },
      },
    });
    renderAt('/cards');
    expect(await screen.findByText('************1234')).toBeInTheDocument();
    expect(calls[0].query.get('accountId')).toBe('00000000010');
    await userEvent.type(screen.getByLabelText('Select row 1'), 's');
    await userEvent.keyboard('{Enter}');
    await waitFor(() => expect(screen.getByTestId('program')).toHaveTextContent('COCRDSLC'));
    await waitFor(() => expect(calls.some((c) => c.path === '/cards/REF1' && c.query.get('accountId') === '00000000010')).toBe(true));
  });
});
