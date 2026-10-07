import { screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it } from 'vitest';
import { ctx, mockApi, problem, renderAt, signedIn } from './harness';

describe('ApiError.field focus and message area', () => {
  it('shows the API message and focuses the field it names', async () => {
    signedIn('USER', { context: ctx('COMEN01C', 'COBIL00C') });
    mockApi({ 'POST /accounts/%20/bill-payment': problem(400, 'INVALID', 'Acct ID can NOT be empty...', 'accountId') });
    renderAt('/bill-payment');
    await userEvent.click(screen.getByLabelText(/Please confirm/));
    await userEvent.keyboard('{Enter}');
    expect(await screen.findByTestId('message-line')).toHaveTextContent('Acct ID can NOT be empty...');
    await waitFor(() => expect(screen.getByLabelText('Enter Acct ID')).toHaveFocus());
    expect(screen.getByLabelText('Enter Acct ID')).toHaveAttribute('aria-invalid', 'true');
  });

  it('focuses a nested field part (report start month)', async () => {
    signedIn('USER');
    mockApi({ 'POST /reports/transactions': problem(400, 'INVALID', 'Start Date - Not a valid Month...', 'startDate.month') });
    renderAt('/reports');
    await userEvent.click(screen.getByLabelText('Custom (Date Range)'));
    await userEvent.type(screen.getByLabelText('Start Date month'), '13');
    await userEvent.keyboard('{Enter}');
    expect(await screen.findByText('Start Date - Not a valid Month...')).toBeInTheDocument();
    await waitFor(() => expect(screen.getByLabelText('Start Date month')).toHaveFocus());
  });

  it('401 SIGNON_REQUIRED drops the session and returns to sign-on', async () => {
    signedIn('USER');
    mockApi({
      'GET /menu/main': problem(401, 'SIGNON_REQUIRED', 'Please sign on to CardDemo'),
      'GET /auth/login': { body: { header: null, message: '', cursor: 'userId' } },
    });
    renderAt('/menu');
    await waitFor(() => expect(screen.getByTestId('location')).toHaveTextContent('/signon'));
    expect(await screen.findByText('Please sign on to CardDemo')).toBeInTheDocument();
    expect(sessionStorage.getItem('carddemo.session')).toBeNull();
  });

  it('BMS NUM attribute: numeric fields drop non-digits, lengths follow the map', async () => {
    signedIn('USER');
    mockApi({ 'GET /menu/main': { body: { header: null, options: [], optionLines: [], message: '' } } });
    renderAt('/menu');
    const option = screen.getByLabelText('Please select an option');
    await userEvent.type(option, 'a1b2c');
    expect(option).toHaveValue('12');
    expect(option).toHaveAttribute('maxlength', '2');
  });
});
