import { screen, waitFor } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { db } from '../mocks/db';
import { NO_CHANGE, validateAccountForm, type AccountForm } from '../validation/account';
import { USER, renderApp, signOnAs } from './renderApp';

const VALID: AccountForm = {
  acctId: '00000000003',
  activeStatus: 'Y',
  openYear: '2014',
  openMon: '11',
  openDay: '20',
  creditLimit: '2020.00',
  expYear: '2025',
  expMon: '05',
  expDay: '20',
  cashCreditLimit: '1020.00',
  risYear: '2025',
  risMon: '05',
  risDay: '20',
  currBal: '194.00',
  currCycCredit: '0.00',
  currCycDebit: '0.00',
  groupId: 'A000000000',
  custId: '000000003',
  ssn1: '123',
  ssn2: '45',
  ssn3: '6789',
  dobYear: '1961',
  dobMon: '06',
  dobDay: '08',
  fico: '700',
  firstName: 'Immanuel',
  middleName: 'Madeline',
  lastName: 'Kessler',
  addrLine1: '618 Deshaun Route',
  addrStateCd: 'NC',
  addrLine2: 'Apt. 802',
  addrZip: '27601',
  addrLine3: 'Raleigh',
  addrCountryCd: 'USA',
  ph1a: '919',
  ph1b: '119',
  ph1c: '8310',
  ph2a: '',
  ph2b: '',
  ph2c: '',
  govtIssuedId: '00000000000049368437',
  eftAccountId: '0053581756',
  priCardHolderInd: 'Y',
};

const edit = (patch: Partial<AccountForm>) => validateAccountForm({ ...VALID, ...patch }, VALID);

describe('COACTUPC field edits (validateAccountForm)', () => {
  it('accepts a valid changed image', () => {
    expect(edit({ creditLimit: '2500.00' })).toBeNull();
  });

  it('reports no change against the fetched image', () => {
    expect(validateAccountForm(VALID, VALID)?.message).toBe(NO_CHANGE);
  });

  it.each<[Partial<AccountForm>, keyof AccountForm, string]>([
    [{ activeStatus: 'X' }, 'activeStatus', 'Account Status must be Y or N.'],
    [{ activeStatus: '' }, 'activeStatus', 'Account Status must be supplied.'],
    [{ creditLimit: 'abc' }, 'creditLimit', 'Credit Limit is not valid'],
    [{ creditLimit: '' }, 'creditLimit', 'Credit Limit must be supplied.'],
    [{ openMon: '13' }, 'openMon', 'Open Date: Month must be a number between 1 and 12.'],
    [{ expYear: '1850' }, 'expYear', 'Expiry Date : Century is not valid.'],
    [{ risYear: '2023', risMon: '02', risDay: '29' }, 'risDay', 'Reissue Date:Not a leap year.Cannot have 29 days in this month.'],
    [{ openMon: '04', openDay: '31' }, 'openDay', 'Open Date:Cannot have 31 days in this month.'],
    [{ ssn1: '666' }, 'ssn1', 'SSN: First 3 chars: should not be 000, 666, or between 900 and 999'],
    [{ ssn3: '12a4' }, 'ssn3', 'SSN Last 4 chars must be all numeric.'],
    [{ dobYear: '2099' }, 'dobYear', 'Date of Birth:cannot be in the future '],
    [{ fico: '900' }, 'fico', 'FICO Score: should be between 300 and 850'],
    [{ firstName: 'J0hn' }, 'firstName', 'First Name can have alphabets only.'],
    [{ addrStateCd: 'XX' }, 'addrStateCd', 'State: is not a valid state code'],
    [{ ph1a: '000' }, 'ph1a', 'Phone Number 1: Area code cannot be zero'],
    [{ ph1a: '123' }, 'ph1a', 'Phone Number 1: Not valid North America general purpose area code'],
    [{ ph2a: '212' }, 'ph2b', 'Phone Number 2: Prefix code must be supplied.'],
    [{ priCardHolderInd: 'Q' }, 'priCardHolderInd', 'Primary Card Holder must be Y or N.'],
    [{ addrZip: '90210' }, 'addrZip', 'Invalid zip code for state'],
  ])('%o -> %s', (patch, field, message) => {
    expect(edit(patch)).toEqual({ field, message });
  });

  it('reports only the first failing edit, in legacy order', () => {
    expect(edit({ activeStatus: 'X', creditLimit: 'abc', fico: '1' })).toEqual({
      field: 'activeStatus',
      message: 'Account Status must be Y or N.',
    });
  });
});

describe('COACTUP account update screen', () => {
  it('validates the account number before the lookup', async () => {
    await signOnAs(USER);
    const { user } = renderApp('/accounts/update');
    await user.keyboard('{Enter}');
    expect(screen.getByTestId('message-line')).toHaveTextContent('Account number not provided');
    await user.type(screen.getByLabelText('Account Number :'), '0{Enter}');
    expect(screen.getByTestId('message-line')).toHaveTextContent('Account number must be a non zero 11 digit number');
    await user.clear(screen.getByLabelText('Account Number :'));
    await user.type(screen.getByLabelText('Account Number :'), '99999999999{Enter}');
    expect(await screen.findByText('Did not find this account in account card xref file')).toBeInTheDocument();
  });

  it('flags the first invalid field, then saves a valid change with F5', async () => {
    // Sample customer data is synthetic and fails some COACTUPC edits (e.g. ZIP+4); give it a clean image.
    Object.assign(db.customers.find((c) => c.custId === 3)!, {
      addrStateCd: 'GA',
      addrZip: '30301',
      addrLine3: 'Atlanta',
      phoneNum1: '(404)396-9024',
      phoneNum2: '(678)168-8826',
    });
    await signOnAs(USER);
    const { user } = renderApp('/accounts/update');
    await user.type(screen.getByLabelText('Account Number :'), '3{Enter}');
    const status = await screen.findByLabelText('Active Y/N:');
    await waitFor(() => expect(status).toHaveValue('Y'));

    await user.keyboard('{Enter}');
    expect(screen.getByTestId('message-line')).toHaveTextContent(NO_CHANGE);

    await user.clear(status);
    await user.type(status, 'X{Enter}');
    expect(screen.getByTestId('message-line')).toHaveTextContent('Account Status must be Y or N.');
    expect(status).toHaveAttribute('aria-invalid', 'true');

    await user.clear(status);
    await user.type(status, 'N');
    const limit = screen.getByLabelText('Credit Limit :');
    await user.clear(limit);
    await user.type(limit, '12x{Enter}');
    expect(screen.getByTestId('message-line')).toHaveTextContent('Credit Limit is not valid');

    await user.clear(limit);
    await user.type(limit, '5000.00');
    await user.keyboard('{F5}');
    expect(await screen.findByText('Changes committed to database')).toBeInTheDocument();
    const acct = db.accounts.find((a) => a.acctId === 3)!;
    expect(acct.activeStatus).toBe('N');
    expect(acct.creditLimit).toBe('5000.00');
    expect(acct.version).toBe(1);
  });
});
