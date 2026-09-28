import { screen, within } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { db } from '../mocks/db';
import { USER, renderApp, signOnAs } from './renderApp';

const ids = () =>
  within(screen.getByRole('table', { name: 'Transactions' }))
    .getAllByRole('row')
    .slice(1)
    .map((r) => within(r).getAllByRole('cell')[1].textContent)
    .filter(Boolean);

const sorted = () => db.transactions.map((t) => t.tranId).sort();

describe('COTRN00 transaction list paging', () => {
  it('shows 10 rows per page and pages with F8/F7', async () => {
    await signOnAs(USER);
    const { user } = renderApp('/transactions');
    const all = sorted();
    await screen.findByText(all[0]);
    expect(ids()).toEqual(all.slice(0, 10));
    expect(screen.getByTestId('page-no')).toHaveTextContent('Page: 1');

    await user.keyboard('{F8}');
    await screen.findByText(all[10]);
    expect(ids()).toEqual(all.slice(10, 20));
    expect(screen.getByTestId('page-no')).toHaveTextContent('Page: 2');

    await user.click(screen.getByRole('button', { name: /F8/ }));
    await screen.findByText(all[20]);
    expect(ids()).toEqual(all.slice(20, 30));

    await user.keyboard('{F7}');
    await screen.findByText(all[10]);
    expect(ids()).toEqual(all.slice(10, 20));
    expect(screen.getByTestId('page-no')).toHaveTextContent('Page: 2');
  });

  it('shows the legacy top-of-page message on F7 from the first page', async () => {
    await signOnAs(USER);
    const { user } = renderApp('/transactions');
    await screen.findByText(sorted()[0]);
    await user.keyboard('{F7}');
    expect(screen.getByTestId('message-line')).toHaveTextContent('You are already at the top of the page...');
  });

  it('stops at the bottom with the legacy message', async () => {
    db.transactions = db.transactions.slice(0, 15);
    await signOnAs(USER);
    const { user } = renderApp('/transactions');
    const all = sorted();
    await screen.findByText(all[0]);
    await user.keyboard('{F8}');
    await screen.findByText(all[14]);
    expect(ids()).toEqual(all.slice(10, 15));
    await user.keyboard('{F8}');
    expect(screen.getByTestId('message-line')).toHaveTextContent('You are already at the bottom of the page...');
  });

  it('positions the browse at the searched Tran ID and validates it is numeric', async () => {
    await signOnAs(USER);
    const { user } = renderApp('/transactions');
    const all = sorted();
    await screen.findByText(all[0]);
    await user.type(screen.getByLabelText('Search Tran ID:'), 'ABC{Enter}');
    expect(screen.getByTestId('message-line')).toHaveTextContent('Tran ID must be Numeric ...');

    await user.clear(screen.getByLabelText('Search Tran ID:'));
    await user.type(screen.getByLabelText('Search Tran ID:'), `${all[42]}{Enter}`);
    await screen.findByText(all[42]);
    expect(ids()[0]).toBe(all[42]);
  });

  it('opens the detail screen for a row selected with S', async () => {
    await signOnAs(USER);
    const { user } = renderApp('/transactions');
    const first = sorted()[0];
    await screen.findByText(first);
    await user.type(screen.getByLabelText(`Select transaction ${first}`), 's{Enter}');
    expect(await screen.findByRole('heading', { name: 'View Transaction' })).toBeInTheDocument();
    expect(await screen.findAllByText(first)).not.toHaveLength(0);
  });
});
