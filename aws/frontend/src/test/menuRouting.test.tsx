import { screen } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { ADMIN, USER, renderApp, signOnAs } from './renderApp';

const option = () => screen.getByLabelText('Please select an option :');

describe('menu routing (COMEN01 / COADM01)', () => {
  it('redirects unauthenticated access to the signon screen', async () => {
    renderApp('/transactions');
    expect(await screen.findByText('COSGN00C')).toBeInTheDocument();
  });

  it('lists the main-menu options and navigates by option number', async () => {
    await signOnAs(USER);
    const { user } = renderApp('/menu');
    expect(await screen.findByText('Account View')).toBeInTheDocument();
    expect(screen.getByText('Transaction List')).toBeInTheDocument();
    await user.type(option(), '6');
    await user.keyboard('{Enter}');
    expect(await screen.findByRole('heading', { name: 'List Transactions' })).toBeInTheDocument();
  });

  it('rejects invalid option numbers with the COMEN01C message', async () => {
    await signOnAs(USER);
    const { user } = renderApp('/menu');
    await screen.findByText('Account View');
    await user.type(option(), '99{Enter}');
    expect(screen.getByTestId('message-line')).toHaveTextContent('Please enter a valid option number...');
  });

  it('flags optional sub-applications as not installed', async () => {
    await signOnAs(USER);
    const { user } = renderApp('/menu');
    await screen.findByText('Account View');
    await user.type(option(), '11{Enter}');
    expect(screen.getByTestId('message-line')).toHaveTextContent(/is not installed/);
  });

  it('keeps regular users out of admin routes (role from the JWT)', async () => {
    await signOnAs(USER);
    renderApp('/admin/users');
    expect(await screen.findByRole('heading', { name: 'Main Menu' })).toBeInTheDocument();
    expect(screen.getByTestId('message-line')).toHaveTextContent('No access - Admin Only option...');
  });

  it('lets admins reach user maintenance from the admin menu and F3 back', async () => {
    await signOnAs(ADMIN);
    const { user } = renderApp('/admin');
    expect(await screen.findByText('User List (Security)')).toBeInTheDocument();
    await user.type(option(), '1{Enter}');
    expect(await screen.findByRole('heading', { name: 'List Users' })).toBeInTheDocument();
    expect(await screen.findByText('ADMIN001')).toBeInTheDocument();
    await user.keyboard('{F3}');
    expect(await screen.findByRole('heading', { name: 'Admin Menu' })).toBeInTheDocument();
  });

  it('signs out on F3 from the main menu', async () => {
    await signOnAs(USER);
    const { user } = renderApp('/menu');
    await screen.findByText('Account View');
    await user.keyboard('{F3}');
    expect(await screen.findByText('COSGN00C')).toBeInTheDocument();
    expect(sessionStorage.getItem('carddemo.token')).toBeNull();
  });
});
