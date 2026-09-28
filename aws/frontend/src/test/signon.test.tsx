import { screen } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { renderApp } from './renderApp';

const message = () => screen.getByTestId('message-line');

describe('COSGN00 signon', () => {
  it('shows the 3270 header for CC00 / COSGN00C', () => {
    renderApp('/login');
    expect(screen.getByText('CC00')).toBeInTheDocument();
    expect(screen.getByText('COSGN00C')).toBeInTheDocument();
    expect(screen.getByText('CardDemo')).toBeInTheDocument();
  });

  it('requires the user id, then the password (legacy order and messages)', async () => {
    const { user } = renderApp('/login');
    await user.keyboard('{Enter}');
    expect(message()).toHaveTextContent('Please enter User ID ...');
    expect(screen.getByLabelText('User ID')).toHaveAttribute('aria-invalid', 'true');

    await user.type(screen.getByLabelText('User ID'), 'user0001');
    await user.keyboard('{Enter}');
    expect(message()).toHaveTextContent('Please enter Password ...');
  });

  it('reports an unknown user and a wrong password with the COSGN00C messages', async () => {
    const { user } = renderApp('/login');
    await user.type(screen.getByLabelText('User ID'), 'NOBODY');
    await user.type(screen.getByLabelText('Password'), 'PASSWORD{Enter}');
    expect(await screen.findByText('User not found. Try again ...')).toBeInTheDocument();

    await user.clear(screen.getByLabelText('User ID'));
    await user.type(screen.getByLabelText('User ID'), 'USER0001');
    await user.clear(screen.getByLabelText('Password'));
    await user.type(screen.getByLabelText('Password'), 'WRONG{Enter}');
    expect(await screen.findByText('Wrong Password. Try again ...')).toBeInTheDocument();
  });

  it('routes a regular user to the main menu (upper-casing credentials like the BMS map)', async () => {
    const { user } = renderApp('/login');
    await user.type(screen.getByLabelText('User ID'), 'user0001');
    await user.type(screen.getByLabelText('Password'), 'password');
    await user.click(screen.getByRole('button', { name: /ENTER/ }));
    expect(await screen.findByRole('heading', { name: 'Main Menu' })).toBeInTheDocument();
    expect(screen.getByText('CM00')).toBeInTheDocument();
  });

  it('routes an admin to the admin menu', async () => {
    const { user } = renderApp('/login');
    await user.type(screen.getByLabelText('User ID'), 'ADMIN001');
    await user.type(screen.getByLabelText('Password'), 'PASSWORD{Enter}');
    expect(await screen.findByRole('heading', { name: 'Admin Menu' })).toBeInTheDocument();
    expect(screen.getByText('CA00')).toBeInTheDocument();
  });
});
