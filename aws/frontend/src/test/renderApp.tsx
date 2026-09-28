import { render } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { MemoryRouter } from 'react-router-dom';
import { App } from '../App';
import { AuthProvider } from '../auth/AuthProvider';
import { api } from '../api/endpoints';

export const ADMIN = { userId: 'ADMIN001', password: 'PASSWORD' };
export const USER = { userId: 'USER0001', password: 'PASSWORD' };

/** Signs on against the MSW mock and stores the JWT the way AuthProvider does. */
export async function signOnAs(creds: { userId: string; password: string }): Promise<void> {
  const { token } = await api.signon(creds);
  sessionStorage.setItem('carddemo.token', token);
}

export function renderApp(path = '/login', state?: unknown) {
  const user = userEvent.setup();
  const utils = render(
    <MemoryRouter initialEntries={[{ pathname: path, state }]}>
      <AuthProvider>
        <App />
      </AuthProvider>
    </MemoryRouter>,
  );
  return { user, ...utils };
}
