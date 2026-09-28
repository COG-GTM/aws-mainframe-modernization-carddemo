import type { ReactElement } from 'react';
import { Navigate, Route, Routes } from 'react-router-dom';
import { RequireAuth } from './auth/RequireAuth';
import { homeRoute, useAuth } from './auth/context';
import { AccountUpdateScreen } from './screens/AccountUpdateScreen';
import { AccountViewScreen } from './screens/AccountViewScreen';
import { BillPaymentScreen } from './screens/BillPaymentScreen';
import { CardDetailScreen } from './screens/CardDetailScreen';
import { CardListScreen } from './screens/CardListScreen';
import { CardUpdateScreen } from './screens/CardUpdateScreen';
import { MenuScreen } from './screens/MenuScreen';
import { ReportsScreen } from './screens/ReportsScreen';
import { SignonScreen } from './screens/SignonScreen';
import { TransactionAddScreen } from './screens/TransactionAddScreen';
import { TransactionDetailScreen } from './screens/TransactionDetailScreen';
import { TransactionListScreen } from './screens/TransactionListScreen';
import { UserAddScreen } from './screens/UserAddScreen';
import { UserDeleteScreen } from './screens/UserDeleteScreen';
import { UserListScreen } from './screens/UserListScreen';
import { UserUpdateScreen } from './screens/UserUpdateScreen';

function Home() {
  const { session } = useAuth();
  return <Navigate to={session ? homeRoute(session.role) : '/login'} replace />;
}

const user = (el: ReactElement) => <RequireAuth>{el}</RequireAuth>;
const admin = (el: ReactElement) => <RequireAuth role="ADMIN">{el}</RequireAuth>;

export function App() {
  return (
    <Routes>
      <Route path="/login" element={<SignonScreen />} />
      <Route path="/menu" element={user(<MenuScreen kind="main" />)} />
      <Route path="/admin" element={admin(<MenuScreen kind="admin" />)} />
      <Route path="/accounts/view" element={user(<AccountViewScreen />)} />
      <Route path="/accounts/update" element={user(<AccountUpdateScreen />)} />
      <Route path="/cards" element={user(<CardListScreen />)} />
      <Route path="/cards/view" element={user(<CardDetailScreen />)} />
      <Route path="/cards/update" element={user(<CardUpdateScreen />)} />
      <Route path="/transactions" element={user(<TransactionListScreen />)} />
      <Route path="/transactions/view" element={user(<TransactionDetailScreen />)} />
      <Route path="/transactions/new" element={user(<TransactionAddScreen />)} />
      <Route path="/reports" element={user(<ReportsScreen />)} />
      <Route path="/bill-payment" element={user(<BillPaymentScreen />)} />
      <Route path="/admin/users" element={admin(<UserListScreen />)} />
      <Route path="/admin/users/new" element={admin(<UserAddScreen />)} />
      <Route path="/admin/users/edit" element={admin(<UserUpdateScreen />)} />
      <Route path="/admin/users/:userId/edit" element={admin(<UserUpdateScreen />)} />
      <Route path="/admin/users/delete" element={admin(<UserDeleteScreen />)} />
      <Route path="/admin/users/:userId/delete" element={admin(<UserDeleteScreen />)} />
      <Route path="*" element={<Home />} />
    </Routes>
  );
}
