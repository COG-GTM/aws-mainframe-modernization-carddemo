import type { ReactElement } from 'react';
import { Navigate, Route, Routes } from 'react-router-dom';
import { AccountUpdatePage } from './pages/AccountUpdatePage';
import { AccountViewPage } from './pages/AccountViewPage';
import { BillPaymentPage } from './pages/BillPaymentPage';
import { CardDetailPage } from './pages/CardDetailPage';
import { CardListPage } from './pages/CardListPage';
import { CardUpdatePage } from './pages/CardUpdatePage';
import { MenuPage } from './pages/MenuPage';
import { NotAuthorizedPage } from './pages/NotAuthorizedPage';
import { ReportPage } from './pages/ReportPage';
import { SignonPage } from './pages/SignonPage';
import { TransactionAddPage } from './pages/TransactionAddPage';
import { TransactionListPage } from './pages/TransactionListPage';
import { TransactionViewPage } from './pages/TransactionViewPage';
import { UserAddPage } from './pages/UserAddPage';
import { UserDeletePage } from './pages/UserDeletePage';
import { UserListPage } from './pages/UserListPage';
import { UserUpdatePage } from './pages/UserUpdatePage';
import { menuProgram, page, routeFor } from './programs';
import { useSession } from './session/session';

const ELEMENTS: Record<string, ReactElement> = {
  COMEN01C: <MenuPage menu="main" />,
  COADM01C: <MenuPage menu="admin" />,
  COACTVWC: <AccountViewPage />,
  COACTUPC: <AccountUpdatePage />,
  COCRDLIC: <CardListPage />,
  COCRDSLC: <CardDetailPage />,
  COCRDUPC: <CardUpdatePage />,
  COTRN00C: <TransactionListPage />,
  COTRN01C: <TransactionViewPage />,
  COTRN02C: <TransactionAddPage />,
  COBIL00C: <BillPaymentPage />,
  CORPT00C: <ReportPage />,
  COUSR00C: <UserListPage />,
  COUSR01C: <UserAddPage />,
  COUSR02C: <UserUpdatePage />,
  COUSR03C: <UserDeletePage />,
};

/** No token -> sign-on; admin-only maps (COADM01, COUSR0x) are not rendered for a USER token. */
function Guard({ program }: { program: string }) {
  const { session } = useSession();
  if (!session) return <Navigate to="/signon" replace />;
  if (page(program).adminOnly && session.role !== 'ADMIN') return <NotAuthorizedPage />;
  return ELEMENTS[program];
}

function Home() {
  const { session } = useSession();
  return <Navigate to={session ? routeFor(menuProgram(session.role))! : '/signon'} replace />;
}

export function App() {
  return (
    <Routes>
      <Route path="/signon" element={<SignonPage />} />
      {Object.keys(ELEMENTS).map((program) => (
        <Route key={program} path={routeFor(program)} element={<Guard program={program} />} />
      ))}
      <Route path="*" element={<Home />} />
    </Routes>
  );
}
