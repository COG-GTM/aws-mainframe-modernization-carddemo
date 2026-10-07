import { expect, test, type Page } from '@playwright/test';

// Sample data: USRSEC plaintext passwords (scripts/online/*.sh), account 00000000010 with its card.
const USER = { id: 'USER0001', password: 'PASSWORD' };
const ADMIN = { id: 'ADMIN001', password: 'PASSWORD' };
const ACCOUNT = '00000000010';

const letters = (n: number) => Array.from({ length: n }, () => String.fromCharCode(65 + Math.floor(Math.random() * 26))).join('');

async function signOn(page: Page, who: { id: string; password: string }) {
  await page.goto('/signon');
  await page.getByLabel('User ID').fill(who.id);
  await page.getByLabel('Password').fill(who.password);
  await page.keyboard.press('Enter');
}

async function pf(page: Page, key: string) {
  await page.getByRole('button', { name: new RegExp(`^${key} `) }).click();
}

const program = (page: Page) => page.getByTestId('program');
const message = (page: Page) => page.getByTestId('message-line');

async function menuOption(page: Page, label: string) {
  await page.getByRole('button', { name: label }).click();
}

test('USER0001: every main-menu transaction, then sign-off', async ({ page }) => {
  await signOn(page, USER);
  await expect(program(page)).toHaveText('COMEN01C');

  // COACTVW
  await menuOption(page, '01. Account View');
  await expect(program(page)).toHaveText('COACTVWC');
  await page.getByLabel('Account Number').fill(ACCOUNT);
  await page.keyboard.press('Enter');
  await expect(page.locator('#custId')).not.toHaveValue('');
  await pf(page, 'F3');
  await expect(program(page)).toHaveText('COMEN01C');

  // COACTUP: ENTER validates, F5 commits
  await menuOption(page, '02. Account Update');
  await page.getByLabel('Account Number').fill(ACCOUNT);
  await page.keyboard.press('Enter');
  await expect(page.locator('#firstName')).not.toHaveValue('');
  await page.locator('#middleName').fill(`E${letters(6)}`);
  // the sample customer's zip does not match its state (COACTUPC 1270-EDIT-US-STATE-ZIP-CD rejects it)
  await page.locator('#state').fill('NC');
  await page.locator('#zip').fill('27601');
  await pf(page, 'ENTER');
  await expect(page.getByTestId('info-line')).toContainText('F5');
  await pf(page, 'F5');
  await expect(page.getByTestId('info-line')).toContainText(/committed/i);
  await pf(page, 'F3');

  // COCRDLI -> S -> COCRDSL
  await menuOption(page, '03. Credit Card List');
  await page.getByLabel('Account Number').fill(ACCOUNT);
  await pf(page, 'ENTER');
  await expect(page.getByTestId('card-row-1')).toContainText(ACCOUNT);
  await expect(page.getByTestId('card-row-1')).toContainText('*');
  await page.getByLabel('Select row 1').fill('S');
  await pf(page, 'ENTER');
  await expect(program(page)).toHaveText('COCRDSLC');
  await expect(page.locator('#embossedName')).not.toHaveValue('');
  await pf(page, 'F3');
  await expect(program(page)).toHaveText('COCRDLIC');

  // COCRDLI -> U -> COCRDUP: ENTER validates, F5 saves
  await page.getByLabel('Select row 1').fill('U');
  await pf(page, 'ENTER');
  await expect(program(page)).toHaveText('COCRDUPC');
  await expect(page.locator('#embossedName')).not.toHaveValue('');
  await page.locator('#embossedName').fill(`E TEST ${letters(5)}`);
  await pf(page, 'ENTER');
  await expect(page.getByTestId('info-line')).toContainText('F5');
  await pf(page, 'F5');
  await expect(page.getByTestId('info-line')).toContainText(/committed/i);
  await pf(page, 'F3');
  await pf(page, 'F3');
  await expect(program(page)).toHaveText('COMEN01C');

  // COTRN02: F5 copy-last is not needed on an empty account; validate then confirm Y
  await menuOption(page, '08. Transaction Add');
  await page.getByLabel('Enter Acct #').fill(ACCOUNT);
  await page.getByLabel('Type CD').fill('01');
  await page.getByLabel('Category CD').fill('0001');
  await page.getByLabel('Source').fill('POS TERM');
  await page.getByLabel('Description').fill('E2E purchase');
  await page.getByLabel('Amount').fill('-00000012.34');
  await page.getByLabel('Orig Date').fill('2022-07-06');
  await page.getByLabel('Proc Date').fill('2022-07-06');
  await page.getByLabel('Merchant ID').fill('000000001');
  await page.getByLabel('Merchant Name').fill('Corner Store');
  await page.getByLabel('Merchant City').fill('Seattle');
  await page.getByLabel('Merchant Zip').fill('98101');
  await pf(page, 'ENTER');
  await expect(message(page)).toContainText('Confirm to add this transaction');
  await page.getByLabel(/Please confirm/).fill('Y');
  await pf(page, 'ENTER');
  await expect(message(page)).toContainText('Transaction added successfully');
  const tranId = (await message(page).textContent())!.match(/(\d{16})/)![1];
  await page.getByLabel('Enter Acct #').fill(ACCOUNT);
  await pf(page, 'F5');
  await expect(page.getByLabel('Description')).toHaveValue('E2E purchase');
  await pf(page, 'F3');

  // COTRN00 -> S -> COTRN01
  await menuOption(page, '06. Transaction List');
  await expect(program(page)).toHaveText('COTRN00C');
  await page.getByLabel('Search Tran ID').fill(tranId);
  await pf(page, 'ENTER');
  await expect(page.getByTestId('tran-row-1')).toContainText(tranId);
  await page.getByLabel('Select row 1').fill('S');
  await pf(page, 'ENTER');
  await expect(program(page)).toHaveText('COTRN01C');
  await expect(page.locator('#transaction\\.tranId')).toHaveValue(tranId);
  await pf(page, 'F3');
  await expect(program(page)).toHaveText('COTRN00C');
  await pf(page, 'F3');

  // COBIL00: ENTER shows balance, Y pays (a second run finds nothing to pay)
  await menuOption(page, '10. Bill Payment');
  await page.getByLabel('Enter Acct ID').fill(ACCOUNT);
  await pf(page, 'ENTER');
  await expect(message(page)).toContainText(/Confirm to make a bill payment|You have nothing to pay/);
  if ((await message(page).textContent())?.includes('Confirm')) {
    await page.getByLabel(/Please confirm/).fill('Y');
    await pf(page, 'ENTER');
    await expect(message(page)).toContainText('Payment successful');
  }
  await pf(page, 'F3');

  // CORPT00: submit, poll, download
  await menuOption(page, '09. Transaction Reports');
  await page.getByLabel('Custom (Date Range)').check();
  await page.getByLabel('Start Date month').fill('01');
  await page.getByLabel('Start Date day').fill('01');
  await page.getByLabel('Start Date year').fill('2022');
  await page.getByLabel('End Date month').fill('07');
  await page.getByLabel('End Date day').fill('06');
  await page.getByLabel('End Date year').fill('2022');
  await page.getByLabel(/Please confirm/).fill('Y');
  await pf(page, 'ENTER');
  await expect(message(page)).toContainText('Custom report submitted for printing');
  await expect(page.getByTestId('report-status')).toHaveText('COMPLETED', { timeout: 60_000 });
  const download = page.waitForEvent('download');
  await page.getByRole('button', { name: 'Download TRANREPT' }).click();
  expect((await download).suggestedFilename()).toMatch(/^TRANREPT-\d+\.txt$/);
  await pf(page, 'F3');

  // USER is denied the admin pages
  await page.goto('/admin/users');
  await expect(message(page)).toHaveText('No access - Admin Only option...');
  await page.goto('/menu');

  // sign-off
  await pf(page, 'F3');
  await expect(page).toHaveURL(/\/signon$/);
  await expect(message(page)).toContainText('Thank you');
});

test('ADMIN001: user list, add, update, delete, F3 back', async ({ page }) => {
  const userId = `E2E${letters(5)}`;
  await signOn(page, ADMIN);
  await expect(program(page)).toHaveText('COADM01C');

  // COUSR01
  await menuOption(page, '02. User Add (Security)');
  await expect(program(page)).toHaveText('COUSR01C');
  await page.getByLabel('First Name').fill('Eve');
  await page.getByLabel('Last Name').fill('Tester');
  await page.getByLabel('User ID').fill(userId);
  await page.getByLabel('Password').fill('E2EPASS');
  await page.getByLabel('User Type').fill('U');
  await pf(page, 'ENTER');
  await expect(message(page)).toContainText(`User ${userId} has been added`);
  await pf(page, 'F3');
  await expect(program(page)).toHaveText('COADM01C');

  // COUSR00 -> U -> COUSR02
  await menuOption(page, '01. User List (Security)');
  await expect(program(page)).toHaveText('COUSR00C');
  await page.getByLabel('Search User ID').fill(userId);
  await pf(page, 'ENTER');
  await expect(page.getByTestId('user-row-1')).toContainText(userId);
  await page.getByLabel(`Select ${userId}`).fill('U');
  await pf(page, 'ENTER');
  await expect(program(page)).toHaveText('COUSR02C');
  await expect(page.getByLabel('First Name')).toHaveValue('Eve');
  await page.getByLabel('Last Name').fill('Updated');
  await pf(page, 'F5');
  await expect(message(page)).toContainText(`User ${userId} has been updated`);
  await pf(page, 'F3');
  await expect(program(page)).toHaveText('COUSR00C');

  // COUSR00 -> D -> COUSR03
  await page.getByLabel('Search User ID').fill(userId);
  await pf(page, 'ENTER');
  await expect(page.getByTestId('user-row-1')).toContainText(userId);
  await page.getByLabel(`Select ${userId}`).fill('D');
  await pf(page, 'ENTER');
  await expect(program(page)).toHaveText('COUSR03C');
  await expect(page.getByLabel('Last Name')).toHaveValue('Updated');
  await expect(message(page)).toContainText('Press PF5 key to delete this user');
  await pf(page, 'F5');
  await expect(message(page)).toContainText(`User ${userId} has been deleted`);
  await pf(page, 'F3');
  await expect(program(page)).toHaveText('COUSR00C');
  await pf(page, 'F3');
  await expect(program(page)).toHaveText('COADM01C');
  await pf(page, 'F3');
  await expect(page).toHaveURL(/\/signon$/);
});
