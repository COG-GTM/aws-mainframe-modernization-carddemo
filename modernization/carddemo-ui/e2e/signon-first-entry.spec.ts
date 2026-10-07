import { expect, test, type Page } from '@playwright/test';

// s6.4 item 1.7: the s6.3 recording showed one rejected first password entry. Every way of entering the
// credentials on a fresh page must sign on at the first attempt (docs/validation/hardening/signon-first-entry.md).
const program = (page: Page) => page.getByTestId('program');
const message = (page: Page) => page.getByTestId('message-line');

const entries: Record<string, (page: Page) => Promise<void>> = {
  'fill + Enter before the screen header loads': async (page) => {
    await page.getByLabel('User ID').fill('USER0001');
    await page.getByLabel('Password').fill('PASSWORD');
    await page.keyboard.press('Enter');
  },
  'typed at keyboard speed, lower case, Tab between fields': async (page) => {
    await page.getByLabel('User ID').pressSequentially('user0001', { delay: 40 });
    await page.keyboard.press('Tab');
    await page.keyboard.type('password', { delay: 40 });
    await page.keyboard.press('Enter');
  },
  'ENTER button click': async (page) => {
    await page.getByLabel('User ID').fill('USER0001');
    await page.getByLabel('Password').fill('PASSWORD');
    await page.getByRole('button', { name: /^ENTER / }).click();
  },
};

for (const [name, enter] of Object.entries(entries)) {
  test(`first sign-on attempt succeeds: ${name}`, async ({ browser }) => {
    for (let i = 0; i < 3; i++) {
      const context = await browser.newContext();
      const page = await context.newPage();
      await page.goto('/signon');
      await enter(page);
      await expect(program(page), `attempt ${i + 1}: ${await message(page).textContent()}`).toHaveText('COMEN01C');
      await context.close();
    }
  });
}

test('sign-off then the next user signs on at the first attempt', async ({ page }) => {
  await page.goto('/signon');
  await entries['fill + Enter before the screen header loads'](page);
  await expect(program(page)).toHaveText('COMEN01C');
  await page.getByRole('button', { name: /^F3 / }).click();
  await expect(program(page)).toHaveText('COSGN00C');
  await page.getByLabel('User ID').fill('ADMIN001');
  await page.getByLabel('Password').fill('PASSWORD');
  await page.keyboard.press('Enter');
  await expect(program(page)).toHaveText('COADM01C');
});
