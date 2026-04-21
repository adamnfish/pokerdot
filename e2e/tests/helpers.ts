import { Page, TestInfo } from '@playwright/test';

export async function waitForWelcome(page: Page): Promise<void> {
  await page.goto('/');
  await page.getByRole('button', { name: /create.*game/i }).waitFor();
}

export async function createGame(
  page: Page,
  { gameName, playerName }: { gameName: string; playerName: string }
): Promise<void> {
  await page.goto('/');
  await page.getByRole('button', { name: /create.*game/i }).click();
  await page.getByLabel(/game name/i).fill(gameName);
  await page.getByLabel(/your name/i).fill(playerName);
  await page.getByRole('button', { name: /create.*game/i }).click();
}

export async function joinGame(
  page: Page,
  { gameCode, playerName }: { gameCode: string; playerName: string }
): Promise<void> {
  await page.goto('/');
  await page.getByRole('button', { name: /join.*game/i }).click();
  await page.getByLabel(/game code/i).fill(gameCode);
  await page.getByLabel(/your name/i).fill(playerName);
  await page.getByRole('button', { name: /join.*game/i }).click();
}

export async function startGame(page: Page): Promise<void> {
  // Uses form defaults for starting stack and small blind
  await page.getByRole('button', { name: /start/i }).click();
}

export async function fold(page: Page): Promise<void> {
  await page.getByRole('button', { name: /fold/i }).click();
}

export async function getJoinCodeFromLink(page: Page): Promise<string> {
  // The host's share link has href="/#join/{gameCode}"
  const href = await page.getByRole('link', { name: /join link/i }).getAttribute('href');
  if (!href) throw new Error('Join link not found in lobby');
  const match = href.match(/#join\/(.+)$/);
  if (!match) throw new Error(`Unexpected join link format: ${href}`);
  return match[1];
}

export async function snap(page: Page, name: string, testInfo: TestInfo): Promise<void> {
  const buffer = await page.screenshot({ fullPage: true });
  await testInfo.attach(name, { body: buffer, contentType: 'image/png' });
}
