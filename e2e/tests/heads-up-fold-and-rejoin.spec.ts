import { test, expect, devices } from '@playwright/test';
import { createGame, joinGame, startGame, fold, getJoinCodeFromLink, snap, waitForWelcome } from './helpers';

test.setTimeout(90_000);

test('heads up fold and rejoin', async ({ browser }, testInfo) => {
  const iphoneCtx = await browser.newContext({ ...devices['iPhone 15'] });
  const androidCtx = await browser.newContext({ ...devices['Pixel 8'] });
  const iphone = await iphoneCtx.newPage();
  const android = await androidCtx.newPage();

  try {
    // --- Step 1: Alice creates the game ---
    await waitForWelcome(iphone);
    await snap(iphone, '01-iphone-welcome', testInfo);
    await createGame(iphone, { gameName: 'E2E Test', playerName: 'Alice' });
    await snap(iphone, '02-iphone-create-form', testInfo);

    // Wait for lobby
    await expect(iphone.getByRole('link', { name: /join link/i })).toBeVisible();
    await snap(iphone, '03-iphone-lobby', testInfo);

    const gameCode = await getJoinCodeFromLink(iphone);

    // --- Step 2: Bob joins via game code ---
    await waitForWelcome(android);
    await snap(android, '04-android-welcome', testInfo);
    await joinGame(android, { gameCode, playerName: 'Bob' });
    await snap(android, '05-android-join-form', testInfo);

    // Wait for Bob's lobby — positive check that Alice is visible in the player list
    await expect(android.getByText(/Alice/i).first()).toBeVisible();
    await snap(android, '06-android-lobby', testInfo);

    // --- Step 3: Alice starts the game ---
    await startGame(iphone);
    // Wait for the server to deal the first hand before snapping
    await expect(
      iphone.getByRole('button', { name: /fold/i })
        .or(iphone.getByRole('button', { name: /deal/i }))
        .or(iphone.getByRole('button', { name: /peek/i }))
        .first()
    ).toBeVisible();
    await snap(iphone, '07-iphone-game-start', testInfo);

    // --- Step 4: Bob's game screen ---
    // Wait for Bob to reach the game screen; he may or may not be first to act
    await expect(android.getByText('Bob').first()).toBeVisible();
    await snap(android, '08-android-game-start', testInfo);

    // --- Step 5: Whoever acts first folds ---
    // Wait for the fold button to appear on either page before checking which player is active
    await Promise.any([
      iphone.getByRole('button', { name: /fold/i }).waitFor(),
      android.getByRole('button', { name: /fold/i }).waitFor(),
    ]);
    const iphoneFoldVisible = await iphone.getByRole('button', { name: /fold/i }).isVisible();
    const androidFoldVisible = await android.getByRole('button', { name: /fold/i }).isVisible();

    if (iphoneFoldVisible) {
      await snap(iphone, '09-iphone-pre-fold', testInfo);
      await fold(iphone);
      await snap(iphone, '10-iphone-folded', testInfo);
    } else if (androidFoldVisible) {
      await snap(android, '09-android-pre-fold', testInfo);
      await fold(android);
      await snap(android, '10-android-folded', testInfo);
    } else {
      throw new Error('Neither player has the fold button — cannot determine who acts first');
    }

    // --- Step 6: Both see round advance ---
    // Wait for the folder's screen to show the between-hands state
    const foldedPage = iphoneFoldVisible ? iphone : android;
    await expect(
      foldedPage.getByRole('button', { name: /deal/i })
        .or(foldedPage.getByRole('button', { name: /peek/i }))
        .first()
    ).toBeVisible();
    await snap(iphone, '11-iphone-next-hand', testInfo);
    await snap(android, '12-android-next-hand', testInfo);

    // --- Step 7: Bob navigates home (simulates returning via home button) ---
    // goto('/') exercises the library/rejoin flow; reload() would reconnect directly at /#game/…
    await android.goto('/');
    await expect(android.getByRole('button', { name: /E2E Test/i })).toBeVisible();
    await snap(android, '13-android-welcome-with-library', testInfo);

    // --- Step 8: Bob rejoins ---
    await android.getByRole('button', { name: /E2E Test/i }).click();
    // After rejoin: game may be in any state — deal/peek (between hands), fold/call (active round)
    await expect(
      android.getByRole('button', { name: /deal/i })
        .or(android.getByRole('button', { name: /peek/i }))
        .or(android.getByRole('button', { name: /fold/i }))
        .first()
    ).toBeVisible();
    await snap(android, '14-android-rejoined', testInfo);

    // --- Step 9: Alice still sees Bob in the game ---
    await snap(iphone, '15-iphone-after-rejoin', testInfo);
    await expect(iphone.getByText(/Bob/i).first()).toBeVisible();
  } finally {
    await iphoneCtx.close();
    await androidCtx.close();
  }
});
