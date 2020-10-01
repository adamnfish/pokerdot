/**
 * Functions for storing and retrieving saved games in the browser
 */

const storageKey = "pokerdot-games"
const maxAge = 1000 * 60 * 60 * 24 * 7; // 7 days

export function getGameLibrary() {
    try {
        const games = loadSavedGames();
        console.log("saved games:", games);
        // clear out old games
        const filtered = filterExpiredGames(games);
        persistSavedGames(filtered);
        return filtered;
    } catch(e) {
        console.error("Unable to load saved games", e);
        persistSavedGames([]);
        return [];
    }
}

export function removeGame(games, game) {
    const filteredGames = games.filter(function(savedGame) {
        const match = savedGame.gameId == game.gameId && savedGame.playerKey == game.playerKey;
        return !match;
    });
    persistSavedGames(filteredGames);
    return filteredGames;
}

export function saveGame(game) {
    game.startTime = new Date().getTime();

    const existingGames = loadSavedGames();
    // remove existing instance of this player / game combination before we save
    const filteredGames = existingGames.filter(function(savedGame) {
        const match = savedGame.gameId == game.gameId && savedGame.playerKey == game.playerKey;
        return !match;
    });

    filteredGames.unshift(game);
    return persistSavedGames(filteredGames);
}

function filterExpiredGames(games) {
    const now = new Date().getTime();
    return games.filter(function(game) {
        return game.startTime > (now - maxAge);
    });
}

function loadSavedGames() {
    return JSON.parse(localStorage.getItem(storageKey)) || [];
}

function persistSavedGames(games) {
    return localStorage.setItem(storageKey, JSON.stringify(games));
}
