# Revision history for fearOfView

## 0.1.0.0 -- 2025-08-10
* First release.

## 0.1.0.3 -- 2025-08-12
* Fix incompatibility with recent GHC. Thanks Francesco Ariis.
* Fix WASD keybindings

## 0.1.1.0 -- 2025-08-15
* Add bearlibterminal-based UI (enabled with -fbear)

## 0.2.0.0 -- 2026-08-08
* Rework monsters: 4 monster types, each with upgrade
* Rework vision: visibility of walls independent of tiles
* Increase difficulty on breaking exit seal rather than on leaving
* Increase only wall density over time within a round
* Uncover seen tiles and exits in order of distance, increasing predictability
* Increase life to 7
* Reduce treasures per round
* Reduce equipment junk costs
* Preserve status effects across levels of a round
* Remove minipotions
* Leave Camera in current location on use
* Limit Umbrella to 8 uses
* Rename GrabHand to Hook; disallow hooking placed cameras and rolling orbs
* Allow use of Undo after death
* Prevent spawning walls next to player on entry and teleport
* Ensure key exit doesn't spawn by same tile as main exit
* Use more box-drawing characters for prettiness
* List treasures found in a level
* Include number of gems collected in highscore entry
* Show average score over last 10 games
