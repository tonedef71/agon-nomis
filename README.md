# agon-nomis
NOMIS: a clone of the classic game [*Simon*](https://en.wikipedia.org/wiki/Simon_(game)) written in [BBC BASIC v3](https://en.wikipedia.org/wiki/BBC_BASIC)
* A colorful, musical game of ["Follow the Leader"](https://en.wikipedia.org/wiki/Follow_the_leader_(game))
* Won first place in the [1st Olimex AgonLight Weekend Programming Challenge 2023](https://olimex.wordpress.com/2023/04/21/agonlight-weekend-programming-challenge-issue-1/)
* NOMIS may be played on an [AgonLight retro-computer](https://www.olimex.com/Products/Retro-Computers/AgonLight2/open-source-hardware), the [AgonLight Emulator](https://github.com/astralaster/agon-light-emulator/releases), the upcoming [Agon Console8](https://heber.co.uk/agon-console8/), or the [BBC BASIC SDL](http://www.bbcbasic.co.uk/bbcsdl/).

## Display
NOMIS supports multiple display modes on the AgonLight.  The game should be played in a mode that supports a minimum of 16 colors and minimum screen dimensions of 40 columns by 22 rows.

On BBC BASIC SDL, one display mode (MODE 9) is supported.

## Controls
Use the keys R, F, J, and I keys (comfortably located near the "home row" keys on a QWERTY keyboard) to press the colored light-up sensor pads (Green, Yellow, Blue, and Red, respectively) to correctly repeat a longer and longer series of signals.

## Difficulty
There are four levels of difficulty:
1. The player must correctly repeat eight signals to win the game
2. The player must correctly repeat fourteen signals to win the game
3. The player must correctly repeat twenty signals to win the game
4. The player must correctly repeat thirty-one signals to win the game

## Best Score
* The default best score is six out of eight.
* The best score will be saved to a file named `nomis.hi` in the same folder as the `nomis.bas` file.
* NOMIS must be run from the exact folder where the `nomis.hi` file resides in order for the saved best score to be read-in by the game.

## Demo Playthrough
https://github.com/tonedef71/agon-nomis/assets/3978924/4d8dd15b-6c95-47c7-8b17-6cbd326683a1

