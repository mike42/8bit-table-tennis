# 8-Bit Table Tennis  [![Build Status](https://travis-ci.org/mike42/8bit-table-tennis.svg?branch=master)](https://travis-ci.org/mike42/8bit-table-tennis)

8-Bit Table Tennis is a homebrew 2-player game for the Nintendo Entertainment System (NES).

![Start screen](screenshot/table_tennis_start_screen.png)

![Start screen](screenshot/table_tennis_gameplay_00.png)

## Gamplay

- Push START to begin.
- Press UP/DOWN to move.
- Press A to serve.
- Player 1 serves first.
- If you lose the point, then you serve next.
- First player to 11 points wins.

## Try it

The most recent version is available in the [releases](https://github.com/mike42/8bit-table-tennis/releases) section. It is shipped as an iNES ROM (`.nes`), which is suitable for use in a NES emulator or flash cartridge.

## Build it

The steps to build this project are listed in [build.sh](https://github.com/mike42/8bit-table-tennis/blob/master/build.sh). You will need `python3` and the `cc65` toolchain installed.

## License

8-Bit Table Tennis may be used, distributed and modified under the terms of the MIT license, see [LICENSE](https://github.com/mike42/8bit-table-tennis/blob/master/LICENSE) for details.

The code is based on a NES example project by Brad Smith, which can be found at [bbbradsmith/NES-ca65-example](https://github.com/bbbradsmith/NES-ca65-example).

