# 8-Bit Table Tennis

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

This project may be used under the terms of the MIT license, see [LICENSE](https://github.com/mike42/8bit-table-tennis/blob/master/LICENSE) for details.

It is based on example code by Brad Smith - [bbbradsmith/NES-ca65-example](https://github.com/bbbradsmith/NES-ca65-example).
