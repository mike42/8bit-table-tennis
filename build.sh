#!/bin/bash
set -exu -o pipefail
mkdir -p build


#rm -f example.o example.nes example.map.txt example.labels.txt example.nes.ram.nl example.nes.0.nl example.nes.1.nl example.nes.dbg
# Create 
./tools/chr_tool.py res/background.png --output build/background.chr
./tools/chr_tool.py res/sprite.png --output build/sprite.chr

ca65 table_tennis.s -g -o build/table_tennis.o
ld65 -o build/table_tennis.nes -C table_tennis.cfg build/table_tennis.o -m build/table_tennis.map.txt -Ln build/table_tennis.labels.txt --dbgfile build/table_tennis.nes.dbg
python3 tools/table_tennis_fceux_symbols.py
