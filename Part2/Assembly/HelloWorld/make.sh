#!/bin/bash

# Don't stop if any command fails
set +e

# Check if filename was provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <base_filename_without_extension>"
    exit 1
fi

BASENAME="$1"
SRC="${BASENAME}.asm"
OBJ="${BASENAME}.o"
OUT="${BASENAME}.exe"

rm -r "$OUT"

# Assemble
nasm -f elf64 "$SRC" -o "$OBJ"
if [ $? -ne 0 ]; then
    echo "Assembly failed."
    exit 1
fi

# Link
ld "$OBJ" -o "$OUT"
if [ $? -ne 0 ]; then
    echo "Linking failed."
    exit 1
fi

echo "Build successful. Run with ./$OUT"
