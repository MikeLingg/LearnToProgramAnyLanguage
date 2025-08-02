#!/bin/bash
set +e
echo "Building Rust program..."

rm -f BasicConcepts.exe
rustc BasicConcepts.rs -o BasicConcepts.exe
echo "Rust program built: ./BasicConcepts.exe"

rm -f BasicConcepts_ArrayOutOfBounds.exe
rustc BasicConcepts_ArrayOutOfBounds.rs -o BasicConcepts_ArrayOutOfBounds.exe
echo "Rust program built: ./BasicConcepts_ArrayOutOfBounds.exe"

