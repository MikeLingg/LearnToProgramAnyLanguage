#!/bin/bash
set -e
echo "Building Rust program..."

rm -f HelloWorld.exe
rustc HelloWorld.rs -o HelloWorld.exe
echo "Rust program built: ./HelloWorld.exe"
