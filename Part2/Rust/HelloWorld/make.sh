#!/bin/bash
set -e
echo "Building Rust program..."

rm -r HelloWorld.exe
rustc HelloWorld.rs -o HelloWorld.exe
echo "Rust program built: ./HelloWorld.exe"
