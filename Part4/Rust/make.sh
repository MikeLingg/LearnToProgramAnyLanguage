#!/bin/bash
set -e
echo "Building Rust program..."

rm -f SystemCalls.exe
rustc SystemCalls.rs -o SystemCalls.exe
echo "Rust program built: ./SystemCalls.exe"
