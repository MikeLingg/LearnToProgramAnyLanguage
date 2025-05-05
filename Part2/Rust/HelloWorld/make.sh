#!/bin/bash
set -e
echo "Building Rust program..."
rustc HelloWorld.rs -o HelloWorld
echo "Rust program built: ./HelloWorld"
