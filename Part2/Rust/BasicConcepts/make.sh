#!/bin/bash
set +e
echo "Building Rust program..."

rm -f BasicConcepts.exe
rustc BasicConcepts.rs -o BasicConcepts.exe
echo "Rust program built: ./BasicConcepts.exe"

rm -f BasicConcepts_InvalidASCII.exe
rustc BasicConcepts_InvalidASCII.rs -o BasicConcepts_InvalidASCII.exe
echo "Rust program built: ./BasicConcepts_InvalidASCII.exe"

rm -f BasicConcepts_InvalidNames.exe
rustc BasicConcepts_InvalidNames.rs -o BasicConcepts_InvalidNames.exe
echo "Rust program built: ./BasicConcepts_InvalidNames.exe"

rm -f BasicConcepts_Uninitialized.exe
rustc BasicConcepts_Uninitialized.rs -o BasicConcepts_Uninitialized.exe
echo "Rust program built: ./BasicConcepts_Uninitialized.exe"

rm -f BasicConcepts_Redeclaration.exe
rustc BasicConcepts_Redeclaration.rs -o BasicConcepts_Redeclaration.exe
echo "Rust program built: ./BasicConcepts_Redeclaration.exe"

rm -f BasicConcepts_MixedTypes.exe
rustc BasicConcepts_MixedTypes.rs -o BasicConcepts_MixedTypes.exe
echo "Rust program built: ./BasicConcepts_MixedTypes.exe"

