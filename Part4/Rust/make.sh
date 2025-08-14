#!/bin/bash
set -e
echo "Building Rust program..."

rm -f SystemCalls.exe
rustc SystemCalls.rs -o SystemCalls.exe || true
echo "Rust program built: ./SystemCalls.exe"

rm -f Function_AssignmentOutOfPlace.exe
rustc Function_AssignmentOutOfPlace.rs -o Function_AssignmentOutOfPlace.exe || true
echo "Rust program built: ./Function_AssignmentOutOfPlace.exe"

rm -f Function_ExtraParameters.exe
rustc Function_ExtraParameters.rs -o Function_ExtraParameters.exe || true
echo "Rust program built: ./Function_ExtraParameters.exe"

rm -f Function_FunctionReturnReversed.exe
rustc Function_FunctionReturnReversed.rs -o Function_FunctionReturnReversed.exe || true
echo "Rust program built: ./Function_FunctionReturnReversed.exe"

rm -f Function_IgnoredReturn.exe
rustc Function_IgnoredReturn.rs -o Function_IgnoredReturn.exe || true
echo "Rust program built: ./Function_IgnoredReturn.exe"

rm -f Function_IncorrectCapitalization.exe
rustc Function_IncorrectCapitalization.rs -o Function_IncorrectCapitalization.exe || true
echo "Rust program built: ./Function_IncorrectCapitalization.exe"

rm -f Function_IncorrectFunctionName.exe
rustc Function_IncorrectFunctionName.rs -o Function_IncorrectFunctionName.exe || true
echo "Rust program built: ./Function_IncorrectFunctionName.exe"

rm -f Function_MissingParameter.exe
rustc Function_MissingParameter.rs -o Function_MissingParameter.exe || true
echo "Rust program built: ./Function_MissingParameter.exe"

rm -f Function_UsedEquivalance.exe
rustc Function_UsedEquivalance.rs -o Function_UsedEquivalance.exe || true
echo "Rust program built: ./Function_UsedEquivalance.exe"

