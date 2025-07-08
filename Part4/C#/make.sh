#!/bin/bash
set +e
echo "Building C# program..."

rm -r SystemCalls.exe
mcs -out:SystemCalls.exe SystemCalls.cs
echo "C# program built: SystemCalls.exe"
