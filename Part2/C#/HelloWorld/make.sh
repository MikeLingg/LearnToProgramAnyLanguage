#!/bin/bash
set +e
echo "Building C# program..."

rm -r HelloWorld.exe
mcs -out:HelloWorld.exe HelloWorld.cs
echo "C# program built: HelloWorld.exe"
