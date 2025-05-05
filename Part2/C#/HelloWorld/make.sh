#!/bin/bash
set -e
echo "Building C# program..."
mcs -out:HelloWorld.exe HelloWorld.cs
echo "C# program built: HelloWorld.exe"
