#!/bin/bash
set +e
echo "Building C# program..."

rm -r BasicConcepts.exe
mcs /unsafe -out:BasicConcepts.exe BasicConcepts.cs
echo "C# program built: BasicConcepts.exe"

rm -r BasicConcepts_ArrayOutOfBounds.exe
mcs /unsafe -out:BasicConcepts_ArrayOutOfBounds.exe BasicConcepts_ArrayOutOfBounds.cs
echo "C# program built: BasicConcepts_ArrayOutOfBounds.exe"

