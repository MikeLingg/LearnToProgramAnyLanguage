#!/bin/bash
set +e
echo "Building C# program..."

rm -r BasicConcepts.exe
mcs -out:BasicConcepts.exe BasicConcepts.cs
echo "C# program built: BasicConcepts.exe"

rm -r BasicConcepts_InvalidASCII.exe
mcs -out:BasicConcepts_InvalidASCII.exe BasicConcepts_InvalidASCII.cs
echo "C# program built: BasicConcepts_InvalidASCII.exe"

rm -r BasicConcepts_InvalidNames.exe
mcs -out:BasicConcepts_InvalidNames.exe BasicConcepts_InvalidNames.cs
echo "C# program built: BasicConcepts_InvalidNames.exe"

rm -r BasicConcepts_Uninitialized.exe
mcs -out:BasicConcepts_Uninitialized.exe BasicConcepts_Uninitialized.cs
echo "C# program built: BasicConcepts_Uninitialized.exe"

rm -r BasicConcepts_Redeclaration.exe
mcs -out:BasicConcepts_Redeclaration.exe BasicConcepts_Redeclaration.cs
echo "C# program built: BasicConcepts_Redeclaration.exe"

rm -r BasicConcepts_MixedTypes.exe
mcs -out:BasicConcepts_MixedTypes.exe BasicConcepts_MixedTypes.cs
echo "C# program built: BasicConcepts_MixedTypes.exe"

