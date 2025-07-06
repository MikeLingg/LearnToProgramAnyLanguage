#!/bin/bash
set +e
echo "Building Ada program..."

rm -f BasicConcepts.exe
gnatmake BasicConcepts.adb -o BasicConcepts.exe
echo "Ada program built: ./BasicConcepts.exe"

rm -f BasicConcepts_InvalidASCII.exe
gnatmake BasicConcepts_InvalidASCII.adb -o BasicConcepts_InvalidASCII.exe
echo "Ada program built: ./BasicConcepts_InvalidASCII.exe"

rm -f BasicConcepts_InvalidNames.exe
gnatmake BasicConcepts_InvalidNames.adb -o BasicConcepts_InvalidNames.exe
echo "Ada program built: ./BasicConcepts_InvalidNames.exe"

rm -f BasicConcepts_Uninitialized.exe
gnatmake BasicConcepts_Uninitialized.adb -o BasicConcepts_Uninitialized.exe
echo "Ada program built: ./BasicConcepts_Uninitialized.exe"

rm -f BasicConcepts_Redeclaration.exe
gnatmake BasicConcepts_Redeclaration.adb -o BasicConcepts_Redeclaration.exe
echo "Ada program built: ./BasicConcepts_Redeclaration.exe"

rm -f BasicConcepts_MixedTypes.exe
gnatmake BasicConcepts_MixedTypes.adb -o BasicConcepts_MixedTypes.exe
echo "Ada program built: ./BasicConcepts_MixedTypes.exe"

