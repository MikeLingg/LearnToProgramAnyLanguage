#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C++ program..."

rm -f BasicConcepts.exe
g++ -Wall -Wextra -O2 -o BasicConcepts.exe BasicConcepts.cpp
echo "C++ program built: ./BasicConcepts.exe"

rm -f BasicConcepts_InvalidASCII.exe
g++ -Wall -Wextra -O2 -o BasicConcepts_InvalidASCII.exe BasicConcepts_InvalidASCII.cpp
echo "C++ program built: ./BasicConcepts_InvalidASCII.exe"

rm -f BasicConcepts_InvalidNames.exe
g++ -Wall -Wextra -O2 -o BasicConcepts_InvalidNames.exe BasicConcepts_InvalidNames.cpp
echo "C++ program built: ./BasicConcepts_InvalidNames.exe"

rm -f BasicConcepts_Uninitialized.exe
g++ -Wall -Wextra -O2 -o BasicConcepts_Uninitialized.exe BasicConcepts_Uninitialized.cpp
echo "C++ program built: ./BasicConcepts_Uninitialized.exe"

rm -f BasicConcepts_Redeclaration.exe
g++ -Wall -Wextra -O2 -o BasicConcepts_Redeclaration.exe BasicConcepts_Redeclaration.cpp
echo "C++ program built: ./BasicConcepts_Redeclaration.exe"

rm -f BasicConcepts_MixedTypes.exe
g++ -Wall -Wextra -O2 -o BasicConcepts_MixedTypes.exe BasicConcepts_MixedTypes.cpp
echo "C++ program built: ./BasicConcepts_MixedTypes.exe"
