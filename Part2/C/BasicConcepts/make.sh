#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C program..."

rm -f BasicConcepts.exe
gcc -Wall -Wextra -O2 -o BasicConcepts.exe BasicConcepts.c
echo "C program built: ./BasicConcepts.exe"

rm -f BasicConcepts_InvalidASCII.exe
gcc -Wall -Wextra -O2 -o BasicConcepts_InvalidASCII.exe BasicConcepts_InvalidASCII.c
echo "C program built: ./BasicConcepts_InvalidASCII.exe"

rm -f BasicConcepts_InvalidNames.exe
gcc -Wall -Wextra -O2 -o BasicConcepts_InvalidNames.exe BasicConcepts_InvalidNames.c
echo "C program built: ./BasicConcepts_InvalidNames.exe"

rm -f BasicConcepts_Uninitialized.exe
gcc -Wall -Wextra -O2 -o BasicConcepts_Uninitialized.exe BasicConcepts_Uninitialized.c
echo "C program built: ./BasicConcepts_Uninitialized.exe"

rm -f BasicConcepts_Redeclaration.exe
gcc -Wall -Wextra -O2 -o BasicConcepts_Redeclaration.exe BasicConcepts_Redeclaration.c
echo "C program built: ./BasicConcepts_Redeclaration.exe"

rm -f BasicConcepts_MixedTypes.exe
gcc -Wall -Wextra -O2 -o BasicConcepts_MixedTypes.exe BasicConcepts_MixedTypes.c
echo "C program built: ./BasicConcepts_MixedTypes.exe"
