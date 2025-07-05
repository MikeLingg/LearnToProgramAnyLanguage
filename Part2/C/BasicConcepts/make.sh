#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C program..."
rm -f BasicConcepts
gcc -Wall -Wextra -O2 -o BasicConcepts BasicConcepts.c
echo "C program built: ./BasicConcepts"
rm -f BasicConcepts_InvalidASCII
gcc -Wall -Wextra -O2 -o BasicConcepts_InvalidASCII BasicConcepts_InvalidASCII.c
echo "C program built: ./BasicConcepts_InvalidASCII"
rm -f BasicConcepts_InvalidNames
gcc -Wall -Wextra -O2 -o BasicConcepts_InvalidNames BasicConcepts_InvalidNames.c
echo "C program built: ./BasicConcepts_InvalidNames"
rm -f BasicConcepts_Uninitialized
gcc -Wall -Wextra -O2 -o BasicConcepts_Uninitialized BasicConcepts_Uninitialized.c
echo "C program built: ./BasicConcepts_Uninitialized"
rm -f BasicConcepts_Redeclaration
gcc -Wall -Wextra -O2 -o BasicConcepts_Redeclaration BasicConcepts_Redeclaration.c
echo "C program built: ./BasicConcepts_Redeclaration"
rm -f BasicConcepts_MixedTypes
gcc -Wall -Wextra -O2 -o BasicConcepts_MixedTypes BasicConcepts_MixedTypes.c
echo "C program built: ./BasicConcepts_MixedTypes"
