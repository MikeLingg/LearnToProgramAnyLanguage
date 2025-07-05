#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C++ program..."
rm -f BasicConcepts
g++ -Wall -Wextra -O2 -o BasicConcepts BasicConcepts.cpp
echo "C++ program built: ./BasicConcepts"
rm -f BasicConcepts_InvalidASCII
g++ -Wall -Wextra -O2 -o BasicConcepts_InvalidASCII BasicConcepts_InvalidASCII.cpp
echo "C++ program built: ./BasicConcepts_InvalidASCII"
rm -f BasicConcepts_InvalidNames
g++ -Wall -Wextra -O2 -o BasicConcepts_InvalidNames BasicConcepts_InvalidNames.cpp
echo "C++ program built: ./BasicConcepts_InvalidNames"
rm -f BasicConcepts_Uninitialized
g++ -Wall -Wextra -O2 -o BasicConcepts_Uninitialized BasicConcepts_Uninitialized.cpp
echo "C++ program built: ./BasicConcepts_Uninitialized"
rm -f BasicConcepts_Redeclaration
g++ -Wall -Wextra -O2 -o BasicConcepts_Redeclaration BasicConcepts_Redeclaration.cpp
echo "C++ program built: ./BasicConcepts_Redeclaration"
rm -f BasicConcepts_MixedTypes
g++ -Wall -Wextra -O2 -o BasicConcepts_MixedTypes BasicConcepts_MixedTypes.cpp
echo "C++ program built: ./BasicConcepts_MixedTypes"
