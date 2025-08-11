#!/bin/bash

# Don't stop if any command fails
set +e

echo "Building C++ program..."

rm -f SystemCalls.exe
g++ -Wall -Wextra -O2 -o SystemCalls.exe SystemCalls.cpp
echo "C++ program built: ./SystemCalls.exe"

rm -f Function_AssignmentOutOfPlace.exe
g++ -Wall -Wextra -O2 -o Function_AssignmentOutOfPlace.exe Function_AssignmentOutOfPlace.cpp
echo "C++ program built: ./Function_AssignmentOutOfPlace.exe"

rm -f Function_ExtraParameter.exe
g++ -Wall -Wextra -O2 -o Function_ExtraParameter.exe Function_ExtraParameter.cpp
echo "C++ program built: ./Function_ExtraParameter.exe"

rm -f Function_FunctionReturnReversed.exe
g++ -Wall -Wextra -O2 -o Function_FunctionReturnReversed.exe Function_FunctionReturnReversed.cpp
echo "C++ program built: ./Function_FunctionReturnReversed.exe"

rm -f Function_IgnoredReturn.exe
g++ -Wall -Wextra -O2 -o Function_IgnoredReturn.exe Function_IgnoredReturn.cpp
echo "C++ program built: ./Function_IgnoredReturn.exe"

rm -f Function_IncorrectCapitalization.exe
g++ -Wall -Wextra -O2 -o Function_IncorrectCapitalization.exe Function_IncorrectCapitalization.cpp
echo "C++ program built: ./Function_IncorrectCapitalization.exe"

rm -f Function_IncorrectFunctionName.exe
g++ -Wall -Wextra -O2 -o Function_IncorrectFunctionName.exe Function_IncorrectFunctionName.cpp
echo "C++ program built: ./Function_IncorrectFunctionName.exe"

rm -f Function_MissingParameter.exe
g++ -Wall -Wextra -O2 -o Function_MissingParameter.exe Function_MissingParameter.cpp
echo "C++ program built: ./Function_MissingParameter.exe"

rm -f Function_UsedEquivalance.exe
g++ -Wall -Wextra -O2 -o Function_UsedEquivalance.exe Function_UsedEquivalance.cpp
echo "C++ program built: ./Function_UsedEquivalance.exe"

