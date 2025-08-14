#!/bin/bash
set +e
echo "Building Ada program..."

rm -f SystemCalls.exe
gnatmake SystemCalls.adb -o SystemCalls.exe
echo "Ada program built: ./SystemCalls.exe"

rm -f Function_AssignmentOutOfPlace.exe
gnatmake Function_AssignmentOutOfPlace.adb -o Function_AssignmentOutOfPlace.exe
echo "Ada program built: ./Function_AssignmentOutOfPlace.exe"

rm -f Function_ExtraParameter.exe
gnatmake Function_ExtraParameter.adb -o Function_ExtraParameter.exe
echo "Ada program built: ./Function_ExtraParameter.exe"

rm -f Function_FunctionReturnReversed.exe
gnatmake Function_FunctionReturnReversed.adb -o Function_FunctionReturnReversed.exe
echo "Ada program built: ./Function_FunctionReturnReversed.exe"

rm -f Function_IgnoredReturn.exe
gnatmake Function_IgnoredReturn.adb -o Function_IgnoredReturn.exe
echo "Ada program built: ./Function_IgnoredReturn.exe"

rm -f Function_IncorrectCapitalization.exe
gnatmake Function_IncorrectCapitalization.adb -o Function_IncorrectCapitalization.exe
echo "Ada program built: ./Function_IncorrectCapitalization.exe"

rm -f Function_IncorrectFunctionName.exe
gnatmake Function_IncorrectFunctionName.adb -o Function_IncorrectFunctionName.exe
echo "Ada program built: ./Function_IncorrectFunctionName.exe"

rm -f Function_UsedEquivalance.exe
gnatmake Function_UsedEquivalance.adb -o Function_UsedEquivalance.exe
echo "Ada program built: ./Function_UsedEquivalance.exe"

