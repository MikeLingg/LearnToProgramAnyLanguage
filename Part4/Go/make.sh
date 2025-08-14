#!/bin/bash
set +e

echo "Building Go program..."

rm -f SystemCalls.exe
go build -o SystemCalls.exe SystemCalls.go
echo "Go program built: ./SystemCalls.exe"

rm -f Function_AssignmentOutOfPlace.exe
go build -o Function_AssignmentOutOfPlace.exe Function_AssignmentOutOfPlace.go
echo "Go program built: ./Function_AssignmentOutOfPlace.exe"

rm -f Function_ExtraParameter.exe
go build -o Function_ExtraParameter.exe Function_ExtraParameter.go
echo "Go program built: ./Function_ExtraParameter.exe"

rm -f Function_FunctionReturnReversed.exe
go build -o Function_FunctionReturnReversed.exe Function_FunctionReturnReversed.go
echo "Go program built: ./Function_FunctionReturnReversed.exe"

rm -f Function_IgnoredReturn.exe
go build -o Function_IgnoredReturn.exe Function_IgnoredReturn.go
echo "Go program built: ./Function_IgnoredReturn.exe"

rm -f Function_IncorrectCapitalization.exe
go build -o Function_IncorrectCapitalization.exe Function_IncorrectCapitalization.go
echo "Go program built: ./Function_IncorrectCapitalization.exe"

rm -f Function_IncorrectFunctionName.exe
go build -o Function_IncorrectFunctionName.exe Function_IncorrectFunctionName.go
echo "Go program built: ./Function_IncorrectFunctionName.exe"

rm -f Function_MissingParameter.exe
go build -o Function_MissingParameter.exe Function_MissingParameter.go
echo "Go program built: ./Function_MissingParameter.exe"

rm -f Function_UsedEquivalance.exe
go build -o Function_UsedEquivalance.exe Function_UsedEquivalance.go
echo "Go program built: ./Function_UsedEquivalance.exe"

