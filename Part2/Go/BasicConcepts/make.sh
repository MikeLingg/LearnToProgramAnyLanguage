#!/bin/bash
set +e

echo "Building Go program..."

rm -f BasicConcepts.exe
go build -o BasicConcepts.exe BasicConcepts.go
echo "Go program built: ./BasicConcepts.exe"

rm -f BasicConcepts_InvalidASCII.exe
go build -o BasicConcepts_InvalidASCII.exe BasicConcepts_InvalidASCII.go
echo "Go program built: ./BasicConcepts_InvalidASCII.exe"

rm -f BasicConcepts_InvalidNames.exe
go build -o BasicConcepts_InvalidNames.exe BasicConcepts_InvalidNames.go
echo "Go program built: ./BasicConcepts_InvalidNames.exe"

rm -f BasicConcepts_Uninitialized.exe
go build -o BasicConcepts_Uninitialized.exe BasicConcepts_Uninitialized.go
echo "Go program built: ./BasicConcepts_Uninitialized.exe"

rm -f BasicConcepts_Redeclaration.exe
go build -o BasicConcepts_Redeclaration.exe BasicConcepts_Redeclaration.go
echo "Go program built: ./BasicConcepts_Redeclaration.exe"

rm -f BasicConcepts_MixedTypes.exe
go build -o BasicConcepts_MixedTypes.exe BasicConcepts_MixedTypes.go
echo "Go program built: ./BasicConcepts_MixedTypes.exe"

