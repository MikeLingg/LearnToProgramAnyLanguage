#!/bin/bash
set +e

echo "Building Go program..."

rm -f BasicConcepts.exe
go build -o BasicConcepts.exe BasicConcepts.go
echo "Go program built: ./BasicConcepts.exe"

