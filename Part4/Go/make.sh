#!/bin/bash
set +e

echo "Building Go program..."

rm -f SystemCalls.exe
go build -o SystemCalls.exe SystemCalls.go
echo "Go program built: ./SystemCalls.exe"

