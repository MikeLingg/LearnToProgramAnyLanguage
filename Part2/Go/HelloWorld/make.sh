#!/bin/bash
set -e
echo "Building Go program..."

rm -r HelloWorld.exe
go build -o HelloWorld.exe HelloWorld.go
echo "Go program built: ./HelloWorld.exe"
