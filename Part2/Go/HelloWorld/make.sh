#!/bin/bash
set -e
echo "Building Go program..."
go build -o HelloWorld HelloWorld.go
echo "Go program built: ./HelloWorld"
