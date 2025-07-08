#!/bin/bash
set +e
echo "Building Ada program..."

rm -f SystemCalls.exe
gnatmake SystemCalls.adb -o SystemCalls.exe
echo "Ada program built: ./SystemCalls.exe"
