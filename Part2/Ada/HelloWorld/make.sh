#!/bin/bash
set +e
echo "Building Ada program..."

rm -f HelloWorld.exe
gnatmake HelloWorld.adb -o HelloWorld.exe
echo "Ada program built: ./HelloWorld.exe"
