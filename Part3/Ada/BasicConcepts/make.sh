#!/bin/bash
set +e
echo "Building Ada program..."

rm -f BasicConcepts.exe
gnatmake BasicConcepts.adb -o BasicConcepts.exe
echo "Ada program built: ./BasicConcepts.exe"

