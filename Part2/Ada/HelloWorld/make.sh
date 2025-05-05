#!/bin/bash
set -e
echo "Building Ada program..."
gnatmake HelloWorld.adb
echo "Ada program built: ./HelloWorld"
