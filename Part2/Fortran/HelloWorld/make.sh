#!/bin/bash
set +e
echo "Building Fortran program..."

rm -f HellowWorld.exe
gfortran -o HelloWorld.exe HelloWorld.f90
echo "Fortran program built: ./HelloWorld.exe"
