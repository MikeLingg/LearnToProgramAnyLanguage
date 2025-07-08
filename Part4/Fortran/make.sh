#!/bin/bash
set +e
echo "Building Fortran program..."

rm -f SystemCalls.exe
gfortran -o SystemCalls.exe SystemCalls.f90
echo "Fortran program built: ./SystemCalls.exe"
