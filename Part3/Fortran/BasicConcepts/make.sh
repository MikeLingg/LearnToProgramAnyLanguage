#!/bin/bash
set +e
echo "Building Fortran program..."

rm -f BasicConcepts.exe
gfortran -o BasicConcepts.exe BasicConcepts.f90
echo "Fortran program built: ./BasicConcepts.exe"

rm -f BasicConcepts_ArrayOutOfBounds.exe
gfortran -o BasicConcepts_ArrayOutOfBounds.exe BasicConcepts_ArrayOutOfBounds.f90
echo "Fortran program built: ./BasicConcepts_ArrayOutOfBounds.exe"

