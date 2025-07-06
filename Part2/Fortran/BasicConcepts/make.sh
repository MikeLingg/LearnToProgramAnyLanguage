#!/bin/bash
set +e
echo "Building Fortran program..."

rm -f BasicConcepts.exe
gfortran -o BasicConcepts.exe BasicConcepts.f90
echo "Fortran program built: ./BasicConcepts.exe"

rm -f BasicConcepts_InvalidASCII.exe
gfortran -o BasicConcepts_InvalidASCII.exe BasicConcepts_InvalidASCII.f90
echo "Fortran program built: ./BasicConcepts_InvalidASCII.exe"

rm -f BasicConcepts_InvalidNames.exe
gfortran -o BasicConcepts_InvalidNames.exe BasicConcepts_InvalidNames.f90
echo "Fortran program built: ./BasicConcepts_InvalidNames.exe"

rm -f BasicConcepts_Uninitialized.exe
gfortran -o BasicConcepts_Uninitialized.exe BasicConcepts_Uninitialized.f90
echo "Fortran program built: ./BasicConcepts_Uninitialized.exe"

rm -f BasicConcepts_Redeclaration.exe
gfortran -o BasicConcepts_Redeclaration.exe BasicConcepts_Redeclaration.f90
echo "Fortran program built: ./BasicConcepts_Redeclaration.exe"

rm -f BasicConcepts_MixedTypes.exe
gfortran -o BasicConcepts_MixedTypes.exe BasicConcepts_MixedTypes.f90
echo "Fortran program built: ./BasicConcepts_MixedTypes.exe"

