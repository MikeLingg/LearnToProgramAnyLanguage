#!/bin/bash
set +e
echo "Building Fortran program..."

rm -f SystemCalls.exe
gfortran -o SystemCalls.exe SystemCalls.f90
echo "Fortran program built: ./SystemCalls.exe"

rm -f Function_AssignmentOutOfPlace.exe
gfortran -o Function_AssignmentOutOfPlace.exe Function_AssignmentOutOfPlace.f90
echo "Fortran program built: ./Function_AssignmentOutOfPlace.exe"

rm -f Function_ExtraParameter.exe
gfortran -o Function_ExtraParameter.exe Function_ExtraParameter.f90
echo "Fortran program built: ./Function_ExtraParameter.exe"

rm -f Function_FunctionReturnReversed.exe
gfortran -o Function_FunctionReturnReversed.exe Function_FunctionReturnReversed.f90
echo "Fortran program built: ./Function_FunctionReturnReversed.exe"

rm -f Function_IngoredReturn.exe
gfortran -o Function_IngoredReturn.exe Function_IngoredReturn.f90
echo "Fortran program built: ./Function_IngoredReturn.exe"

rm -f Function_IncorrectCapitalization.exe
gfortran -o Function_IncorrectCapitalization.exe Function_IncorrectCapitalization.f90
echo "Fortran program built: ./Function_IncorrectCapitalization.exe"

rm -f Function_IncorrectFunctionName.exe
gfortran -o Function_IncorrectFunctionName.exe Function_IncorrectFunctionName.f90
echo "Fortran program built: ./Function_IncorrectFunctionName.exe"

rm -f Function_MissingParameter.exe
gfortran -o Function_MissingParameter.exe Function_MissingParameter.f90
echo "Fortran program built: ./Function_MissingParameter.exe"

rm -f Function_UsedEquivalance.exe
gfortran -o Function_UsedEquivalance.exe Function_UsedEquivalance.f90
echo "Fortran program built: ./Function_UsedEquivalance.exe"

