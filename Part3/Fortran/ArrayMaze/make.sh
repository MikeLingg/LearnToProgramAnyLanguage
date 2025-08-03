#!/bin/bash
set +e
echo "Building Fortran program..."

rm -f ArrayMaze.exe
gfortran -o ArrayMaze.exe ArrayMaze.f90
echo "Fortran program built: ./ArrayMaze.exe"


