#!/bin/bash
set +e
echo "Building Fortran program..."

rm -f PrintMaze.exe
gfortran -o PrintMaze.exe PrintMaze.f90
echo "Fortran program built: ./PrintMaze.exe"
