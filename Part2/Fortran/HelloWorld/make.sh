#!/bin/bash
set -e
echo "Building Fortran program..."
gfortran -o HelloWorld HelloWorld.f90
echo "Fortran program built: ./HelloWorld"
