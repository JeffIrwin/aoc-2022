#!/bin/bash

#===============================================================================
#
# Run this script from the day's directory, e.g.:
#
#     cd 1
#     ../build.sh
#     ./main
#
#===============================================================================

# Fortran compiler
fc=gfortran
#flags="-cpp -O3 -fopenmp"
#flags="-cpp -Wall -Wextra -Wno-tabs"
flags="-cpp -Wall -Wextra -Wno-tabs -fbounds-check -Wno-maybe-uninitialized"

#fc=ifort
#flags="-fpp -check all -check bounds -traceback -check uninit"

## fmlib directory
#fmlib="../submodules/fmlibj/"
#flags="$flags -I$fmlib"

$fc --version

##********
#
## Build fmlib (one-time only)
#pushd $fmlib
#
#gfortran -c -O3 fmsave.f95 fmzm90.f95 fm.f95
#ar rv fmlib.a *.o
#
#popd
#
##********

$fc -o main ../../utils.f90 main.f90 $flags
#$fc -o main $fmlib/fmlib.a ../../utils.f90 main.f90 $flags

#===============================================================================

