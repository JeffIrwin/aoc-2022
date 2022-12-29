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

$fc --version

$fc -o main ../../utils.f90 ../intcode.f90 main.f90 $flags

#===============================================================================

