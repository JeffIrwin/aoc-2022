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
#flags="-cpp -Wall -Wextra -Wno-tabs"
flags="-cpp -Wall -Wextra -Wno-tabs -fbounds-check"

#fc=ifort
#flags="-fpp -check all -check bounds -traceback -check uninit"

$fc -o main ../utils.f90 main.f90 $flags

#===============================================================================

