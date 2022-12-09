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
flags="-Wall -Wextra -Wno-tabs"

#fc=ifort
#flags="-check all -check bounds -traceback -check uninit"

$fc -o main main.f90 $flags

#===============================================================================

