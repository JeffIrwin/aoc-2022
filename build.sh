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
fc=ifort

$fc -o main main.f90

#===============================================================================

