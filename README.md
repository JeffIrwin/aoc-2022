
# aoc-2022

Advent of code 2022 in Fortran

https://adventofcode.com/

The solution for each day is implemented in `n/main.f90`, where `n` is the day, e.g. 1, 2, 3, ...

To compile and run an implementation:

    cd 1
    ../build.sh
    ./main

The solution is printed to the console.

## Requirements

1. A Fortran compiler, preferably `gfortran` or `ifort`.  Configure the compiler in [`build.sh`](build.sh).

## Index

| Day | Description | Difficulty |
|-----|-------------|------------|
|  1  | Summing integers | 😀 |
|  2  | Rock paper scissors | 😀 |
|  3  | Finding common characters in multiple strings | 😀 |
|  4  | Comparing integer ranges | 😀 |
|  5  | Rearranging data in a rank-2 array | 😅 |
|  6  | Searching for substrings that contain a unique set of characters | 😀 |
|  7  | Reconstructing a filesystem by parsing shell commands and their output | 🥵 |
|  8  | Comparing integers in a rank-2 array | 😅 |
|  9  | |   |

