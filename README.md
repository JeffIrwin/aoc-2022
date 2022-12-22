
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
|   [1](1) | Summing integers | 😀 |
|   [2](2) | Playing rock paper scissors | 😀 |
|   [3](3) | Finding common characters in multiple strings | 😀 |
|   [4](4) | Comparing integer ranges | 😀 |
|   [5](5) | Rearranging data in a rank-2 array | 😅😅 |
|   [6](6) | Searching for substrings that contain a unique set of characters | 😀 |
|   [7](7) | Reconstructing a filesystem by parsing shell commands and their output | 🥵🥵🥵 |
|   [8](8) | Comparing integers in a rank-2 array | 😅😅 |
|   [9](9) | Implementing snake game kinematics | 😅😅 |
| [10](10) | Drawing pixels on a simple screen | 😀 |
| [11](11) | Performing bignum arithmetic, or is it 😉 | 😅😅 |
| [12](12) | Finding the shortest path | 😅😅 |
| [13](13) | Parsing and comparing arbitrarily-nested JSON arrays | 😅😅 |
| [14](14) | Detecting collisions in a pixel game | 😀 |
| [15](15) | Searching a large space with "beacons" and "sensors" | 😅😅 |
| [16](16) | Traversing paths in a graph and maximization | 💀💀💀💀 |
| [17](17) | Simulating a trillion tetris pieces | part 1 😅😅, part 2 💀💀💀💀 |
| [18](18) | Finding the surface area of a voxel mesh by flood filling | 😅😅 |
| 19 | Looks like another dynamic programming problem like day 16?  Ion want nothing to do with that | |
| [20](20) | Permuting numbers in a circular list | 😅😅 |
| [21](21) | Evaluating arithmetic expressions and symbolic algebra | 😅😅 |

