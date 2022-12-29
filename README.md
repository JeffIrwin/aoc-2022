
# aoc-2022

Advent of code 2022 in Fortran

https://adventofcode.com/

The solution for each day is implemented in `yyyy/n/main.f90`, where `yyyy` is the year and `n` is the day, e.g. 2022/1, 2022/2, 2022/3, ...

To compile and run an implementation:

    cd 2022/1
    ../../build.sh
    ./main

The solution is printed to the console.

## Requirements

1. A Fortran compiler, preferably `gfortran` or `ifort`.  Configure the compiler in [`build.sh`](build.sh).

## Index

My difficulty ratings are obviously subjective.  What's hard for me might be easy for others.  Also, there is a path-dependence to the difficulty.  For example, I thought [2022 day 7](2022/7) was particularly hard compared to days 1-6, mostly because I had never implemented a general tree data structure in Fortran.  After that, I used trees in several other days (e.g. [day 13](2022/13)).  Since I already had trees in my toolbelt, I didn't feel like that was as difficult anymore.

### 2019

| Day | Description | Difficulty |
|-----|-------------|------------|
|   [2](2019/2) | Intcode: add, multiply, and finish | 😀 |
|   [5](2019/5) | Intcode: input, output, and parameter modes | |

### 2022

| Day | Description | Difficulty |
|-----|-------------|------------|
|   [1](2022/1) | Summing integers | 😀 |
|   [2](2022/2) | Playing rock paper scissors | 😀 |
|   [3](2022/3) | Finding common characters in multiple strings | 😀 |
|   [4](2022/4) | Comparing integer ranges | 😀 |
|   [5](2022/5) | Rearranging data in a rank-2 array | 😅😅 |
|   [6](2022/6) | Searching for substrings that contain a unique set of characters | 😀 |
|   [7](2022/7) | Reconstructing a filesystem by parsing shell commands and their output | 🥵🥵🥵 |
|   [8](2022/8) | Comparing integers in a rank-2 array | 😅😅 |
|   [9](2022/9) | Implementing snake game kinematics | 😅😅 |
| [10](2022/10) | Drawing pixels on a simple screen | 😀 |
| [11](2022/11) | Performing bignum arithmetic, or is it 😉 | 😅😅 |
| [12](2022/12) | Finding the shortest path | 😅😅 |
| [13](2022/13) | Parsing and comparing arbitrarily-nested JSON arrays | 😅😅 |
| [14](2022/14) | Detecting collisions in a pixel game | 😀 |
| [15](2022/15) | Searching a large space with "beacons" and "sensors" | 😅😅 |
| [16](2022/16) | Traversing paths in a graph and maximization | 💀💀💀💀 |
| [17](2022/17) | Simulating a trillion tetris pieces | part 1 😅😅, part 2 💀💀💀💀 |
| [18](2022/18) | Finding the surface area of a voxel mesh by flood filling | 😅😅 |
| 19 | Looks like another dynamic programming problem like day 16?  Ion want nothing to do with that | |
| [20](2022/20) | Permuting numbers in a circular list | 😅😅 |
| [21](2022/21) | Evaluating arithmetic expressions and symbolic algebra | 😅😅 |
| [22](2022/22) | Following a path with obstacles in 2D and projected 3D | 🥵🥵🥵 |
| [23](2022/23) | Implementing cellular automata | 😅😅* couldn't crack part2, probably some dump typo |
| [24](2022/24) | Finding the shortest path on a map with moving obstacles | 😅😅 |
| [25](2022/25) | Arithmetic in base-5 signed-digit representation | 😀 |

