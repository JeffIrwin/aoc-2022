#!/bin/bash

if [[ $# -ne 1 ]]; then
	echo "Error in newday.sh: bad cmd args"
	echo ""
	echo "Usage:"
	echo ""
	echo "	newday.sh 1"
	echo ""
	exit -1
fi

mkdir -p $1

cp template.f90 $1/main.f90

echo "Run these commands:"
echo ""
echo "cd $1"
echo "../../build.sh && time ./main"

## I'm now using console vi in tmux instead of gvim in multiple windows
#gvim $1/test-input.txt $1/main.f90 &

