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

mkdir $1

cp template.f90 $1/main.f90

echo "Run these commands:"
echo ""
echo "cd $1"
echo "../../build.sh && time ./main"

gvim $1/test-input.txt $1/main.f90 &

