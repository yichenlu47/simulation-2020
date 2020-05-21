#!/bin/sh

# Executable commands

#cor
 for in1 in 0.5 0.7 0.9
# for in1 in 0.5
do

# beta
# for in2 in 1 10 30 60
for in2 in 1
do

echo "This script has just run another script." 

qsub -cwd ./bin-toy.sh $in1 $in2

done
done


