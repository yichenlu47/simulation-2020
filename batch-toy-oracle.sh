#!/bin/sh

# Executable commands

#cor
for in1 in 0.5 0.7 0.9
do

# beta
for in2 in 1 10
do

echo "This script has just run another script bin-toy-oracle.sh." 

qsub -cwd ./bin-toy-oracle.sh $in1 $in2

done
done


