#!/bin/sh

# Executable commands

#cor
for in1 in 0.9
do

# beta
# for in2 in 1 10
for in2 in 0.1 0.5 1 2.5 5 10
do

echo "This script has just run another script bin-oracle.sh." 

qsub -cwd ./bin-oracle.sh $in1 $in2

done
done


