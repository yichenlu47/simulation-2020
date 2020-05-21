#!/bin/sh

# Executable commands

#n.cor.1
#for in1 in 1 5 10 15 20
for in1 in 1 3 6 9 12 15
do

#n.cor
#for in2 in 20
for in2 in 15
do

#cor
for in3 in 0.9
do

# beta
for in4 in 1
do

qsub -cwd ./main-bin.sh $in1 $in2 $in3 $in4

done
done
done
done

