#!/bin/sh

# Executable commands

#n.cor.1
 for in1 in 3 6 9 12 15
# for in1 in 9 18 27 36 45
# for in1 in 15 30 45 60 75
# for in1 in 15
# for in1 in 3
do

#n.cor
 for in2 in 15
# for in2 in 45
# for in2 in 75
# for in2 in 45 90 105 120
#for in2 in 3 6 9 12
do

#cor
# for in3 in 0.5 0.7 0.9
for in3 in 0.9
do

# beta
# for in4 in 1 15 30
# for in4 in 1 45 90
# for in4 in 1 75 150
# for in4 in 0.5 5 10
for in4 in 0.1 0.5 2.5 5 10
do

echo "This script has just run another script." 

qsub -cwd ./bin.sh $in1 $in2 $in3 $in4

done
done
done
done

