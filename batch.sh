#!/bin/sh

# Executable commands

# beta
for in1 in 1
do

#cor.random
# for in2 in 0
for in2 in 1
do

#n.cor
# for in3 in 15
# for in3 in 20 30 60 90
# for in3 in 250
# for in3 in 200
for in3 in 15 30 45 60 90
do

#n.cor.1
# for in4 in 5 15 20 30 50
for in4 in 15
# for in4 in 80 100 150 200 250
# for in4 in 10 15 20 30
do

#cor
# for in5 in 0.5 0.7
for in5 in 0.9
do

#n.noise
for in6 in 300
do

qsub -cwd ./bin.sh $in1 $in2 $in3 $in4 $in5 $in6

done
done
done
done
done
done