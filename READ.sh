#!/bin/bash

DATA=$1
#Plink 1.9
ml plink

#Prepare dataset
plink --bfile ${DATA} --keep-allele-order --maf 0.01 --geno 0.999999 --mind 1.0 --allow-no-sex --recode transpose --out ${DATA}

rm ${DATA}.nosex

awk '{print $1,$2"__",$3,$4,$5,$6}' ${DATA}.tfam > ${DATA}.2.tfam

mv ${DATA}.2.tfam ${DATA}.tfam

# Run READ
python2 $PATH/READ.py ${DATA}

# Count SNPs per pair
pairs = $(awk '{print $1}' READ_output_ordered | sort | uniq)

for i in pairs; do
	grep "${i}" READ_output_ordered > ${i}_READ_output_ordered
done

awk '{s+=$4} END {print s}' *_READ_output_ordered > READ_snps_per_pair  
