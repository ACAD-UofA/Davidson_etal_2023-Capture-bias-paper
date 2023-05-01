#!/bin/bash

in1=$1

#first run smartPCA and output snpweights

#list SNPs from PC2 weighted over |1|
awk '{ if ($5 >= 1 || $5 <= -1) print $1}' ${in1}.pca.snpweight.txt > ${in1}.PC2_snpwgt_over1.txt

#filter from original dataset
convertf -p <(echo "genotypename:	${in1}.geno
snpname:	${in1}.snp
indivname:	${in1}.ind
outputformat:	EIGENSTRAT
genotypeoutname:	${in1}_PC2filter1.geno
snpoutname:	${in1}_PC2filter1.snp
indivoutname:	${in1}_PC2filter1.ind
badsnpname:	${in1}.PC2_snpwgt_over1.txt")
