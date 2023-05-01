#!/bin/bash

in1=$1
in2=poplist.txt

## NB: Use "outliermode" when calculating PCA from missingness. 
## Use "poplistname" when projecting


smartpca -p <(echo "genotypename:	${in1}.geno
snpname:	${in1}.snp
indivname:	${in1}.ind
evecoutname:	${in1}.pca.evec.txt
evaloutname:	${in1}.pca.eval.txt
poplistname:	${in2}
#missingmode:	YES
lsproject:	YES
outliermode:	2
outlieroutname:	${in1}.pca.outlier.txt
snpweightoutname:	${in1}.pca.snpweight.txt
numthreads:	4
newshrink:	YES
maxpops:	500
numoutevec:	10")
