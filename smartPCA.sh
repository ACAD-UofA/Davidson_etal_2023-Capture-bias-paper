#!/bin/bash

module load Singularity/3.10.5

in1=$1
in2=$2

#Use poplistname to define populations used in eigenvector calculation
#Use missingmode to calculate PCA from SNP missingness rather than genotypr

singularity exec <path>/eigensoft_8.0.0--h6a739c9_3.sif smartpca -p <(echo "genotypename:	${in1}.geno
snpname:	${in1}.snp
indivname:	${in1}.ind
evecoutname:	${in1}.8.0.pca.evec.txt
evaloutname:	${in1}.8.0.pca.eval.txt
poplistname:	${in2}
missingmode:	NO
lsqproject:	YES
outliermode:	1
outlieroutname:	${in1}.8.0.pca.outlier.txt
snpweightoutname:	${in1}.8.0.pca.snpweight.txt
numthreads:	4
newshrink:	YES
maxpops:	500
numoutevec:	10")
