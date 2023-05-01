#!/bin/bash

#load modules on hpc
module purge
ml arch/haswell
ml arch/arch/haswell
ml modulefiles/arch/haswell
ml GSL/2.5
ml OpenBLAS/0.3.1

#assign variable to dataset called in command
in1=$1

# for slurm
cat ${in1}.trees.txt

echo "done
Running f4 tests"

#Run on every quadruple combination
$USER/Programs/AdmixTools/bin/qpDstat \
-p <(echo "indivname:    ${in1}.ind  
snpname:      ${in1}.snp
genotypename: ${in1}.geno
poplistname: ${in1}.poplist.txt
popfilename: ${in1}.trees.txt
printsd:  YES
f4mode:   YES") \
> ${in1}.qpDstat_f4.out

#clean up output file for R
grep "result" ${in1}.qpDstat_f4.out | awk '{print $2,$3,$4,$5,$6,$7,$8,$9,$10,$11}' > ${in1}.qpDstat_f4.R.out
