#!/bin/bash

ml GSL/2.5
ml OpenBLAS/0.3.1

in1=$1

# Automatically merge with Mbuti outgroup fileset
mergeit -p <(echo "geno1: ${in1}.geno
snp1:  ${in1}.snp
ind1:  ${in1}.ind
geno2: Mbuti.geno
snp2:  Mbuti.snp
ind2:  Mbuti.ind
genooutfilename:   ${in1}_Mbuti.geno
snpoutfilename:    ${in1}_Mbuti.snp
indoutfilename:    ${in1}_Mbuti.ind
outputformat:  EIGENSTRAT
docheck: YES
strandcheck:  YES
hashcheck: NO")

# for loops to write the qp3.list file as required, one triplet per line, in order (X,Test;Outgroup)
echo "writing list file"
pops=$(awk '{print $3}' ${in1}.ind | sort | uniq)

rm ${in1}.qp3.list

for P2 in Spain_Arbor Peru_Arbor; do
	for i in ${pops}; do
		echo "${i} ${P2} Mbuti" >> ${in1}.qp3.list
	done
done

#for slurm
cat ${in1}.qp3.list

echo "done
Running qp3Pop"

#run qp3
$USER/Programs/AdmixTools/bin/qp3Pop \
-p <(echo "genotypename:	${in1}_Mbuti.geno
snpname:	${in1}_Mbuti.snp
indivname:	${in1}_Mbuti.ind
popfilename: ${in1}.qp3.list
inbreed:	YES") \
> ${in1}.qp3Pop.out

# for slurm
cat ${in1}.qp3Pop.out

#print results for R pltting
grep 'result:' ${in1}.qp3Pop.out | awk '{print $2, $3, $4, $5, $6, $7, $8, $9}' > ${in1}.R.qp3Pop.out
