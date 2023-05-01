#!/bin/bash

ml plink 

in1=$1

#first remove totally missing genotypes
plink --bfile ${in1} --keep-allele-order --allow-no-sex --make-founders --geno 0.9999999 --mind 1.0 --make-bed --out ${in1}_1

#count number of each type of SNP in dataset
awk '{print $5,$6}' ${in1}_1.bim | sort | uniq -c > ${in1}_count.txt

#allele frq counts for whole dataset
plink --bfile ${in1}_1 --keep-allele-order --allow-no-sex --make-founders --freq counts --out ${in1}_1
rm *nosex

#write "A1/A2" column
awk '{OFS="\t"} {print $1,$2,$3"/"$4,$3,$4,$5,$6,$7}' ${in1}_1.frq.counts > ${in1}_2.frq.counts

# fix allele orders for pairing types
awk '{OFS="\t"} {if ($3=="G/A") print $1,$2,$5"/"$4,$5,$4,$7,$6,$8; else print $0}' ${in1}_2.frq.counts > ${in1}_3.frq.counts 
awk '{OFS="\t"} {if ($3=="G/C") print $1,$2,$5"/"$4,$5,$4,$7,$6,$8; else print $0}' ${in1}_3.frq.counts > ${in1}_4.frq.counts
awk '{OFS="\t"} {if ($3=="G/T") print $1,$2,$5"/"$4,$5,$4,$7,$6,$8; else print $0}' ${in1}_4.frq.counts > ${in1}_5.frq.counts
awk '{OFS="\t"} {if ($3=="C/T") print $1,$2,$5"/"$4,$5,$4,$7,$6,$8; else print $0}' ${in1}_5.frq.counts > ${in1}_6.frq.counts
awk '{OFS="\t"} {if ($3=="C/A") print $1,$2,$5"/"$4,$5,$4,$7,$6,$8; else print $0}' ${in1}_6.frq.counts > ${in1}_7.frq.counts
awk '{OFS="\t"} {if ($3=="T/A") print $1,$2,$5"/"$4,$5,$4,$7,$6,$8; else print $0}' ${in1}_7.frq.counts > ${in1}_8.frq.counts

mv ${in1}_8.frq.counts ${in1}_fixed.frq.counts
rm ${in1}_2.frq.counts ${in1}_3.frq.counts ${in1}_4.frq.counts ${in1}_5.frq.counts ${in1}_6.frq.counts ${in1}_7.frq.counts

#rename A1/A2 column to pair
sed -i 's&A1/A2&pair&g' ${in1}_fixed.frq.counts
#rename SNP column to CHR_POS
sed -i 's/SNP/CHR_POS/g' ${in1}_fixed.frq.counts

#write position column in final file
sed 's/_/ /g' ${in1}_fixed.frq.counts | awk '{print $1,$1"_"$3,$3,$4,$5,$6,$7,$8,$9}' > ${in1}_fixed.2.frq.counts
mv  ${in1}_fixed.2.frq.counts ${in1}_fixed.frq.counts
