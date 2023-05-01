#!/bin/bash

conda activate sequencetools
ml SAMtools/1.8

map=$PATH/mapping_resources

### GENOTYPE
## pseudo haploid genotype dbSNP 138 
samtools mpileup -R -B -q30 -Q30 --positions ${map}/dbsnp_138.b37/dbsnp_138.b37.pos \
        --fasta-ref ${map}/GRCh37/human_g1k_v37_decoy.fasta \
        --bam-list bamlist.txt | pileupCaller --sampleNameFile newSamplelist.txt \
	--snpFile ${map}/dbsnp_138.b37/dbsnp_138.b37.snp \
	--randomHaploid \
	--samplePopName U_53M \
	--eigenstratOut AP_53M

rename ".txt" "" AP_53M*

### OFF TARGET
#filter out prime plus sites for off-target
ml plink/1.90beta-4.4-21-May
plink --bfile AP_53M \
	--keep-allele-order \
	--allow-no-sex \
	--mind 1.0 \
	--geno 0.9999999 \
	--exclude PrimePlus_1240k_Y46k_sorted.sites \
	--make-bed --out AP_53M_OffTargetOnly
rm AP_53M.nosex

### OUT OF RANGE
# Out of range genotype filter
in1=$1

#find SNPs in 100bp range of target site
awk '{print $2,$4-100,$4+100,$1}' ${map}/PrimePlus_1240k_Y46k_sorted.snp > PP.100bp.set

#extract/exclude with --set
plink --bfile AP_53M --exclude range PP.100bp.set --keep-allele-order --allow-no-sex --make-bed --out AP_53M_OTrange
