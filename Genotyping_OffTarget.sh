#!/bin/bash

conda activate sequencetools
ml SAMtools/1.8

map=$PATH/mapping_resources

## dbSNP 138 
samtools mpileup -R -B -q30 -Q30 --positions ${map}/dbsnp_138.b37/dbsnp_138.b37.pos \
        --fasta-ref ${map}/GRCh37/human_g1k_v37_decoy.fasta \
        --bam-list bamlist.txt | pileupCaller --sampleNameFile newSamplelist.txt \
	--snpFile ${map}/dbsnp_138.b37/dbsnp_138.b37.snp \
	--randomHaploid \
	--samplePopName U_53M \
	--eigenstratOut AP_53M

rename ".txt" "" AP_53M*

#filter out prime plus sites
ml plink/1.90beta-4.4-21-May
plink --bfile AP_53M \
	--keep-allele-order \
	--allow-no-sex \
	--mind 1.0 \
	--geno 0.9999999 \
	--exclude PrimePlus_1240k_Y46k_sorted.sites \
	--make-bed --out AP_53M_OffTargetOnly

rm *nosex
