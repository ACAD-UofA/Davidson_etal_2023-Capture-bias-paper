#!/bin/bash

conda activate sequencetools
ml SAMtools/1.8

map=$PATH

## Prime Plus Capture 1240K plus Y46K
samtools mpileup -R -B -q20 -Q30 --positions ${map}/PrimePlus_CaptureBED/PrimePlus_1240k_Y46k_sorted.pos \
        --fasta-ref ${map}/GRCh37/human_g1k_v37_decoy.fasta \
        --bam-list bamlist.txt | pileupCaller --sampleNameFile newSamplelist.txt \
	--snpFile ${map}/PrimePlus_CaptureBED/PrimePlus_1240k_Y46k_sorted.snp \
	--minDepth 5 \
	--majorityCall \
	--samplePopName UN_MAJ \
	--eigenstratOut AP_majority
