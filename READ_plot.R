library(tidyverse)
library(reshape2)
library(MASS)
library(reshape) 
library(plotly)
library(RColorBrewer)
library(paletteer)

setwd("/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/READ")
#get data
fn="/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/READ/meansP0_AncientDNA_normalized2"
READdat=read.table(fn, header=TRUE)
ffn="/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/READ/SNP_counts_pairs.txt"
SNPdat=read.table(ffn, header=TRUE)

#merge
READdat <- full_join(READdat,SNPdat, by=c("PairIndividuals"="PairIndividuals")) %>% 
  mutate(P0_norm = NonNormalizedP0*(SNPs/min(SNPs)))

#order by P0
READdatOrdered <- arrange(READdat,NonNormalizedP0)
#READdatOrdered <- arrange(READdat, SNPs)
#set pop order as factor so ggplot won't re-order automatically
READdatOrdered$PseudoPairNames <- factor(READdatOrdered$PseudoPairNames, levels = READdatOrdered$PseudoPairNames) 
#READdatOrdered <- filter(READdatOrdered, PseudoPair!="A-A", PseudoPair!="B-B", PseudoPair!="C-C", PseudoPair!="D-D")
READdatOrdered$PseudoPair <- factor(READdatOrdered$PseudoPair, levels = c("A-A","B-B","C-C","D-D","A-B","B-C","B-D","A-D","A-C","C-D"))

#ggplot
plot <- ggplot(READdatOrdered, aes(x=PseudoPair, y=NonNormalizedP0, fill = Type)) +
  geom_errorbar(aes(ymin=NonNormalizedP0-2*StandardError, ymax = NonNormalizedP0+2*StandardError), width = 0,
                 position=position_dodge(width=0.3)) +
 # geom_text(data=READdatOrdered, aes(label=scales::comma(SNPs), y=NonNormalizedP0, x=PseudoPair), nudge_x = 0.4, size=3, 
 #           check_overlap = T, angle=0, max.overlaps = Inf) +
  geom_point(aes(y=NonNormalizedP0), size = 4, shape=21, colour="black", position=position_dodge(width=0.3)) +
  scale_fill_manual(values=c("limegreen","grey","grey20"))  +
  labs(y= 'Average Pairwise P0 (± 2SE)', x='Individual Pair') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.text = element_text(size = 11), legend.text = element_text(size=11),
        legend.position=c(0.9,0.15), legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"),
        axis.title = element_text(size=12))
plot

ggsave("FigS3.pdf", width = 10, height = 6, dpi=300) #save in pdf format with size 12 x 6 in
