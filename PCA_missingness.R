library(ggplot2)
library(plotly)
library(ggrepel)
library(tidyverse)
library(ggtext)
library(patchwork)
library(seecolor)

fn = "<path>/AP_big_11.miss.pca.evec.txt"
evecDat1 = read.table(fn, col.names=c("Sample","PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10","group")) #smartpca output
freq = "<path>/AP_big_9.imiss"
freqdat = read.table(freq, header = T)
evecDat1 <- left_join(evecDat1,freqdat, by=c("Sample"="IID"))
anno = readxl::read_excel("<path>/LABELS.xls", sheet = "v54.1_1240K_public", col_names = TRUE)
lbl = readxl::read_excel("<path>/LABELS.xls", sheet = "SA_PCA_labels", col_names = TRUE)
evecDat1 <- left_join(evecDat1,lbl, by=c("Sample"="ID")) %>%
  mutate(SNP_COV = F_MISS*100)

#ggplot 1v2
PC1_2_type <- 
  ggplot(evecDat1, aes(y = PC2, x = PC1, fill = Type, shape = Type)) + 
  geom_point(size = 3, alpha=1, stroke = 0.5) + 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20","grey30","grey40")) +
  theme_bw() + 
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + 
  scale_y_continuous(sec.axis = dup_axis()) + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), 
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank())  
PC1_2_type #show plot

PC1_2_region <- 
  ggplot(evecDat1, aes(x = PC1, y = PC2)) + 
  geom_point(aes(fill=(1-F_MISS)*100), size = 3, alpha=1, stroke = 0.5, shape = 21) +  
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_fill_gradient(guide = "colourbar", name="% SNPs covered", low = "yellow", high = "purple") +
  theme_bw() + 
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) +  
  scale_y_continuous(sec.axis = dup_axis()) + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), 
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_2_region #show plot

#ggplot 1v3
PC1_3_type <- 
  ggplot(evecDat1, aes(x = PC1, y = PC3, fill = Type, shape = Type)) + 
  geom_point(size = 3, alpha=1, stroke = 0.5) + 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25), labels = c("1240k","PP","Shotgun")) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20"), labels = c("1240k","PP","Shotgun")) +
  theme_bw() + 
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + 
  scale_y_continuous(sec.axis = dup_axis()) + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), 
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_3_type #show plot

PC1_3_region <- 
  ggplot(evecDat1, aes(x = PC1, y = PC3, fill = (1-F_MISS)*100)) + 
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_fill_gradient(guide = "colourbar", name="% SNPs covered", low = "yellow", high = "purple") +
  theme_bw() + 
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + 
  scale_y_continuous(sec.axis = dup_axis()) + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), 
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_3_region #show plot


#panel
PC1_2_region_patch <- PC1_2_region + theme(legend.position="none", axis.title.x.bottom = element_blank(), 
                                           axis.text.x.bottom=element_blank()) 
PC1_3_region_patch <- PC1_3_region + 
  theme(legend.position="bottom", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey")) +
  guides(fill=guide_legend(nrow =3)) + guides(colour=guide_legend(nrow =3)) + guides(shape=guide_legend(nrow =3)) 
PC1_2_type_patch <- PC1_2_type + 
  theme(legend.position="none", axis.title.x.bottom = element_blank(), axis.text.x.bottom=element_blank(),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank()) 
PC1_3_type_patch <- PC1_3_type + 
  theme(legend.position="bottom", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank())

patch <- PC1_2_type_patch + PC1_2_region_patch   + PC1_3_type_patch +  PC1_3_region_patch
patch 

ggsave("Fig_S2.pdf", width = 10, height = 10, dpi =300)
