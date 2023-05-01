library(ggplot2)
library(plotly)
library(ggrepel)
library(tidyverse)
library(ggtext)
library(patchwork)
library(seecolor)

setwd("/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter") #set working directory

fn = "/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_big_9.miss.pca.evec.txt"
#evecDat1 = read.table(fn, col.names=c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")) #emu output
evecDat1 = read.table(fn, col.names=c("Sample","PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10","group")) #smartpca output
#ind = "/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_SA_2.ind"
#indDat1 = read.table(ind, col.names=c("Sample","Sex","Pop"))
freq = "/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_big_9.imiss"
freqdat = read.table(freq, header = T)

#ind = "/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_big_6.fam"
#indDat1 = read.table(ind, col.names=c("a","Sample","b","c","d","e"))
#evecDat1 = cbind(evecDat1, indDat1)
evecDat1 <- left_join(evecDat1,freqdat, by=c("Sample"="IID"))

anno = readxl::read_excel("/Users/robertadavidson/Box Sync/Robbi_PhD/LABELS.xls", sheet = "v54.1_1240K_public", col_names = TRUE)

lbl = readxl::read_excel("/Users/robertadavidson/Box Sync/Robbi_PhD/LABELS.xls", sheet = "SA_PCA_labels", col_names = TRUE)

#datatypes = "/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/v50.0_1240k_public.datatypes.tsv"
#datatypes1 = read.table(datatypes, col.names=c("ID","Type"))

evecDat1 <- left_join(evecDat1,lbl, by=c("Sample"="ID")) %>%
  mutate(SNP_COV = F_MISS*100)
#evecDat1 <- left_join(evecDat1,datatypes1, by=c("Sample"="ID"))

## missingness
#miss = read.table("/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_big_8.imiss", header = T)
#evecDat1 <- left_join(evecDat1,miss, by=c("Sample"="IID"))


#relabel
evecDat1$Populations <- factor(evecDat1$AP_label, levels=c("Argentina_Pampas","Argentina_Patagonia","Bolivia","Brazil","CentralChile",
                                                              "Chile_Patagonia","NorthArgentina","NorthChile","Peru", 
                                                           "Portugal","Spain","Gibraltar","Sicily","Basque","France","Italy"))

labels <- c("Pampas","South Argentina","Bolivia","Brazil","Central Chile","South Chile","North Argentina","North Chile","Peru",
            "Portugal","Spain","Gibraltar","Sicily","Basque","France","Italy")

colours34 <- c("#AC37B2","#865CD8","#F2C74B","#669F1A","#407DB1","#2238B6","#D96443","#F09035","#BF7932",
               "grey50","grey80","black","burlywood1","burlywood3", "darkslategray4","darkseagreen1")
seecolor::print_color(colours34)

#count number of each data type
table(evecDat1$Type)

#ggplot 1v2
PC1_2_type <- 
  ggplot(evecDat1, aes(y = PC2, x = PC1, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
  #geom_jitter(size = 3, alpha=1, stroke = 0.5, width=0.1) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20","grey30","grey40")) +
  #geom_text_repel(data = subset(evecDat1, AP_label=="Spain"), aes(label=Sample,y = PC1, x =PC2), segment.size  = 0.2, 
  #segment.color = "grey80", colour="black", force=4, show.legend = FALSE, max.overlaps = Inf, size=2) +
  theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank())  #remove labels on right y-axis
PC1_2_type #show plot

#ggplot(evecDat1, aes(PC1,PC2, fill=F_MISS)) + geom_point(shape=21, size = 4)


PC1_2_region <- 
  ggplot(evecDat1, aes(x = PC1, y = PC2)) + #each species is represented by a different shape
  geom_point(aes(fill=(1-F_MISS)*100), size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  #geom_jitter(size = 3, alpha=1, stroke = 0.5, width=0.1) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  #scale_shape_manual(values = c(21,22,23,24,25)) +
 # scale_fill_manual(name="Populations", values = colours34) +
  scale_fill_gradient(guide = "colourbar", name="% SNPs covered", low = "yellow", high = "purple") +
  # geom_text_repel(data = subset(evecDat1, AP_label=="NorthArgentina"), aes(label=Sample,y = PC1, x = PC2), segment.size  = 0.2, 
 #                  segment.color = "grey80", colour="black", force=4, show.legend = FALSE, max.overlaps = Inf, size=2) +
  theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_2_region #show plot
#ggplotly(PC1_2_region)
#ggplot 1v3
PC1_3_type <- 
  ggplot(evecDat1, aes(x = PC1, y = PC3, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25), labels = c("1240k","PP","Shotgun")) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20"), labels = c("1240k","PP","Shotgun")) +
  theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_3_type #show plot

PC1_3_region <- 
  ggplot(evecDat1, aes(x = PC1, y = PC3, fill = (1-F_MISS)*100)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  # scale_color_manual(values = c("white","grey","black")) +
#  scale_fill_manual(name="Population",labels=labels, values = colours34) +
  scale_fill_gradient(guide = "colourbar", name="% SNPs covered", low = "yellow", high = "purple") +
  # geom_text_repel(data = subset(evecDat1, MacroRegion=="Chonos"), aes(label=Sample), segment.size  = 0.2, 
  #                 segment.color = "grey80", colour="black", force=4, show.legend = FALSE, max.overlaps = Inf, size=4) +
  theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_3_region #show plot

#panel
PC1_2_region_patch <- PC1_2_region + theme(legend.position="none", axis.title.x.bottom = element_blank(), 
                                           axis.text.x.bottom=element_blank()) + 
  labs(title = "smartPCA missingness")
PC1_3_region_patch <- PC1_3_region + #prepare a version of gg3 for patchwork design
  theme(legend.position="bottom", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey")) +
  guides(fill=guide_legend(nrow =3)) + guides(colour=guide_legend(nrow =3)) + guides(shape=guide_legend(nrow =3)) 

PC1_2_type_patch <- PC1_2_type + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", axis.title.x.bottom = element_blank(), axis.text.x.bottom=element_blank(),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank()) #move legend
PC1_3_type_patch <- PC1_3_type + #prepare a version of gg3 for patchwork design
  theme(legend.position="bottom", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank())

patch <- PC1_2_region_patch + PC1_2_type_patch + PC1_3_region_patch  + PC1_3_type_patch
patch 

ggsave("AP_missingness_smartPCA_2.pdf", width = 10, height = 10, dpi =300)
