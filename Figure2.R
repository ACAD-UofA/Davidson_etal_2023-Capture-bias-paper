library(ggplot2)
library(plotly)
library(ggrepel)
library(tidyverse)
library(ggtext)
library(patchwork)
library(seecolor)
library(ggplotify)
library(cowplot)

anno = readxl::read_excel("./LABELS.xls", sheet = "v54.1_1240K_public", col_names = TRUE)
lbl = readxl::read_excel("./LABELS.xls", sheet = "SA_PCA_labels", col_names = TRUE)

#for all plots
labels <- c("Pampas","South Argentina","Bolivia","Brazil","Central Chile","South Chile","North Argentina","North Chile","Peru",
            "Portugal","Spain","Gibraltar","Sicily","Basque","France","Italy")
colours34 <- c("#AC37B2","#865CD8","#F2C74B","#669F1A","#407DB1","#2238B6","#D96443","#F09035","#BF7932",
               "grey50","grey80","black","burlywood1","burlywood3", "darkslategray4","darkseagreen1")

#PLOT A - EMU
fn = "./AP_big_9.eigenvecs"
A_Dat1 = read.table(fn, col.names=c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")) 
ind = "./AP_big_9.ind"
indDat1 = read.table(ind, col.names=c("Sample","Sex","group"))
A_Dat1 = cbind(A_Dat1, indDat1)

A_Dat1 <- left_join(A_Dat1,lbl, by=c("Sample"="ID"))

#relabel
A_Dat1$Populations <- factor(A_Dat1$AP_label, levels=c("Argentina_Pampas","Argentina_Patagonia","Bolivia","Brazil","CentralChile",
                                                       "Chile_Patagonia","NorthArgentina","NorthChile","Peru", 
                                                       "Portugal","Spain","Gibraltar","Sicily","Basque","France","Italy"))
#ggplot 1v2
PC1_2_typeA <- 
  ggplot(A_Dat1, aes(y = PC2, x = PC1, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
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
PC1_2_typeA #show plot

PC1_2_regionA <- 
  ggplot(A_Dat1, aes(x = PC1, y = PC2, fill = Populations)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_fill_manual(name="Populations", values = colours34) +
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
PC1_2_regionA #show plot
#ggplot 1v3
PC1_3_typeA <- 
  ggplot(A_Dat1, aes(x = PC1, y = PC3, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25), labels = c("1240k, n=223","PP, n=125","Shotgun, n=38","Shotgun.diploid","Unknown")) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20","grey30","grey40"), labels = c("1240k, n=223","PP, n=125","Shotgun, n=38","Shotgun.diploid","Unknown")) +
  theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_3_typeA #show plot

PC1_3_regionA <- 
  ggplot(A_Dat1, aes(x = PC1, y = PC3, fill = Populations)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(name="Population",labels=labels, values = colours34) +
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
PC1_3_regionA #show plot


#panel
PC1_2_region_patch <- PC1_2_regionA + theme(legend.position="none", axis.title.x.bottom = element_blank(), 
                                            axis.text.x.bottom=element_blank()) + 
  labs(title = "EMU")
PC1_3_region_patch <- PC1_3_regionA + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"))
PC1_2_type_patch <- PC1_2_typeA + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", axis.title.x.bottom = element_blank(), axis.text.x.bottom=element_blank(),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank()) #move legend
PC1_3_type_patch <- PC1_3_typeA + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank())

patchA <- PC1_2_region_patch + PC1_2_type_patch + PC1_3_region_patch  + PC1_3_type_patch
patchA
#### B - smartPCA ####
fn = "./AP_big_9.pca.evec.txt"
B_Dat1 = read.table(fn, col.names=c("Sample","PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10","group")) 
B_Dat1 <- left_join(B_Dat1,lbl, by=c("Sample"="ID"))

#relabel
B_Dat1$Populations <- factor(B_Dat1$AP_label, levels=c("Argentina_Pampas","Argentina_Patagonia","Bolivia","Brazil","CentralChile",
                                                       "Chile_Patagonia","NorthArgentina","NorthChile","Peru", 
                                                       "Portugal","Spain","Gibraltar","Sicily","Basque","France","Italy"))
#ggplot 1v2
PC1_2_typeB <- 
  ggplot(B_Dat1, aes(y = PC2, x = PC1, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
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
PC1_2_typeB #show plot

PC1_2_regionB <- 
  ggplot(B_Dat1, aes(x = PC1, y = PC2, fill = Populations)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_fill_manual(name="Populations", values = colours34) +
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
PC1_2_regionB #show plot
#ggplot 1v3
PC1_3_typeB <- 
  ggplot(B_Dat1, aes(x = PC1, y = PC3, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25), labels = c("1240k, n=223","PP, n=125","Shotgun, n=38","Shotgun.diploid","Unknown")) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20","grey30","grey40"), labels = c("1240k, n=223","PP, n=125","Shotgun, n=38","Shotgun.diploid","Unknown")) +
  theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_3_typeB #show plot

PC1_3_regionB <- 
  ggplot(B_Dat1, aes(x = PC1, y = PC3, fill = Populations)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(name="Population",labels=labels, values = colours34) +
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
PC1_3_regionB #show plot


#panel
PC1_2_region_patch <- PC1_2_regionB + theme(legend.position="none", axis.title.x.bottom = element_blank(), 
                                            axis.text.x.bottom=element_blank()) + 
  labs(title = "smartPCA without projection")
PC1_3_region_patch <- PC1_3_regionB + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"))
PC1_2_type_patch <- PC1_2_typeB + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", axis.title.x.bottom = element_blank(), axis.text.x.bottom=element_blank(),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank()) #move legend
PC1_3_type_patch <- PC1_3_typeB + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank())

patchB <- PC1_2_region_patch + PC1_2_type_patch + PC1_3_region_patch  + PC1_3_type_patch
patchB 

####C - smartPCA filter ####
fn = "./AP_big_9_PC2filter1.pca.evec.txt"
C_Dat1 = read.table(fn, col.names=c("Sample","PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10","group")) #smartpca output
C_Dat1 <- left_join(C_Dat1,lbl, by=c("Sample"="ID"))

#relabel
C_Dat1$Populations <- factor(C_Dat1$AP_label, levels=c("Argentina_Pampas","Argentina_Patagonia","Bolivia","Brazil","CentralChile",
                                                       "Chile_Patagonia","NorthArgentina","NorthChile","Peru", 
                                                       "Portugal","Spain","Gibraltar","Sicily","Basque","France","Italy"))
#ggplot 1v2
PC1_2_typeC <- 
  ggplot(C_Dat1, aes(y = PC2, x = PC1, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
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
PC1_2_typeC #show plot

PC1_2_regionC <- 
  ggplot(C_Dat1, aes(x = PC1, y = PC2, fill = Populations)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_fill_manual(name="Populations", values = colours34) +
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
PC1_2_regionC #show plot
#ggplot 1v3
PC1_3_typeC <- 
  ggplot(C_Dat1, aes(x = PC1, y = PC3, fill = Type, shape = Type)) + #each species is represented by a different shape
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
PC1_3_typeC #show plot

PC1_3_regionC <- 
  ggplot(C_Dat1, aes(x = PC1, y = PC3, fill = Populations)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(nrow =3)) + 
  scale_fill_manual(name="Region",labels=labels, values = colours34) +
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
        axis.ticks = element_blank(), legend.position = "right") 
PC1_3_regionC #show plot


#panel
PC1_2_region_patch <- PC1_2_regionC + theme(legend.position="none", axis.title.x.bottom = element_blank(), 
                                            axis.text.x.bottom=element_blank()) + 
  labs(title = "smartPCA + SNP weight based filter")
PC1_3_region_patch <- PC1_3_regionC + #prepare a version of gg3 for patchwork design
  theme(legend.position="none")
PC1_2_type_patch <- PC1_2_typeC + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.box.just = "left", axis.title.x.bottom = element_blank(), axis.text.x.bottom=element_blank(),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank()) #move legend
PC1_3_type_patch <- PC1_3_typeC + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank())

patchC <- PC1_2_region_patch + PC1_2_type_patch + PC1_3_region_patch  + PC1_3_type_patch
patchC 

####D - smartPCA projection####
fn = "./AP_big_9.projectPP.pca.evec.txt"
D_Dat1 = read.table(fn, col.names=c("Sample","PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10","group")) #smartpca output
D_Dat1 <- left_join(D_Dat1,lbl, by=c("Sample"="ID"))

#relabel
D_Dat1$Populations <- factor(D_Dat1$AP_label, levels=c("Argentina_Pampas","Argentina_Patagonia","Bolivia","Brazil","CentralChile",
                                                           "Chile_Patagonia","NorthArgentina","NorthChile","Peru", 
                                                           "Portugal","Spain","Gibraltar","Sicily","Basque","France","Italy"))
#ggplot 1v2
PC1_2_typeD <- 
  ggplot(D_Dat1, aes(y = PC2, x = PC1, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
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
PC1_2_typeD #show plot

PC1_2_regionD <- 
  ggplot(D_Dat1, aes(x = PC1, y = PC2, fill = Populations)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_fill_manual(name="Region", values = colours34) +
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
PC1_2_regionD #show plot
#ggplot 1v3
PC1_3_typeD <- 
  ggplot(D_Dat1, aes(x = PC1, y = PC3, fill = Type, shape = Type)) + #each species is represented by a different shape
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
PC1_3_typeD #show plot

PC1_3_regionD <- 
  ggplot(D_Dat1, aes(x = PC1, y = PC3, fill = Populations)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(name="Population",labels=labels, values = colours34) +
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
PC1_3_regionD #show plot


#panel
PC1_2_region_patch <- PC1_2_regionD + theme(legend.position="none", axis.title.x.bottom = element_blank(), 
                                           axis.text.x.bottom=element_blank()) + 
  labs(title = "smartPCA with Prime Plus projected")
PC1_3_region_patch <- PC1_3_regionD + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"))
PC1_2_type_patch <- PC1_2_typeD + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", axis.title.x.bottom = element_blank(), axis.text.x.bottom=element_blank(),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank()) #move legend
PC1_3_type_patch <- PC1_3_typeD + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank())

patchD <- PC1_2_region_patch + PC1_2_type_patch + PC1_3_region_patch  + PC1_3_type_patch
patchD 

#### MEGA ####
region <- as.ggplot(get_legend(PC1_3_regionC)) 
region
type <- as.ggplot(get_legend(PC1_3_typeD))
type
patch <- (patchA/patchC/region) + plot_layout(heights = c(9,9,1.5)) | (patchB/patchD/type) + 
  plot_layout(heights = c(9,9,1.5)) 
patch <- patch +
  plot_annotation(tag_levels = list(c('A','','','','C','','','','','B','','','','D'))) 
patch
ggsave("Fig2.pdf", width = 14, height = 15, dpi =300)
