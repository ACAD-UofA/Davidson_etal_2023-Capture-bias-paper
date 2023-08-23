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
labels <- c("Pampas","South Argentina","Bolivia","Brazil","Central Chile","South Chile","North Chile","North Argentina","Peru",
            "Portugal","Spain","Gibraltar","Sicily","Basque","France","Italy")
colours34 <- c("#AC37B2","#865CD8","#F2C74B","#669F1A","#407DB1","#2238B6","#F09035","#D96443","#BF7932",
               "grey80","grey40","black")

#PLOT A - EMU
fn = "/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_big_11.eigenvecs"
A_Dat1 = read.table(fn, col.names=c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")) #smartpca output
ind = "/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_big_11.ind"
indDat1 = read.table(ind, col.names=c("Sample","Sex","group"))
A_Dat1 = cbind(A_Dat1, indDat1)

A_Dat1 <- left_join(A_Dat1,lbl, by=c("Sample"="ID"))

#relabel
A_Dat1$Populations <- factor(A_Dat1$AP_label, levels=c("Argentina_Pampas","Argentina_Patagonia","Bolivia","Brazil","CentralChile",
                                                       "Chile_Patagonia","NorthChile","NorthArgentina","Peru", 
                                                      "Portugal","Spain","Gibraltar","Sicily","Basque","France","Italy"))
#ggplot 1v2
PC1_2_typeA <- 
  ggplot(A_Dat1, aes(y = PC2, x = PC1, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20","grey30","grey40")) +
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
  guides(fill=guide_legend(nrow =3)) + 
  scale_fill_manual(name="Populations", values = colours34) +
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
  guides(fill=guide_legend(nrow =3)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(name="Population",labels=labels, values = colours34) +
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
fn = "/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_big_11.8.0.pca.evec.txt"
B_Dat1 = read.table(fn, col.names=c("Sample","PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10","group")) #smartpca output
B_Dat1 <- left_join(B_Dat1,lbl, by=c("Sample"="ID"))

#relabel
B_Dat1$Populations <- factor(B_Dat1$AP_label, levels=c("Argentina_Pampas","Argentina_Patagonia","Bolivia","Brazil","CentralChile",
                                                       "Chile_Patagonia","NorthChile","NorthArgentina","Peru", 
                                                       "Portugal","Spain","Gibraltar"))
#ggplot 1v2
PC1_2_typeB <- 
  ggplot(B_Dat1, aes(y = PC2, x = PC1, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20","grey30","grey40")) +
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
  guides(fill=guide_legend(nrow =3)) + 
  scale_fill_manual(name="Populations", values = colours34) +
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
  guides(fill=guide_legend(nrow =3)) + 
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


#### PLOT E - EMU South America ####
fn = "/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_SA_3.eigenvecs"
E_Dat1 = read.table(fn, col.names=c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")) #smartpca output
ind = "/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_SA_3.ind"
indDat1 = read.table(ind, col.names=c("Sample","Sex","group"))
E_Dat1 = cbind(E_Dat1, indDat1)

E_Dat1 <- left_join(E_Dat1,lbl, by=c("Sample"="ID"))

#relabel
E_Dat1$Populations <- factor(E_Dat1$AP_label, levels=c("Argentina_Pampas","Argentina_Patagonia","Bolivia","Brazil","CentralChile",
                                                       "Chile_Patagonia","NorthChile","NorthArgentina","Peru"))
#ggplot 1v2
PC1_2_typeE <- 
  ggplot(E_Dat1, aes(y = PC2, x = PC1, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20")) +
  theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank())  #remove labels on right y-axis
PC1_2_typeE #show plot

PC1_2_regionE <- 
  ggplot(E_Dat1, aes(x = PC1, y = PC2, fill = Populations)) + #each species is represented by a different shape
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
PC1_2_regionE #show plot
#ggplot 1v3
PC1_3_typeE <- 
  ggplot(E_Dat1, aes(x = PC1, y = PC3, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
  guides(fill=guide_legend(nrow =3)) + 
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
PC1_3_typeE #show plot

PC1_3_regionE <- 
  ggplot(E_Dat1, aes(x = PC1, y = PC3, fill = Populations)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(nrow =3)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(name="Population",labels=labels, values = colours34) +
  theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_3_regionE #show plot

#panel
PC1_2_region_patch <- PC1_2_regionE + theme(legend.position="none", axis.title.x.bottom = element_blank(), 
                                            axis.text.x.bottom=element_blank()) + 
  labs(title = "EMU")
PC1_3_region_patch <- PC1_3_regionE + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"))
PC1_2_type_patch <- PC1_2_typeE + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", axis.title.x.bottom = element_blank(), axis.text.x.bottom=element_blank(),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank()) #move legend
PC1_3_type_patch <- PC1_3_typeE + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank())

patchE <- (PC1_2_region_patch/PC1_3_region_patch) + plot_layout(heights = c(9,9)) | (PC1_2_type_patch/PC1_3_type_patch) + 
  plot_layout(heights = c(9,9))
patchE

#### F - smartPCA South America ####
fn = "/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_SA_3.8.0.pca.evec.txt"
F_Dat1 = read.table(fn, col.names=c("Sample","PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10","group")) #smartpca output
F_Dat1 <- left_join(F_Dat1,lbl, by=c("Sample"="ID"))

#relabel
F_Dat1$Populations <- factor(F_Dat1$AP_label, levels=c("Argentina_Pampas","Argentina_Patagonia","Bolivia","Brazil","CentralChile",
                                                       "Chile_Patagonia","NorthChile","NorthArgentina","Peru"))
#ggplot 1v2
PC1_2_typeF <- 
  ggplot(F_Dat1, aes(y = PC2, x = PC1, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20","grey30","grey40")) +
  theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank())  #remove labels on right y-axis
PC1_2_typeF #show plot

PC1_2_regionF <- 
  ggplot(F_Dat1, aes(x = PC1, y = PC2, fill = Populations)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_fill_manual(name="Populations", values = colours34) +
  theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_2_regionF #show plot
#ggplot 1v3
PC1_3_typeF <- 
  ggplot(F_Dat1, aes(x = PC1, y = PC3, fill = Type, shape = Type)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5) + #customised scatter plot 
  guides(fill=guide_legend(nrow =3)) + 
  scale_shape_manual(values = c(21,22,23,24,25), labels = c("1240k","PP","Shotgun")) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20","grey30","grey40"), labels = c("1240k","PP","Shotgun")) +
  theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_3_typeF #show plot

PC1_3_regionF <- 
  ggplot(F_Dat1, aes(x = PC1, y = PC3, fill = Populations)) + #each species is represented by a different shape
  geom_point(size = 3, alpha=1, stroke = 0.5, shape = 21) + #customised scatter plot 
  guides(fill=guide_legend(nrow =3)) + 
  scale_shape_manual(values = c(21,22,23,24,25)) +
  scale_fill_manual(name="Population",labels=labels, values = colours34) +
   theme_bw() + #light theme with no grey background and with grey lines and axes
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_3_regionF #show plot

#panel
PC1_2_region_patch <- PC1_2_regionF + theme(legend.position="none", axis.title.x.bottom = element_blank(), 
                                            axis.text.x.bottom=element_blank()) + 
  labs(title = "smartPCA without projection")
PC1_3_region_patch <- PC1_3_regionF + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"))
PC1_2_type_patch <- PC1_2_typeF + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", axis.title.x.bottom = element_blank(), axis.text.x.bottom=element_blank(),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank()) #move legend
PC1_3_type_patch <- PC1_3_typeF + #prepare a version of gg3 for patchwork design
  theme(legend.position="none", legend.background = element_rect(linewidth=0.3, linetype="solid",colour ="darkgrey"),
        axis.title.y.left = element_blank(), axis.text.y.left=element_blank())

patchF <- (PC1_2_region_patch/PC1_3_region_patch) + plot_layout(heights = c(9,9)) | (PC1_2_type_patch/PC1_3_type_patch) + 
  plot_layout(heights = c(9,9))
patchF

#### MEGA ####
region <- as.ggplot(get_legend(PC1_3_regionA)) 
region
type <- as.ggplot(get_legend(PC1_3_typeF))
type
patch <- (patchE/patchA/region) + plot_layout(heights = c(9,9,1.5)) | (patchF/patchB/type) + 
  plot_layout(heights = c(9,9,1.5)) 
patch <- patch +
  plot_annotation(tag_levels = list(c('A','','','','C','','','','','B','','','','D'))) 
patch


ggsave("Fig2.pdf", width = 14, height = 15, units= "in", dpi =800)
