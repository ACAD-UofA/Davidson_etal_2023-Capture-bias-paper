library(ggplot2)
library(plotly)
library(ggrepel)
library(tidyverse)
library(ggtext)
library(patchwork)
library(seecolor)

anno = readxl::read_excel("./LABELS.xls", sheet = "v54.1_1240K_public", col_names = TRUE)
lbl = readxl::read_excel("./LABELS.xls", sheet = "SA_PCA_labels", col_names = TRUE)

#prep EMU data
fn = "./worldPCA_1.eigenvecs"
emucDat1 = read.table(fn, col.names=c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")) #emu output
ind = "./worldPCA_1.ind"
indDat1 = read.table(ind, col.names=c("Sample","Sex","Pop"))
emuDat1 = cbind(emucDat1, indDat1)

emuDat1 <- left_join(emuDat1,lbl, by=c("Sample"="ID"))
emuDat1 <- left_join(emuDat1,anno, by=c("Sample"="Genetic_ID"))

ancientemu <- filter(emuDat1, Type=="1240k" | Type=="Shotgun" |Type=="Arbor")

modernemu <- filter(emuDat1, Method_for_Date=="Modern")

table(ancientemu$AP_label)

anc <- select(ancientemu, Sample, AP_label, Type) %>%
  mutate(f3group = paste(AP_label,Type, sep="_")) 
head(anc)

#prep smartPCA data
fn = "./worldPCA_1.outlierremoval.pca.evec.txt"
evecDat1 = read.table(fn, col.names=c("Sample","PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10","group")) #smartpca output
evecDat1 <- left_join(evecDat1,lbl, by=c("Sample"="ID"))
evecDat1 <- left_join(evecDat1,anno, by=c("Sample"="Genetic_ID"))

ancientPCA <- filter(evecDat1, Type=="1240k" | Type=="Shotgun" |Type=="Arbor")

modernPCA <- filter(evecDat1, Method_for_Date=="Modern")

#relabel
ancientemu$Populations <- factor(ancientemu$AP_label, levels=c("Argentina_Pampas","Argentina_Patagonia","Bolivia","Brazil","CentralChile",
                                                               "Chile_Patagonia","NorthArgentina","NorthChile","Peru", "MesoAmerica","SWAsia",
                                                               "Portugal","Spain","Gibraltar"))
ancientPCA$Populations <- factor(ancientPCA$AP_label, levels=c("Argentina_Pampas","Argentina_Patagonia","Bolivia","Brazil","CentralChile",
                                                           "Chile_Patagonia","NorthArgentina","NorthChile","Peru", "MesoAmerica","SWAsia",
                                                           "Portugal","Spain","Gibraltar"))

labels <- c("Pampas","South Argentina","Bolivia","Brazil","Central Chile","South Chile","North Argentina","North Chile","Peru","Meso America","South West Asia",
            "Portugal","Spain","Gibraltar")

colours34 <- c("#AC37B2","#865CD8","#F2C74B","#669F1A","#407DB1","#2238B6","#D96443","#F09035","#BF7932","firebrick4","darkslategray4",
               "grey50","grey80","black")

## plot
PC1_2_regionemu <- ggplot() + 
  geom_point(data=modernemu, aes(PC1,PC2), colour="grey") +
  geom_point(data=ancientemu, aes(PC1,PC2, fill = Populations), shape = 21, size = 4, alpha=1, stroke = 0.5) + 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_fill_manual(name="Population",labels=labels, values = colours34) +
  geom_text(aes(label="Africa", x=-0.04, y=0.08), family = "Helvetica") +
  geom_text(aes(label="Europe", x=-0.025, y=-0.02), family = "Helvetica") +
  geom_text(aes(label="Americas", x=0.03, y=0.02), family = "Helvetica") +
  theme_bw() + 
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank()) 
PC1_2_regionemu #show plot

#ggplot 1v2
PC1_2_typeemu <- ggplot() + 
  geom_point(data=modernemu, aes(PC1,PC2), colour="grey") +
  geom_point(data=ancientemu, aes(PC1,PC2, fill = Type,colour=Type, shape = Type), size = 4, alpha=1, stroke = 0.3) + 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(1,4,5)) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20")) +
  scale_colour_manual(values = c("tomato2","limegreen","black")) +
  geom_text(aes(label="Africa", x=-0.04, y=0.08), family = "Helvetica") +
  geom_text(aes(label="Europe", x=-0.025, y=-0.02), family = "Helvetica") +
  geom_text(aes(label="Americas", x=0.03, y=0.02), family = "Helvetica") +
  theme_bw() + 
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + 
  scale_y_continuous(sec.axis = dup_axis()) + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), 
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank(), legend.position = c(0.8,0.8))  
PC1_2_typeemu #show plot

## plot
PC1_2_regionPCA <- ggplot() + 
  geom_point(data=modernPCA, aes(PC1,-PC2), colour="grey") +
  geom_point(data=ancientPCA, aes(PC1,-PC2, fill = Populations), shape = 21, size = 4, alpha=1, stroke = 0.5) +  
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_fill_manual(name="Population",labels=labels, values = colours34) +
  geom_text(aes(label="Africa", x=-0.09, y=0.04), family = "Helvetica") +
  geom_text(aes(label="Europe", x=0.02, y=-0.05), family = "Helvetica") +
  geom_text(aes(label="Americas", x=0.01, y=0.05), family = "Helvetica") +
  theme_bw() + 
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank())  
PC1_2_regionPCA #show plot

#ggplot 1v2
PC1_2_typePCA <- ggplot() + 
  geom_point(data=modernPCA, aes(PC1,-PC2), colour="grey") +
  geom_point(data=ancientPCA, aes(PC1,-PC2, fill = Type, colour = Type, shape = Type), size = 4, alpha=1, stroke = 0.3) + 
  guides(fill=guide_legend(ncol =1)) + guides(colour=guide_legend(ncol =1)) + guides(shape=guide_legend(ncol =1)) + 
  scale_shape_manual(values = c(1,4,5), labels=c("1240k","PP","Shotgun")) +
  scale_fill_manual(values = c("tomato2","limegreen","grey20"), labels=c("1240k","PP","Shotgun")) +
  scale_colour_manual(values = c("tomato2","limegreen","black"), labels=c("1240k","PP","Shotgun")) +
  geom_text(aes(label="Africa", x=-0.09, y=0.04), family = "Helvetica") +
  geom_text(aes(label="Europe", x=0.02, y=-0.05), family = "Helvetica") +
  geom_text(aes(label="Americas", x=0.01, y=0.05), family = "Helvetica") +
  theme_bw() + 
  geom_vline(xintercept=0, linetype = 3, colour="grey10") +
  geom_hline(yintercept=0, linetype = 3, colour="grey10") +
  scale_x_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on top 
  scale_y_continuous(sec.axis = dup_axis()) + #add duplicated secondary axis on right
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), #remove minor gridline
        axis.title.x.top = element_blank(), axis.text.x.top = element_blank(), #remove labels on top x-axis
        axis.title.y.right = element_blank(), axis.text.y.right = element_blank(), legend.text = element_markdown(),
        axis.ticks = element_blank())  
PC1_2_typePCA #show plot

#panel
PC1_2_regionemu_patch <- PC1_2_regionemu + labs(title = "EMU") + theme(legend.position = "none")
PC1_2_regionPCA_patch <- PC1_2_regionPCA + labs(title = "smartPCA with projection")
PC1_2_typeemu_patch <- PC1_2_typeemu + theme(legend.position = "none")
patch <- PC1_2_regionemu_patch + PC1_2_regionPCA_patch + PC1_2_typeemu_patch  + PC1_2_typePCA + 
  plot_layout(heights = c(5, 5), ncol =2, nrow=2)
patch 

ggsave("Fig3.pdf", width = 13, height = 10, dpi =300)
