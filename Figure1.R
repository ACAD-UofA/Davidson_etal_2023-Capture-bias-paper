library(tidyverse)
library(patchwork)

f3dat = read.table("./AP_ancient_all_2.R.qp3Pop.out", col.names=c("PopB", "PopA", "PopC", "F3", "StdErr", "Z", "SNPs"))

lbl = readxl::read_excel("./LABELS.xls", sheet = "SA_PCA_labels", col_names = TRUE)
lbl = select(lbl, AP_label, Type) %>%
  mutate(f3group = paste(AP_label,Type, sep="_"))

#join labels
f3dat <- inner_join(f3dat,lbl, by=c("PopB"="f3group"), relationship = "many-to-many") %>% 
  unique() 

f3dat$Populations <- factor(f3dat$AP_label, levels = c("Spain","Argentina_Pampas",
                                                       "Argentina_Patagonia", "Bolivia","Brazil",            
                                                       "CentralChile" , "Chile_Patagonia",  
                                                       "NorthChile","Peru"))

labels <- c("Spain","Pampas","South Argentina", "Bolivia","Brazil", "Central Chile","South Chile","North Chile","Peru")

#set pop order as factor so ggplot won't re-order automatically
f3dat$Populations <- factor(f3dat$Populations, levels = f3dat$Populations)

# ggplot 
plot <- ggplot(data = f3dat) + 
  geom_point(aes(x=F3, y=Populations, fill=Type, shape=Type), #plot F3 points
             size = 4, color="black") + #set point size and colour
  geom_errorbar(aes(y=AP_label, xmin=F3-StdErr, xmax=F3+StdErr, width=0), color="black") + # add error bar
  scale_fill_manual(values=c( "tomato2","limegreen","grey20"), labels = c("1240k","PP","Shotgun")) + #adjust point fill
  scale_shape_manual(values=c(21,22,23), labels = c("1240k","PP","Shotgun")) + #adjust point shape
  scale_y_discrete(labels=labels) + #fix y axis labels
  theme_bw() + 
  theme(strip.background = element_rect(fill = "white"), 
        legend.background = element_rect(linewidth=0.2, linetype="solid", colour ="grey10"),
        axis.text = element_text(size = 11), strip.text = element_text(size = 13), 
        legend.position = c(0.9,0.11)) +
  labs(x = "" , y = "", title = "") +
  facet_grid(. ~ factor(PopA, levels = c("Spain_Arbor", "Peru_Arbor"), 
                        labels = c( "(A)           ƒ3(Mbuti; Spain.PP, X)               ", 
                                    "(B)           ƒ3(Mbuti; Peru.PP, X)                ")), scales="free_x")
plot

ggsave("Fig1.pdf", width = 10, height = 8, dpi=300) #save in pdf format
