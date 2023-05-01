library(tidyverse)
library(patchwork)
library(scales)
library(ggtext)
library(ggbreak) 
setwd("/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter")

f4dat = read.table("/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_17.qpDstat_f4.R.out",
                   col.names=c("PopA", "PopB", "PopC", "PopD","F4", "StdErr", "Z", "ABBA","BABA", "SNPs")) #,"name", "Z_score"
f4_OTdat = read.table("/Users/robertadavidson/Box Sync/Robbi_PhD/09_Arbor_Bias_Chapter/AP_OT_4.R.out",
                      col.names=c("PopA", "PopB", "PopC", "PopD","F4", "StdErr", "Z", "ABBA","BABA", "SNPs"))
f4_OTdat = f4_OTdat[1:2,]

f4dat = rbind(f4dat, f4_OTdat)
f4dat = dplyr::mutate(f4dat, absZ=abs(Z)) %>%
  mutate(test= paste(PopB,PopD)) 


f4dat = filter(f4dat, PopC!="IGNORE", PopC!="ignore", PopC!="Peru.2R_LD_200bp_r2_0.5", PopC!="Peru.2R_LD_200bp_r2_0.7", PopC!="Peru.2R_LD_200bp_r2_0.9", PopC!="Peru.2R_LD_200bp_r2_0.3",
               PopC!="Peru.2R_LD_200bp_r2_0.1", PopC!="Peru.2R_LD", PopC!="Peru.2R_LDr2_0.1", PopC!="Peru.2R_LDr2_0.3", PopC!="Peru.2R_LDr2_0.5", PopC!="Peru.2R_LDr2_0.7",
               PopC!="Peru.2R_LDr2_0.9", PopC!="Peru.1RO_MAJ", PopC!="Peru.SG_MAJ", PopC!="Peru.1RJ_MAJ", PopC!="Peru.1RJ_diploid", PopC!="Peru.1RO_diploid", 
               PopC!="Peru.2R_50k", PopC!="Peru.SG_diploid", PopC!="Peru.2R_Rohland1", PopC!="Spain_Anc")

#set pop order as factor so ggplot won't re-order automatically (order is from bottom of plot up)
f4dat$PopC <- factor(f4dat$PopC, levels = c("Peru.1RJ", 
                                            "Peru.1RO",
                                            "Peru.PP.OR",
                                            "Peru.2R_OT",
                                            "Peru.2R_MAJ",
                                            "Peru.2R_diploid",
                                            "Peru_2R_300k", 
                                            "Peru.2R_3vLDr2_0.9", 
                                            "Peru.2R_3vLDr2_0.7", 
                                            "Peru.2R_3vLDr2_0.5", 
                                            "Peru.2R_3vLDr2_0.3", 
                                            "Peru.2R_3vLDr2_0.1",
                                            "Peru.2R_maf0.01",
                                            "Peru.2R_maf0.1", 
                                            "Peru.2R_maf0.3",
                                            "Peru.2R_g0.5",
                                            "Peru.2R_g0.0", 
                                            "Peru.2R_PCAfilter",
                                            "Peru.2R_Rohland0", 
                                            "Peru.1240K_Rohland0",
                                            "Peru.2R_HO",
                                            "Peru.2R_transversion",
                                            "Peru.2R_50bp",
                                            "Spain.2R",
                                            "Spain.1240k",
                                            "Spain_Anc.SG", 
                                            "Peru_Other.SG",
                                            "Peru.2R",
                                            "Peru.1240K",
                                            "Peru.SG"
                                            ))


labels <- c("Peru.PP.1rnd.A", 
            "Peru.PP.1rnd.B",
            "Peru.PP.2rnd.>100bpOffTarget", 
            "Peru.PP.2rnd.OffTarget",
            "Peru.PP.2rnd.>5X.maj",
            "Peru.PP.2rnd.diploid",
            "Peru.PP.2rnd.300k",
            "Peru.PP.2rnd.LDr2_0.9", 
            "Peru.PP.2rnd.LDr2_0.7",
            "Peru.PP.2rnd.LDr2_0.5",
            "Peru.PP.2rnd.LDr2_0.3",
            "Peru.PP.2rnd.LDr2_0.1",
            "Peru.PP.2rnd.MAF0.01",
            "Peru.PP.2rnd.MAF0.1", 
            "Peru.PP.2rnd.MAF0.3" ,
            "Peru.PP.2rnd.miss0.5",
            "Peru.PP.2rnd.miss0.0",
            "Peru.PP.PCAfilter",
            "Peru.PP.2rnd.Rohland", 
            "Peru.1240k.Rohland", 
            "Peru.PP.2rnd.HO", 
            "Peru.PP.2rnd.transversion",
            "Peru.PP.2rnd.>50bp",
            "Spain.PP.2rnd",
            "Spain.1240k",
            "Spain.Shotgun",
            "Peru.other.SG",
            "**Peru.PP.2rnd**",
            "Peru.1240k",
            "Peru.Shotgun")
#split
f4dat_Spain_PP <- filter(f4dat, PopB=="Spain.2R")

#label facets
f4dat_Spain_PP$PopD <- factor(f4dat_Spain_PP$PopD, levels = c("Peru.SG", "Peru.1240K"),
              labels=c("f4(Mbuti, Spain.PP.2rnd; Target, Peru.Shotgun)",
                       "f4(Mbuti, Spain.PP.2rnd; Target, Peru.1240k)"))

#data frame for vertical lines
data_vline <- subset(f4dat, PopC=="Peru.2R") %>%
  select(test,F4) %>%
  rename(vline=F4) 
data_vline

# ggplot attempt
f4_plot <- ggplot(data = f4dat) + 
  geom_errorbar(aes(y=PopC, xmin=F4-2*StdErr, xmax=F4+2*StdErr), color="black", width=0.0) + # change whisker length
  # geom_rect(data=f4dat_Spain_PP, aes(ymin=-Inf, ymax= 2.6, xmin = - Inf, xmax=Inf), fill="grey70", alpha=0.5) +
  #geom_rect(data=f4dat_Spain_PP, aes(ymin=4.6, ymax= 5.6, xmin = - Inf, xmax=Inf), fill="grey90", alpha=0.5) +
  #geom_rect(data=f4dat_Spain_PP, aes(ymin=6.6, ymax= 7.6, xmin = - Inf, xmax=Inf), fill="grey90", alpha=0.5) +
  #geom_rect(data=f4dat_Spain_PP, aes(ymin=8.6, ymax= 10.6, xmin = - Inf, xmax=Inf), fill="grey90", alpha=0.5) +
  #geom_rect(data=f4dat_Spain_PP, aes(ymin=23.6, ymax= 25.6, xmin = - Inf, xmax=Inf), fill="grey90", alpha=0.5) +
  #geom_rect(data=f4dat_Spain_PP, aes(ymin=19.6, ymax= 22.6, xmin = - Inf, xmax=Inf), fill="grey90", alpha=0.5) +
  #geom_rect(data=f4dat_Spain_PP, aes(ymin=16.6, ymax= 17.6, xmin = - Inf, xmax=Inf), fill="grey90", alpha=0.5) +
  geom_hline(yintercept="Peru.2R", linetype = 3, linewidth=0.6, colour="darkred") +
  #geom_hline(yintercept=2.6, linetype = 1, colour="black", linewidth=0.2) +
  geom_vline(xintercept=0, colour="black", linewidth=0.4) +
  geom_vline(data=data_vline, aes(xintercept = vline), linewidth=0.6, linetype = 3, colour="darkred") +
  geom_point(aes(x=F4, y=PopC, fill=absZ), size = 4, shape=21, colour="black") + #set point size
  scale_fill_gradientn(name= "Z",colours=c("gold1","red3"), values=rescale(x=c(3,max(f4dat_Spain_PP$absZ)), from=c(0,max(f4dat_Spain_PP$absZ))), 
                       breaks=c(3, max(f4dat$absZ)),labels = c("<|3|","|50|"), na.value = "blue") +
  theme_bw() + #set theme
  geom_text(data=subset(f4dat, PopC!="Peru_2R_300k"), aes(label=scales::comma(SNPs), x=F4, y=PopC), nudge_x = -0.003, nudge_y = 0.4, size=3) +
  geom_text(data=subset(f4dat, PopC!="Peru_2R_300k"), aes(label=scales::comma(Z), x=F4, y=PopC), nudge_x = 0.003, nudge_y = 0.4, size=3) +
  geom_text(data=subset(f4dat, PopC!="Peru_2R_300k"), aes(label="SNPs | Z=", x=F4, y=PopC), nudge_y = 0.4, size=3) +
  scale_x_continuous(limits = c(-0.012, 0.008), breaks = c(-0.01,-0.005,0,0.005), labels = c("-0.010","-0.005","**0.000**","0.005")) +
  scale_y_discrete(labels=labels) +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), axis.text.y = element_markdown(),axis.text.x = element_markdown(), strip.background = element_rect(fill = "white"),
        legend.position=c(0.04,0.56), legend.background = element_rect(linewidth=0.2, linetype="solid", colour ="grey10"),
        axis.text = element_text(size = 11), legend.key.height = unit(0.4, 'cm'), strip.text = element_text(size = 13)) +
 # facet_grid(. ~ factor(PopD, levels = c("f4(Mbuti, Spain.PP.2rnd; Target, Peru.Shotgun)", "f4(Mbuti, Spain.PP.2rnd; Target, Peru.1240k)")), drop=TRUE) +
  facet_grid(. ~ factor(test, levels = c("Spain.2R Peru.SG", "Spain.2R Peru.1240K", "Spain.1240k Peru.SG", "Spain.1240k Peru.1240K"), 
    labels = c( "f4(Mbuti, Spain.PP; Target, Peru.Shotgun)", "f4(Mbuti, Spain.PP; Target, Peru.1240k)",  "f4(Mbuti, Spain.1240k; Target, Peru.Shotgun)", "f4(Mbuti, Spain.1240k; Target, Peru.1240k)" ))) +
  ggtitle("") +  labs(x = "", y = "")
f4_plot

 ggsave("Fig4.pdf", width = 20, height = 11, dpi=300)
