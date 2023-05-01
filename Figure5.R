library(tidyverse)
library(Hmisc)
library(patchwork)
library(plotly)

#load data
arbor = read.table("./Arbor2_fixed.frq.counts", header=TRUE)
shotgun = read.table("./Shotgun1_fixed.frq.counts", header=TRUE)
boston = read.table("./SA_1240K_fixed.frq.counts", header=TRUE)

#add column of allele frq, calling frq, data type and filter out sex CHR
arbor_calc <- arbor %>% 
  mutate(C1_frq = C1/(C1+C2)) %>% #calculate frequency of first allele
  mutate(missingness=(1-((C1+C2)/(C1+C2+G0)))) %>% #calculate per SNP missingness
  mutate(data_type="Arbor") %>% #add column for data type
  filter(CHR<23) %>% # filter out non-autosomes
  filter(C1_frq<1) %>% filter(C1_frq>0) %>% # remove fixed SNPs
  filter(pair!="C/G",pair!="A/T") #remove C/G and A/T pairs (not present in all data types and thus not comparable)

shotgun_calc <- shotgun %>% 
  mutate(C1_frq = C1/(C1+C2)) %>%
  mutate(missingness=(1-((C1+C2)/(C1+C2+G0)))) %>%
  mutate(data_type="shotgun") %>%
  filter(CHR<23) %>%
  filter(C1_frq<1) %>% filter(C1_frq>0) %>%
  filter(pair!="C/G",pair!="A/T")

boston_calc <- boston %>% 
  mutate(C1_frq = C1/(C1+C2)) %>%
  mutate(missingness=(1-((C1+C2)/(C1+C2+G0)))) %>%
  mutate(data_type="boston_1240K") %>%
  filter(CHR<23) %>%
  filter(C1_frq<1) %>% filter(C1_frq>0) %>%
  filter(pair!="C/G",pair!="A/T")

#join tables
df <- rbind(arbor_calc, shotgun_calc, boston_calc) %>% #join data types into one table
  filter(C1_frq<1) %>% #remove fixed SNPs
  filter(C1_frq>0) %>% #remove fixed SNPs
  mutate(miss_bin = cut(missingness, breaks=100)) #calculate breaks for 100 missingness bins
#create summary stats
means <- df %>% 
  group_by(data_type,miss_bin,pair) %>% #group data by assay, missingness bin and allele pair
  dplyr::summarise(frq_mean = mean(C1_frq),miss_mean = mean(missingness),frq_sd = var(C1_frq)) %>% #calculate mean allele frq, mean missingness and standard deviation of allele frequency per grouped bin
  as.data.frame()
head(means) #look at the data

means$data_type <- factor(means$data_type, levels = c("boston_1240K","shotgun","Arbor"), labels = c("1240k","Shotgun","Prime Plus")) #fix labelsfor data types

stats = pivot_longer(means, cols=c(frq_mean,frq_sd), names_to=c("stat_type"), values_to = c("stat")) #pivot data long to plot
stats

#plot
mean_plot <- ggplot(means, aes(x=miss_mean)) +
  geom_hline(yintercept=0.5) +
  geom_point(aes(fill=data_type, y=frq_mean), colour="black",size=1.5,alpha=0.6,shape=21, stroke=0.5) +
  scale_fill_manual(values=c("tomato4","grey30","darkgreen"), labels=c("1240k","Shotgun","Prime Plus")) +
  geom_smooth(aes(colour = data_type, fill=data_type,y=frq_mean), se=T,stat = "smooth", position = "identity", span=1, level=0.66, method = 'loess', size=1.2) +
  scale_colour_manual(values=c("tomato2","grey40","limegreen"), labels=c("1240k","Shotgun","Prime Plus")) +
  scale_y_continuous(limits=c(0.25,0.75)) +
  scale_x_continuous(limits=c(0,1),breaks=c(0,0.5,1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), strip.background = element_rect(fill = "white"), legend.position = c(0.1,0.1),
        legend.background = element_rect(linewidth=0.2, linetype="solid", colour ="grey10"), axis.text = element_text(size=11),
        axis.title=element_text(size=12), strip.text = element_text(size = 11), plot.title=element_text(hjust=0.5)) +
  labs(x = "SNP missingness", y = "A1 allele frequency", title = "A1/A2", shape = "Data Type", fill = "Data Type", colour = "Data Type") +
  facet_grid(. ~ 
                      factor(pair, levels=c('A/C','T/G','A/G','T/C'), 
      labels = c("A/C - 118,742 SNPs","T/G - 130,443 SNPs","A/G - 518,229 SNPs","T/C - 467,535 SNPs")))
mean_plot

ggsave("Fig5.pdf", width = 12, height = 8, dpi=300)
