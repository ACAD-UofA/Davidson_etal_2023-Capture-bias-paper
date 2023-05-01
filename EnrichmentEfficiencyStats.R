library("RColorBrewer")

fn="./seq_stats_efficiency.tsv"
dat=read.table(fn, header=TRUE) 

#order 
dat$Shotgun.Capture <- factor(dat$Shotgun.Capture, levels = c("Shotgun","PP_1round","PP_1round_62C","PP_2_round")) 
dat <- mutate(dat, x_norm_p = (X.- 7.75)/(99.37 - 7.75))

#plot
plot <- ggplot(dat, aes(x=Shotgun.Capture)) +
  geom_line(aes(y=x_norm_p*100, group=name, colour=Sample), na.rm=T, size = 2, alpha = 0.7) +
  geom_point(aes(y=x_norm_p*100, group=name, fill=Sample, shape=col), colour = "black", size = 4, na.rm=T) +
  scale_shape_manual(values=c(21,23), labels=c("% Endogenous DNA","% Unique reads")) +
  scale_x_discrete(labels=c("Shotgun", "PP.1rnd.A","PP.1rnd.B","PP.2rnd")) +
  labs(x="", y= "%", fill="", shape = "", col="") +
  theme_bw() +
  theme(legend.position = "none",strip.background = element_rect(fill = "white"),axis.text = element_text(size=11),
        axis.title=element_text(size=12), strip.text = element_text(size = 11)) +
  facet_grid(factor(col, labels = c("Endogenous DNA %","Unique Reads %")) ~ .)

plot 

# wow sick graphs mate
ggsave("substitutioncounts_2.pdf", width = 10, height = 8, dpi=300) #save in pdf format with size 12 x 6 in
