setwd("/data/models/")
t<-read.table("results_range_analysis/range_analysis.txt", sep="\t", h=T)
t[is.na(t)]
t <- na.omit(t)
t$Species <- factor(t$Species)
t$Scenario <- factor(t$Scenario)
install.packages("forcats")
library(forcats)
# levels(t$Species) <- c(levels(t$Species), "Campanula") 
# t$Species[t$Species == "morettiana"] <- "Campanula"
# levels(t$Species) <- c(levels(t$Species), "Rhizobotrya") 
# t$Species[t$Species == "alpina"] <- "Rhizobotrya"
# levels(t$Species) <- c(levels(t$Species), "Saxifraga") 
# t$Species[t$Species == "facchinii"] <- "Saxifraga"
# levels(t$Species) <- c(levels(t$Species), "Sempervivum") 
# t$Species[t$Species == "dolomiticum"] <- "Sempervivum"


library(ggplot2)
library(PMCMRplus)
myTheme <- theme(
	panel.background = element_rect(fill = "white", colour = "black"), 
	panel.grid.major =  element_blank(),
	panel.grid.minor = element_blank(),
	axis.text = element_text(color = "black", size=14),
	axis.text.x = element_text(color = "black", size=14),
)
## algorithms -----
p<-ggplot(data = t, aes(x=Species, y=range_change, fill = Algo)) +
 geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5) +
 scale_fill_manual(values=c("grey100","grey60", "grey80","grey70","grey90")) +
  scale_x_discrete(labels=c("Campanula morettiana","Festuca austrodolomitica","Gentiana brentae", "Nigritella bushmanniae", "Primula tyrolensis", "Rhizobotrya alpina", "Saxifraga facchinii", "Sempervivum dolomiticum"),
                   guide = guide_axis(n.dodge = 2))+
 myTheme +  facet_wrap(interaction(t$Scenario))
# ggsave("range_change.pdf", width = 10, height = 10)

### without algorithms

#### RANGE LOSS -----
d<-ggplot(data = t, aes(x=Species, y=range_change_without_gain, fill = Scenario)) +
   geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5) +
   scale_x_discrete(labels=c("Campanula morettiana","Festuca austrodolomitica","Gentiana brentae", "Nigritella bushmanniae", "Primula tyrolensis", "Rhizobotrya alpina", "Saxifraga facchinii", "Sempervivum dolomiticum"),
   guide = guide_axis(n.dodge = 2)) +
   scale_fill_discrete(name = "2080 scenario", labels = c("optimistic (rcp 4.5)", "pessimistic (rcp 8.5)")) +
   ylab("Range Loss (%)") +
  scale_fill_brewer(palette="Greys") +
   stat_compare_means(aes(label = paste0("p = ", ..p.format..)), method = 'kruskal.test')+
   myTheme 
   #facet_wrap(t$Scenario)
ggplot_build(d)$data

### RANGE CHANGE
p<-ggplot(data = t, aes(x=Species, y=range_change, fill = Scenario)) +
  geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5) +
  scale_x_discrete(labels=c("Campanula morettiana","Festuca austrodolomitica","Gentiana brentae", "Nigritella bushmanniae", "Primula tyrolensis", "Rhizobotrya alpina", "Saxifraga facchinii", "Sempervivum dolomiticum"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_fill_discrete(name = "2080 scenario", labels = c("optimistic (rcp 4.5)", "pessimistic (rcp 8.5)")) +
  scale_fill_brewer(palette="Greys") +
  ylab("Range Change (%)") + 
  stat_compare_means(aes(label = paste0("p = ", ..p.format..)), method = 'kruskal.test')+
  myTheme #+ 
#facet_wrap(t$Scenario)


#### RANGE TURNOVER
p<-ggplot(data = t, aes(x=Species, y=range_turnover, fill = Scenario)) +
  geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5) +
  scale_x_discrete(labels=c("Campanula morettiana","Festuca austrodolomitica","Gentiana brentae", "Nigritella bushmanniae", "Primula tyrolensis", "Rhizobotrya alpina", "Saxifraga facchinii", "Sempervivum dolomiticum"),
                   guide = guide_axis(n.dodge = 2)) +
  scale_fill_discrete(name = "2080 scenario", labels = c("optimistic (rcp 4.5)", "pessimistic (rcp 8.5)")) +
  ylab("Range Turnover (%)") + 
  myTheme #+ 
#facet_wrap(t$Scenario)



ggsave("range_change1.pdf", width = 20, height = 10)

p<-ggplot(data = t, aes(x=Species, y=range_turnover, fill = as.factor(Scenario))) 
p<-p+geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)
p<-p+scale_fill_manual(values=c("grey100","grey60"))
p+myTheme +  facet_wrap(interaction(t$year))
ggsave("range_turnover1.pdf", width = 10, height = 10)



p<-ggplot(data = t, aes(x=Species, y= range_turnover, fill = Algo, by = interaction(Scenario))) 
p<-p+geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)
p<-p+scale_fill_manual(values=c("grey100","grey60", "grey80","grey70","grey90"))
p+myTheme +  facet_wrap(t$year)
ggsave("range_turnover.pdf", width = 10, height = 5)

p<-ggplot(data = t, aes(x=Species, y= range_loss)) 
p<-p+geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)
p<-p+scale_fill_manual(values=c("grey100","grey60", "grey80","grey70","grey90", "grey30"))
p+myTheme +  scale_x_discrete(labels=c("Campanula morettiana","Festuca austrodolomitica","Gentiana brentae", "Nigritella bushmanniae", "Primula tyrolensis", "Rhizobotrya alpina", "Saxifraga facchinii", "Sempervivum dolomiticum"),
                              guide = guide_axis(n.dodge = 2))
ggsave("range_loss.pdf", width = 10, height = 5)

mean(t$range_change)
summary(t)


