library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
setwd("/data/models")
df_all_topo <- read.csv("habitat_topohetero_models/df_all_topo.csv")

b_opt_50 <- df_all_topo %>% select(species,CCV_45, elev, TRI, TCI, habitat_opt_50)
b_opt_50$habitat_opt_50 <- factor(b_opt_50$habitat_opt_50)

## remove 0 not suitable and reorder levels
b1 <- b_opt_50 %>% filter(habitat_opt_50 != "0")  %>% mutate(habitat_opt_50 = recode(habitat_opt_50,
     "1" = "Loss",
     "2" = "Gain",
     "3" = "Stable"))

b1$habitat_opt_50 <- factor(b1$habitat_opt_50 , levels= c("Loss", "Stable", "Gain"))
colnames(b1)[6] <- "habitat"
# sample size
sample_size = b1 %>% group_by(habitat) %>% summarize(num=n())

# Plot
p <- b1 %>%
  left_join(sample_size) %>%
  mutate(myaxis = habitat) %>%
  ggplot( aes(x=myaxis, y=elev, fill=habitat)) +
    geom_violin(width=1.4) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete = TRUE) +
    theme_classic() +
    theme(
      legend.position="none",
      strip.text.x = element_text(size = 32))+
    facet_wrap(~ species,scales="free", ncol=8)+ 
    theme(axis.text=element_text(size=32))+#, aspect.ratio = 1) +
    xlab("Habitat suitability") + ylab("Elevation")

svg("boxplot_endemics_elev.svg", height = 10, width = 80)
p
dev.off() 

system('inkscape --export-dpi=300 --export-type="png" boxplot_endemics_elev.svg') 

### CCV_45
p <- b1 %>%
  left_join(sample_size) %>%
  mutate(myaxis = habitat) %>%
  ggplot( aes(x=myaxis, y=CCV_45, fill=habitat)) +
    geom_violin(width=1.4) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete = TRUE) +
    theme_classic() +
    theme(
      legend.position="none",
      strip.text.x = element_text(size = 32))+
    facet_wrap(~ species,scales="free", ncol=8)+ 
    theme(axis.text=element_text(size=32))+#, aspect.ratio = 1) +
    xlab("Habitat suitability") + ylab("CCV_45")

svg("boxplot_endemics_CCV_45.svg", height = 10, width = 80)
p
dev.off() 

system('inkscape --export-dpi=300 --export-type="png" boxplot_endemics_CCV_45.svg') 

##### TRI

p <- b1 %>%
  left_join(sample_size) %>%
  mutate(myaxis = habitat) %>%
  ggplot( aes(x=myaxis, y=TRI, fill=habitat)) +
    geom_violin(width=1.4) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete = TRUE) +
    theme_classic() +
    theme(
      legend.position="none",
      strip.text.x = element_text(size = 32))+
    facet_wrap(~ species,scales="free", ncol=8)+ 
    theme(axis.text=element_text(size=32))+#, aspect.ratio = 1) +
    xlab("Habitat suitability") + ylab("TRI")

svg("boxplot_endemics_TRI.svg", height = 10, width = 80)
p
dev.off() 

system('inkscape --export-dpi=300 --export-type="png" boxplot_endemics_TRI.svg') 


##### TCI

p <- b1 %>%
  left_join(sample_size) %>%
  mutate(myaxis = habitat) %>%
  ggplot( aes(x=myaxis, y=TRI, fill=habitat)) +
    geom_violin(width=1.4) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete = TRUE) +
    theme_classic() +
    theme(
      legend.position="none",
      strip.text.x = element_text(size = 32))+
    facet_wrap(~ species,scales="free", ncol=8)+ 
    theme(axis.text=element_text(size=32))+#, aspect.ratio = 1) +
    xlab("Habitat suitability") + ylab("TRI")

svg("boxplot_endemics_TCI.svg", height = 10, width = 80)
p
dev.off() 

system('inkscape --export-dpi=300 --export-type="png" boxplot_endemics_TCI.svg') 


