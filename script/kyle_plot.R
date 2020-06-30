## Global shark fin trafficking tracing project
## Modeling the relative probability of inside and outside EEZs
## project with Tyler, Sal, Palumbi et al


## Set the working directory, load libraries
setwd("/Users/kvanhoutan/Dropbox (MBA)/Workspace/Sharks/fins")
library(ggplot2)
library(tidyr)
library(scales)
library(sf)
library(readr)
library(dplyr)
library(lwgeom)
library(ggjoy)
library(ggridges)
library(tidyverse)


## load .csv file containing the curated fin modeling data
## build plots of prob density of eez vs high seas
## make a facet wrap for by 4 studies with loess
fins3 <- read.csv('fin_prob_eez_hiseas3.csv', header=T) ##; colnames(fins) = c('study', 'probability','count','region')
ggplot(data = fins3, aes(x=probability, y=count, group=region, color=region)) +
  theme(panel.grid = element_blank(),panel.background = element_blank()) +
  scale_color_manual(values=c("#56B4E9", "#E69F00")) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x, span = 0.8, se = FALSE) +
  scale_y_log10(limits = c(1,90000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l") +
  facet_wrap(~study, ncol=4)

## load .csv file ranking fin probs within eez
## build bar plot of top 10 ranked countries by probability
## make facet of plots for 4 studies
rank_eez_fins <- read.csv('fin_eez_ranking.csv', header=T)
g <- rank_eez_fins %>%
  ggplot(aes(rel_prob, rank)) +
  theme_bw() +
  scale_x_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  theme(panel.grid = element_blank(),panel.background = element_blank()) +
  geom_col(mapping = NULL, data = NULL) +
  scale_y_continuous(breaks = rank_eez_fins$rank,
                     labels = rank_eez_fins$eez,
                     expand = c(0,0))+
  facet_wrap(~study, ncol = 4, scales ="free")