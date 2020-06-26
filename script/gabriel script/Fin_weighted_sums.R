# drafting shark finning geographic impact assesment

# what we should have:
# Maps of occurences for species:
# Modeled ranges for species:
#
# For modeled ranges, I believe they are probability of occurence
# If that is the case, then I could generate 0/1 range maps based 
# on some cutoff probability, perhaps 80% +- 15% (supplement content)
# Or the 80% quantile cutoff of the distribution of the 'suitability 
# index' (environmental niche model).

# We then have a list of species from each seizure with a count of 
# how many fins of that species were from that seizure.

# In order to estimate that geographic coverage of one species -->

# Blueprint
# 1. sample x points from range map, repeat 1000 times
#    x = number of fins from seizure

# 2. save each sample round as a raster

# 3. layer those rasters and add them together

# 4. repeat for each species with sample size x based on number 
#    of fins obtained in seizure

# 5. Add all those together in to a single raster, this is a repeated 
#    simulation showing the likely geographic extent of finning, given
#    that species were found within their estimated ranges.
##############################
###  Popular ggPlot theme  ###
##############################
themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )

#library loan
library(tidyverse)     # standard tidy data packages
library(raster)        # raster manipulation
library(data.table)    # large data.frame fast reading 'fread'
library(foreach)       # paralell for loop package
library(doParallel)    # registering cores for paralell processing
library(rnaturalearth) # coastline shapefiles for figure
library(broom)         # tidying spatial shapefiles 
library(rgdal)         # spatial manipulation

# read in all rasters as DF and cbind in to DF
path <- '/Users/tgagne/shark_finning_2018/data/Reygondeau_dist_mods'

# list of all .csvs of Gabriels range model outputs
list_gab <- list.files(path);list_gab

# build a dataframe of lat lon matching those model outputs
null_df <- read.csv(paste0(path,"/",list_gab)[1])[,1:2] # dummy df with lon/lat grid

# build a dataframe of models, where each column is model predictions of probable range of a single species
for(i in 1:length(list_gab)){
  
  list_gab_n <- list_gab[i]                                   # grab a single file (modeled dist 0-1)
  super_rich <- fread(paste0(path,"/",list_gab_n))            # quick read in with data.table fread
  colnames(super_rich)[4] <- strsplit(list_gab_n,"_")[[1]][1] # grab the name of the spp and assign column name
  col <- data.frame( col_name = super_rich[,4])               # concatenate columns of the multiple species
  colnames(col) <- colnames(super_rich)[4]                    # every column is a spp
  null_df <- cbind( null_df, col)
  print(i)
  
}

gathered_mods <- gather(null_df,                                 # cast this wide df to tall 
                        key = "spp",
                        value = "mod_val", -Lat, -Lon)

# minimize sparsity in the tall dataframe to save space, 
# remove NA cell values, i.e. locations where a spp has no distribution
#gathered_mods <- gathered_mods[complete.cases(gathered_mods),]   
str(gathered_mods)

# bring in seizure data
# for each seizure


megabind <- NULL


for(p in 1:5){

fin_hauls <- readxl::read_xlsx('/Users/tgagne/shark_finning_2018/data/seizure data/shark_fins_counts_KV.xlsx')
seizure_studies <- levels(as.factor(fin_hauls$STUDY)) 
seizure_studies # vector of seizures
seizure_studies_name <- seizure_studies[p]                                 # name a single seizure record, i.e. Feitosa, etc.
seizure_studies_name
fin_hauls <- filter(fin_hauls, STUDY == seizure_studies_name & COUNT >= 1) # subset out a single study
fin_hauls
colnames(fin_hauls)[1] <- 'spp' 
str(fin_hauls)

#ggplot(gathered_mods,aes(Lon,Lat,fill = mod_val ))+geom_tile()+facet_wrap(~spp)

# spp in need 
spp_needed <- c("Carcharhinus leiodon",
                "Lamiopsis temminckii",
                "Rhizoprionodon lalandii",
                "Rhynchobatus australiae",
                "Rhynchobatus djiddensis",
                "Rhynchobatus laevis",
                "Sphyrna media",
                "Carcharodon carcharias" # idk why? Gab?
                )

seizure_spp <- filter(fin_hauls, !spp %in%  spp_needed)
seizure_spp

latlon_df <- read.csv(paste0(path,"/",list_gab)[1])[,1:2]
latlon_df$seizure_name <- seizure_studies_name
str(latlon_df)

for(s in 1:dim(seizure_spp)[1]){
  
  spp_n <- filter(gathered_mods,spp == seizure_spp$spp[s])
  spp_n
  
  fin_count <- filter(fin_hauls,spp == seizure_spp$spp[s])[,"COUNT"] %>% ceiling()
  fin_count
  
  # multiply the model val vector by the fin count
  weighted_mod <- spp_n$mod_val * as.numeric(fin_count)
  weighted_mod
  
  # cbind to a dataframe with: dummy(lon, lat, seizure_name,) spp1_x_fincount, spp2_x_fincount,spp3_x_fincount ...
  latlon_df <- cbind(latlon_df, weighted_mod)
  
  str(latlon_df)

}

# outside the spp specific loop get a row sum ignoring NA for all spp columns
summed_seizure <- rowSums(latlon_df[, 4:dim(latlon_df)[2]], na.rm = T)
seize_df <- data.frame(latlon_df[,1:3],summed_seizure )

# rbind this to the output of the seizure specific loop
megabind <- rbind(megabind,seize_df)

}

str(megabind)


normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}


library(DescTools) 

megabind %>%
  group_by(seizure_name) %>%
  mutate(NORMALIZED = normalize(summed_seizure)) %>% 
  #filter(seizure_name == seizure_studies[1]  ) %>% 
  filter(NORMALIZED > 0.01  ) %>% 
  
  #mutate(NORMALIZED = Winsorize(NORMALIZED,probs = c(0,0.90))) %>% 
  ggplot()+
  geom_raster(aes(Lon,Lat, fill = NORMALIZED, group = seizure_name))+
  facet_wrap(~seizure_name, ncol = 1)+
  coord_fixed()


megabind %>%
  group_by(seizure_name) %>%
  mutate(NORMALIZED = normalize(summed_seizure)) %>% 
  filter(NORMALIZED > 0.01  ) %>% 
  
  #mutate(NORMALIZED = Winsorize(NORMALIZED,probs = c(0,0.90))) %>% 
  ggplot()+
  geom_histogram(aes(NORMALIZED))+
  facet_wrap(~seizure_name, ncol = 1)


str(test_df)
test_df <- test_df[complete.cases(test_df),]   
#test_df <- filter(test_df, impact > quantile(impact, .05))

test_df$lon2 <- ifelse(test_df$Lon < -25, test_df$Lon + 360, test_df$Lon) # where d is your df
mapWorld <- map_data('world', wrap=c(-25,335), ylim=c(-55,75))


map <- ggplot() +
  geom_tile(data = test_df, aes(lon2,Lat,fill=impact)) +
  geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group), fill = "light gray", color = "black", size = .1)+
  #scale_fill_gradientn(colours = c("white","white","white","#fddbc7","#f46d43","#d73027","#b2182b","#b2182b","#8e0516"),na.value = "white")+
  scale_fill_gradientn(colours = c("#fddbc7","#f46d43","#d73027","#b2182b","#b2182b","#8e0516"),na.value = "white")+
  #scale_fill_gradientn(colours = c("dark blue","dark blue","dark blue","#fddbc7","#f46d43","#d73027","#b2182b","#b2182b"),na.value = "dark blue")+
  
  themeo + 
  theme(
    # panel.background = element_rect(fill = 'dark blue', colour = 'dark blue')#,
    #   plot.background = element_rect(fill = 'black', colour = 'black'),
    # legend.text = element_blank()
  )+
  
  #dbc195
  #ffebba 
  
  coord_fixed(ylim = c(-50,50))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  
  xlab(NULL)+
  ylab(NULL)+
  ggtitle(seizure_studies_name)
map

#ggsave(paste0(seizure_studies_name,'.pdf'))

test_df$seizure <- seizure_studies_name

#megabind <- rbind(megabind,test_df)

#}


write.csv(test_df, paste0(seizure_studies_name,'.csv'), row.names = F) 


