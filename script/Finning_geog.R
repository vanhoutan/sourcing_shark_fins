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
# 1. sample x points from range map
#    x = prop of that single spp makes up of total seizure times 1,000,000

# 2. save each sample round as a raster

# 3. layer those rasters and add them together

# 4. repeat for each species with sample size x based on number 
#    of fins obtained in seizure

# 5. Add all those together in to a single raster, this is a repeated 
#    simulation showing the probibal geographic extent of finning, given
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

#functions
normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

#library loan
library(tidyverse)     # standard tidy data packages
library(raster)        # raster manipulation
library(data.table)    # large data.frame fast reading 'fread'
library(foreach)       # paralell for loop package
library(doParallel)    # registering cores for paralell processing
library(rnaturalearth) # coastline shapefiles for figure
library(broom)         # tidying spatial shapefiles 
library(rgdal)         # spatial manipulation


# run earlier and just read in at this pint

# read in all rasters as DF and cbind in to DF
path <- '/Users/ktanaka/Desktop/TYLER_code_sharkfins_2018/data/Reygondeau_dist_mods'
path <- '/Users/ktanaka/Desktop/TYLER_code_sharkfins_2018/data/Reygondeau_aqua_mods'

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

str(null_df)

gathered_mods <- gather(null_df,                                 # cast this wide df to tall 
                        key = "spp",
                        value = "mod_val", -Lat, -Lon)

#write.csv(gathered_mods,'binom_gathered_mods_Nov8.csv', row.names = F)

#gathered_mods <- fread('/Users/tgagne/shark_finning_2018/data/binom_gathered_mods.csv')
gathered_mods <- fread('/Users/tgagne/shark_finning_2018/data/binom_gathered_mods_Nov8.csv')
# minimize sparsity in the tall dataframe to save space, 
# remove NA cell values, i.e. locations where a spp has no distribution
gathered_mods <- gathered_mods[complete.cases(gathered_mods),]   

fin_hauls <- readxl::read_xlsx('/Users/ktanaka/shark_finning_2020/data/seizure data/shark_fins_counts_KV.xlsx')

str(fin_hauls)

# summary of fin counts by study
fin_hauls %>% 
  filter(STUDY %in% levels(as.factor(fin_hauls$STUDY))[c(1,2,3,5)]) %>% 
  group_by(STUDY) %>% 
  summarize(sum_fins = sum(COUNT))


# Chr vector for ordering of levels by a single study
spp_order <- fin_hauls %>% filter(STUDY == "FIELDS_COI") %>% mutate(SPECIES_NAME = SPECIES_NAME %>% fct_reorder(COUNT)) 
spp_order <- levels(spp_order$SPECIES_NAME)

# summary of fin counts by spp by study
fin_hauls %>% 
  filter(STUDY %in% levels(as.factor(fin_hauls$STUDY))[c(1,2,3,5)]) %>% 
  #filter(COUNT > 3) %>% 
  mutate(SPECIES_NAME = SPECIES_NAME %>% fct_relevel(spp_order,after = Inf),
         STUDY = STUDY %>% fct_reorder(COUNT, .fun = sum, .desc = T)  ) %>% 
  
  group_by(STUDY) %>% 
  mutate(scaled_fin = ( COUNT - mean(COUNT) )/sd(COUNT)) %>% 
  ungroup() %>% 
  ggplot()+
  #geom_bar(aes(x=SPECIES_NAME,y=COUNT, fill = COUNT), stat = "identity", show.legend = F)+
  geom_bar(aes(x=SPECIES_NAME,y=COUNT, fill = scaled_fin), stat = "identity", show.legend = F)+
  
  facet_wrap(~STUDY, scales = "free_x",nrow = 1)+
  #scale_fill_manual(values = colorRampPalette(brewer.pal(8,"Spectral"))(62) %>% rev()  ) +
  #scale_fill_manual(values = pals::kovesi.rainbow(62) ) +
  scale_fill_gradientn(colors = pals::parula(60) ) +
  
  coord_flip() +
  themeo



spps <- levels(as.factor(fin_hauls$SPECIES_NAME))
gathered_mods$spp <- as.factor(gathered_mods$spp)

#spps <- c("Alopias pelagicus", "Alopias superciliosus","Alopias vulpinus","Isurus paucus" ,             
#           "Lamiopsis temminckii","Lamna ditropis","Lamna nasus","Sphyrna corona", "Carcharhinus obscurus")

gathered_mods <- gathered_mods %>% 
  filter(spp %in% spps) %>% 
  #filter(mod_val > 0) %>% 
  group_by(spp) %>% 
  mutate(mod_val = if_else(mod_val > 1,1,mod_val)) %>% 
  #mutate(mod_val = normalize(mod_val)) %>% 
  ungroup()

summary(gathered_mods)
str(gathered_mods)
#ranges <- gathered_mods %>% filter(spp %in% spps)
#fct_order <- as.vector( (fct_count(ranges$spp) %>% arrange(n))[[1]] )
#ranges$spp <- fct_relevel(ranges$spp, fct_order)

gathered_mods %>% 
  ggplot(aes(Lon, Lat))+
  geom_raster(aes(fill = mod_val))+ 
  coord_fixed() +
  #scale_fill_gradientn(colours = rev(c('#ffffcc','#ffeda0','#fed976',
  #                                     '#feb24c','#fd8d3c','#fc4e2a',
  #                                     '#e31a1c','#bd0026','#800026','black')), na.value = "gray")+
  scale_fill_gradientn(colors = pals::parula(60) ) +
  facet_wrap(~spp)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  
  themeo+
  theme(panel.background = element_rect(fill = 'black', colour = 'black')) 



#ggplot(ranges, aes(Lon, Lat))+geom_tile(aes(fill = mod_val))+ 
#  coord_fixed() +
#  facet_wrap(~spp)+
#  themeo+
#  theme(panel.background = element_rect(fill = 'black', colour = 'black')) 

# bring in seizure data
# for each seizure
for(p in c(1,2,3,5)){
  
  p = 1
  
  fin_hauls <- readxl::read_xlsx('/Users/ktanaka/shark_finning_2020/data/seizure data/shark_fins_counts_KV.xlsx')
  fin_hauls$COUNT <- floor(fin_hauls$COUNT)
  fin_hauls <- filter(fin_hauls, COUNT > 0)
  
  seizure_studies <- levels(as.factor(fin_hauls$STUDY)) 
  seizure_studies # vector of seizures
  #p = 5
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
  meg_samp <- NULL
  #N <- 1000000
  N <- 500000
  
  # for each paralell function def
  spp_n_sample <- function(spp_n){
    point_samp <- dplyr::sample_n(spp_n,1, replace = T)
    shark_no_shark <- rbinom(n = 1, size = 1, prob = as.numeric(point_samp[,"mod_val"]) )
    run_shark <- cbind(point_samp,shark_no_shark,sim_run = r)
  }
  
  #setup parallel backend to use many processors
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  for(s in 1:dim(seizure_spp)[1]){
    
    spp_n <- filter(gathered_mods,spp == seizure_spp$spp[s])
    spp_n
    
    fin_count <- filter(fin_hauls,spp == seizure_spp$spp[s])[,"COUNT"] %>% ceiling()
    fin_count
    
    N_frac <- fin_count/sum(fin_hauls$COUNT)
    N_spp_total <- ceiling(N_frac*N) %>% as.numeric()
    
    null_samp_df <- NULL # for paralell
    
    # paralell loop
    #null_samp_df <- foreach(r=1:(as.numeric(fin_count)*N), .combine = rbind) %dopar% {
    null_samp_df <- foreach(r=1:N_spp_total, .combine = rbind) %dopar% {
      
      temp_null <- spp_n_sample(spp_n)
      temp_null}
    
    # working slow for loop
    #for(r in 1:(as.numeric(fin_count)*N) ){
    #  point_samp <- dplyr::sample_n(spp_n,1, replace = T)
    #  shark_no_shark <- rbinom(n = 1, size = 1, prob = point_samp[,"mod_val"]);shark_no_shark
    #  run_shark <- cbind(point_samp,shark_no_shark,sim_run = r);run_shark
    #  null_samp_df <- rbind(null_samp_df,run_shark)
    #  print(r)
    #}
    
    str(null_samp_df)
    meg_samp <- rbind(null_samp_df,meg_samp) # works for reg for paralell loop?
    print(s)
    
  }
  
  stopCluster(cl)
  
  str(meg_samp)
  
  meg_samp$mod_val <- NULL
  meg_samp$Lon <- as.factor(meg_samp$Lon)
  meg_samp$Lat <- as.factor(meg_samp$Lat)
  
  test_df <- meg_samp %>% group_by(Lon,Lat) %>% 
    summarise(impact = sum(shark_no_shark, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(Lon = as.numeric(as.character(Lon)),
           Lat = as.numeric(as.character(Lat)),
           impact = ifelse(impact == 0, NA, impact))
  
  str(test_df)
  test_df <- test_df[complete.cases(test_df),]   
  test_df$lon2 <- ifelse(test_df$Lon < -25, test_df$Lon + 360, test_df$Lon) # where d is your df
  mapWorld <- map_data('world', wrap=c(-25,335), ylim=c(-55,75))
  
  map <- ggplot() +
    geom_tile(data = test_df, aes(lon2,Lat,fill=impact)) +
    geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group),fill = "#303030", color = "#303030", size = 1)+
    scale_fill_gradientn(colours = rev(c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026','black')),na.value = "black")+
    
    themeo + 
    theme(panel.background = element_rect(fill = 'black', colour = 'black')) +#, legend.text = element_blank())+
    coord_fixed(ylim = c(-50,50))+
    scale_x_continuous(expand = c(0,0))+scale_y_continuous(expand = c(0,0))+xlab(NULL)+ylab(NULL)+
    ggtitle(seizure_studies_name)
  map
  
  test_df$seizure <- seizure_studies_name
  write.csv(test_df, paste0(seizure_studies_name,"1mil_Nov7",'.csv'), row.names = F) 
  
}

