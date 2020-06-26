library(aquamapsdata)
library(purrr)

#download_db(force = TRUE)

my_db <- aquamapsdata:::src_sqlite_aquamapsdata()

spps <- c("Alopias pelagicus", 
          "Alopias superciliosus",
          "Alopias vulpinus",
          "Isurus oxyrinchus", 
          "Isurus paucus" ,             
          "Lamna ditropis",
          "Lamna nasus",
          "Sphyrna corona")

for(s in 1:length(spps)){
  # pull in species native range from aquampas
  species <- spps[s]
  species
  
  spp_ID <- am_name_search_exact(binomial = species)[[1]]
  spp_ID
  
  range <- my_db %>% tbl("nativemaps") %>% 
    filter(SPECIESID == spp_ID) %>% 
    collect() 
  
  str(range)
  
  range$Lat <- NA
  range$Lon <- NA
  range$initial <- NA
  
  for(i in 1:nrow(range)){
    
    range$Lat[i] <- csquare_to_dd(range$CsquareCode[i])$lat
    range$Lon[i] <- csquare_to_dd(range$CsquareCode[i])$lon
    range$initial[i] <- csquare_to_dd(range$CsquareCode[i])$initial
  } 
  
  # correct lat lon region to relavant sign
  range$Lon <- ifelse(range$initial == "NE",range$Lon, range$Lon)
  range$Lat <- ifelse(range$initial == "NE",range$Lat , range$Lat)
  
  range$Lon <-ifelse(range$initial == "NW",range$Lon * -1, range$Lon)
  range$Lat <- ifelse(range$initial == "NW",range$Lat , range$Lat)
  
  range$Lon <-ifelse(range$initial == "SE",range$Lon , range$Lon)
  range$Lat <- ifelse(range$initial == "SE",range$Lat * -1 , range$Lat)
  
  range$Lon <-ifelse(range$initial == "SW",range$Lon * -1, range$Lon)
  range$Lat <- ifelse(range$initial == "SW",range$Lat * -1, range$Lat)
  
  
  ggplot(range,aes(Lon,Lat,fill = probability))+geom_raster()+coord_fixed()
  
  
  
  # turn in to raster
  range <- rasterFromXYZ(range[c('Lon','Lat','probability')], crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  range
  
  
  # match to raster made from Gabriels models
  match_raster <- read.csv('/Users/ktanaka/Desktop//shark_finning_2018/data/Reygondeau_dist_mods/Aetobatus narinari_OBS_MODEL.csv')
  str(match_raster)
  match_raster <- rasterFromXYZ(match_raster[c(1,2,4)],crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  match_raster
  
  range <- spatial.tools::spatial_sync_raster( range, match_raster, method = "ngb", size_only = FALSE, verbose = T)
  
  plot(match_raster)
  plot(range, add = T)
  
  # after matching turn back in to data frame
  
  range <- range %>% 
    rasterToPoints() %>% 
    as.data.frame() %>% 
    mutate(Lon = x,
           Lat = y,
           OBS = NA,
           MODELAVG = probability) 
  
  
  range <- range[,c("Lon","Lat","OBS","MODELAVG")]
  
  empty_gab_df <- read.csv('/Users/tgagne/shark_finning_2018/data/Reygondeau_dist_mods/Aetobatus narinari_OBS_MODEL.csv')
  empty_gab_df$OBS <- NaN
  empty_gab_df$MODELAVG <- NaN
  
  str(empty_gab_df)
  
  
  str(range)
  
  test <- left_join(empty_gab_df,range,by = c("Lon","Lat")) %>% 
    mutate(MODELAVG = MODELAVG.y,
           OBS = NA) 
  
  
  range <-  test[,c('Lon','Lat','OBS','MODELAVG')]
  range$OBS <- NaN
  range$MODELAVG <- ifelse(is.na(range$MODELAVG), NaN, range$MODELAVG)
  
  str(range)
  
  
  ggplot(range,aes(Lon,Lat,fill=MODELAVG))+geom_raster()+coord_fixed()
  
  
  # save as csv with format of Gabriels .csvs
  csv_name <- paste0("",species,"_OBS_MODEL.csv")
  
  #write.csv(range,csv_name, na = "NaN", row.names = F)
  
  #example naming schema: Aetobatus narinari_OBS_MODEL.csv
  
  
}









