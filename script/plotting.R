  # plotting single studies
  library(ggplot2)
  library(tidyverse)
  library(maps)
  
  
  
  ##  Popular ggPlot theme

  themeo <- theme_classic()+
    theme(strip.background = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
          axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
          axis.ticks.length = unit(-0.1, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5),
          legend.title = element_blank(),
          strip.text = element_text(hjust=0) )
  
  studies_samp <- list.files('./data/binomial_sampling')
  
  stud_df <- NULL
  
  for(f in 1:length(studies_samp)){
    tmp <- read.csv(paste0('/Users/kvanhoutan/shark_finning_2020/data/binomial_sampling/',studies_samp[f]))
    str(tmp)
    stud_df <- rbind(tmp,stud_df)
  }
  
  str(stud_df)
  
  normalize <- function(x){
    return((x-min(x)) / (max(x)-min(x)))
  }
  
  
  stud_df$seizure <- fct_relevel(stud_df$seizure,c("FEITOSA_COI","MBA_COI","STEINKE_COI","FIELDS_COI"))
  
  
  stud_df <- stud_df %>%
    group_by(seizure) %>%
    mutate(NORMALIZED = normalize(impact))
  
  
  
  mapWorld <- map_data('world', wrap=c(-25,335), ylim=c(-55,75))
  
  stud_map <- stud_df 
  
  # main map Fig
  dens_maps <- ggplot()+
    geom_raster(data = stud_map, aes(if_else(lon2 > 178, lon2 - 2, lon2),Lat,fill=NORMALIZED), show.legend = T) +
    geom_polygon(data = mapWorld, aes(x=if_else(long > 178, long - 2, long), y = lat, group = group), 
                 #fill = "#303030", color = "#303030", 
                 fill = 'black', color = 'black',
                 size = .1)+
    #scale_fill_gradientn(colours = c("black","black",'#4c001f','#72002f','#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','white','white'),na.value = "black")+
    
    # good one
    #scale_fill_gradientn(colours = rev(c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026','black')),na.value = "black")+
    
    scale_fill_gradientn(colours = rev(c("light yellow",'#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026','black')),na.value = "black")+
    #scale_fill_gradientn(colours = c("black",'#4c001f','#72002f','#72002f','#9e0142','#9e0142','#d53e4f','#d53e4f','#f46d43','#f46d43','#fdae61','#fee08b','#ffffbf','white','white'),na.value = "black")+
    #scale_fill_gradientn(colours = c("black","black",'#4c001f','#72002f','#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2'),na.value = "black")+
    #scale_fill_gradientn(colours = c("black","black","#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9E89FF", "#35B779FF", "#6DCD59FF", "#B4DE2CFF", "#FDE725FF", "#FDE725FF", "#FDE725FF","#FDE725FF"),na.value = "black")+
    facet_wrap(~seizure, ncol = 1)+
    themeo + 
    theme(
      panel.background = element_rect(fill = 'black', colour = 'black')#,
      #  plot.background = element_rect(fill = 'black', colour = 'black'),
      # legend.text = element_blank()
    )+
    
    #dbc195
    #ffebba  565656
    
    coord_fixed(ylim = c(-45,55))+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    
    xlab(NULL)+
    ylab(NULL)
  
  dens_maps  
  
  stud_df %>% 
    #filter(NORMALIZED > 0) %>% 

    ggplot()+
    geom_histogram(aes(x=NORMALIZED)) +
    facet_wrap(~seizure, ncol = 1)+
    themeo + 
    theme(
      panel.background = element_rect(fill = 'black', colour = 'black')#,
      #  plot.background = element_rect(fill = 'black', colour = 'black'),
      # legend.text = element_blank()
    )+

    
    xlab(NULL)+
    ylab(NULL)
  
  
  # in EEZ vs overseas proportion
  str(stud_df)
  
  #EEZ_df <- readOGR(dsn = "/Users/tgagne/shark_finning_2018/data/EEZ_land_union_v2_201410", layer = "EEZ_land_v2_201410") %>% tidy()
  ##EEZ_df$long <- ifelse(EEZ_df$long < -25, EEZ_df$long + 360, EEZ_df$long) 
  #EEZ_df <- st_as_sf(EEZ_df, coords = c('long','lat'))
  
  #str(EEZ_df)
  #plot(EEZ_df)
  
  library(sf)
  EEZ_df <- st_read("/Users/kvanhoutan/shark_finning_2020/data/EEZ_land_union_v2_201410/EEZ_land_v2_201410.shp") %>% st_as_sf()
  EEZ_df
  EEZ_df <- st_union(EEZ_df) 
  EEZ_df
  #plot(EEZ_df)
  
  
  # stud to point
  stud_df$Lon    <- stud_df$lon2
  stud_df$lon2   <- NULL
  stud_df$impact <- NULL
  #test_df$lon2 <- ifelse(stud_df$Lon < -25, stud_df$Lon + 360, stud_df$Lon) 
  stud_df$Lon <- ifelse(stud_df$Lon < 335 & stud_df$Lon > 180, stud_df$Lon - 360, stud_df$Lon) 
  
  str(stud_df)
  
  my_sf_object <- st_as_sf(stud_df, coords = c("Lon","Lat"), crs = 4326)
  my_sf_object
  #plot(my_sf_object)
  
  inter <- st_intersects(EEZ_df,my_sf_object) %>% data.frame() 
  inter <- inter[,2] %>% as.numeric()
  
  my_sf_object$in_eez <- seq(1,nrow(my_sf_object), by = 1) 
  my_sf_object$in_eez <- ifelse(my_sf_object$in_eez %in% inter, "EEZ", "HIGH_SEAS") 
  my_sf_object
  sp_pts <- as(my_sf_object, "Spatial") %>% as.data.frame()
  
  #plot(my_sf_object)
  
  sp_pts %>% group_by(seizure) %>% summarise(n())
  
  str(sp_pts)
  
  ## simple plot of sourcing model output
  ## with points symboloized by high seas or EEZ
  ggplot(sp_pts)+
    geom_raster(aes(x=coords.x1, y = coords.x2, fill = in_eez))+
    facet_wrap(~seizure)+
    coord_fixed()
  
 
  ## exploratory plot of Tyler's 
  ## that never made it into the paper or OSM  
  dens_fins <- sp_pts %>%  
    mutate(prob_cuts = as.numeric(cut_width(NORMALIZED,.009))) %>% 
    
    ggplot(aes( x = prob_cuts)) + 
    geom_bar(aes(fill = in_eez), position = "fill", width = 1)+
    scale_fill_manual(values = c("white","dark gray"))+
    facet_wrap(~ seizure, ncol = 1)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    labs(x = "prob of fin acquisition",
         y = "proportion EEZ v HS split")+
    themeo
  
  layout_matrix <- rbind(
    c(1,1,1,1,1,2,2),
    c(1,1,1,1,1,2,2))
  
  gridExtra::grid.arrange(ggplotGrob(dens_maps), ggplotGrob(dens_fins), layout_matrix = layout_matrix)
  
  sp_pts %>%  
    mutate(prob_cuts = as.numeric(cut_width(NORMALIZED,.2))) %>% 
    group_by(seizure,in_eez, prob_cuts) %>% 
    dplyr::summarise(count = n()) %>% 
    spread(in_eez, count) %>% 
    mutate(HIGH_SEAS = ifelse(is.na(HIGH_SEAS), 0, HIGH_SEAS)) %>% 
    mutate(prop = HIGH_SEAS/EEZ) %>% 
    
    ggplot(aes(x=prob_cuts,y=prop, group = seizure, color = seizure))+#geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F)+
    geom_line(size = 1)+
    themeo
  
  
  ## stack histogram comparing the EEZ vs high seas split probabilities
  ## another summary plot of output of the sourcing model
  ggplot(sp_pts,aes( x = NORMALIZED)) + 
    geom_histogram(aes(fill = in_eez))+
    scale_fill_brewer(palette = "Dark2")+
    facet_grid(~ seizure)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    labs(x = "prob of fin acquisition",
         y = "count EEZ v HS split")+
    themeo
```