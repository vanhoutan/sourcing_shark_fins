library(sf)
EEZ_df <- readOGR("/Users/tgagne/shark_finning_2018/data/EEZ_land_union_v2_201410/EEZ_land_v2_201410.shp") %>% fortify()
EEZ_df
#EEZ_df <- st_union(EEZ_df) 
#$EEZ_df <- fortify(EEZ_df)
#plot(EEZ_df)
str(EEZ_df)

# stud to point
stud_df$Lon <- ifelse(stud_df$Lon < 335 & stud_df$Lon > 180, stud_df$Lon - 360, stud_df$Lon) 

mapWorld <- map_data('world', ylim=c(-55,75))

str(stud_df)

ggplot()+
  geom_tile(
    data = stud_map %>% filter(seizure == "FIELDS_COI" & Lon > 100 & Lon < 180 & Lat > -20 & Lat < 45) , 
    #data = stud_map %>% filter(seizure == "FEITOSA_COI" & Lon > -85 & Lon < - 0 & Lat > -20 & Lat < 45), 
            aes(Lon,Lat,fill=NORMALIZED), show.legend = F) +
  geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group), 
               fill = "#303030", color = "#303030", 
               #fill = 'black', color = 'black',
               size = .1)+
  #geom_sf(data = EEZ_df, fill = NA, color = "white", size = .25)+
  geom_polygon(data = EEZ_df,aes(x = long, y = lat, group = group), fill = NA, color = "white", size = .25)+
  
  #scale_fill_gradientn(colours = c("black","black",'#4c001f','#72002f','#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','white','white'),na.value = "black")+
  
  # good one
  scale_fill_gradientn(colours = rev(c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026','black')),na.value = "black")+
  
  #scale_fill_gradientn(colours = c("black",'#4c001f','#72002f','#72002f','#9e0142','#9e0142','#d53e4f','#d53e4f','#f46d43','#f46d43','#fdae61','#fee08b','#ffffbf','white','white'),na.value = "black")+
  #scale_fill_gradientn(colours = c("black","black",'#4c001f','#72002f','#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2'),na.value = "black")+
  #scale_fill_gradientn(colours = c("black","black","#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9E89FF", "#35B779FF", "#6DCD59FF", "#B4DE2CFF", "#FDE725FF", "#FDE725FF", "#FDE725FF","#FDE725FF"),na.value = "black")+
  #facet_wrap(~seizure, ncol = 1)+
  themeo + 
  theme(
   panel.background = element_rect(fill = 'black', colour = 'black')#,
    #  plot.background = element_rect(fill = 'black', colour = 'black'),
    # legend.text = element_blank()
  )+
  
  #dbc195
  #ffebba  565656
  coord_map("ortho", 
            orientation = c(20, 160, 0), xlim = c(100,180), ylim = c(-20,45)
            #orientation = c(7, -55, 0), xlim = c(-85,0), ylim = c(-20,45)
            
            ) +
  #coord_fixed(ylim = c(-45,55))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  
  xlab(NULL)+
  ylab(NULL)
