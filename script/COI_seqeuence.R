#plotting COI sequences

library(tidyverse)
library(bold)


spp_seq_plot <- function(id = "NA"){
  
  alopias_pelagicus <- bold_seq(ids = id) %>% data.frame(stringsAsFactors = F)
  seq <- strsplit(alopias_pelagicus$sequence, split = "")[[1]]
  seq_num <- seq_along(seq)
  plot <- ggplot()+
    geom_col( aes( x = seq_num, y = 1, fill = seq))+
    scale_fill_brewer(palette = "Spectral")+
    scale_x_continuous(limits = c(0,1500))+
    theme_void()+
    labs(title = alopias_pelagicus$name)
  print(plot)
  
}

ALOPIA <- spp_seq_plot(id = "KJ146023")
white_shark <- spp_seq_plot(id = "KJ934896")
mustelus <- spp_seq_plot(id = "KT075316")
lmanus <- spp_seq_plot(id = "VADL07")
charchar <- spp_seq_plot(id = "07-GHRI-0654")

gridExtra::grid.arrange(white_shark,
                        ALOPIA,
                        mustelus,
                        lmanus,
                        charchar, ncol = 1)