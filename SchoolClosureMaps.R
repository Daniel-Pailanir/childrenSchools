library(ggplot2)
library(sf)
library(haven)

# paths
path <- "/SchoolClosureViolence/replication/"
data <- paste(path,"data/", sep = "")
graphs <- paste(path,"results/graphs", sep = "")

# import data
reopening <- read_dta(paste(data,"/Reopening_CH.dta", sep = ""))
reopeningRM <- read_dta(paste(data,"/Reopening_RM.dta", sep = ""))

# import shapefiles
map1 <- st_read(paste(data,"/shapefiles/Regional.shp", sep = ""))
colnames(map1)[3] <- "region"
map2 <- st_read(paste(data,"/shapefiles/Comuna.shp", sep = ""))
colnames(map2)[4] <- "comuna"

# merge datas
map1 <- merge(map1, reopening, by.x = "region", by.y = "region")
map2 <- merge(map2, reopeningRM, by.x = "comuna", by.y = "comuna")

dates = c("05aug2020", "10aug2020", "17aug2020", "24aug2020",
          "02sep2020", "07sep2020", "21sep2020", "29sep2020",
          "06oct2020", "13oct2020", "19oct2020", "26oct2020",
          "02nov2020", "09nov2020", "16nov2020", "23nov2020",
          "30nov2020", "07dec2020", "14dec2020", "05mar2021",
          "08mar2021", "15mar2021", "22mar2021", "29mar2021",
          "05apr2021", "12apr2021", "19apr2021", "26apr2021",
          "03may2021", "10may2021", "18may2021", "24may2021",
          "31may2021", "07jun2021", "14jun2021", "22jun2021",
          "29jun2021", "05jul2021", "26jul2021", "02aug2021",
          "09aug2021", "16aug2021", "23aug2021", "30aug2021",
          "06sep2021", "13sep2021", "20sep2021", "27sep2021")

map1$fday <- factor(map1$fday, levels = dates)
map2$fday <- factor(map2$fday, levels = dates)

# Figure S1: Map of Chile
G1 <- ggplot() + geom_sf(data = map1, aes(fill = prop_schools), color = "black") +
      scale_fill_gradient("School Opening", low = 'white', high = 'forestgreen',
                          breaks = c(0, 0.25, 0.5, 0.75, 1)) +
      lims(x = c(-8450000, -7394977), y = c(-7536664, -1978920)) + ggtitle("") +
      theme_bw() + facet_wrap(~ fday, ncol = 16) +
      theme(strip.text.x = element_text(size = 10.5), 
            panel.grid.minor = element_blank(),               
            panel.grid.major = element_blank(),
            strip.text = element_text(size = 35),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 22),
            legend.key.size = unit(0.9, "cm"),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
ggsave(G1, file = paste(graphs,"/Chile.pdf", sep = ""), height = 14, 
       width = 17.5, dpi = 350)

# Figure S2: Map of Region Metropolitana
G2 <-  ggplot() + geom_sf(data = map2, aes(fill = prop_schools), color = "black") +
       scale_fill_gradient("School Opening", low = 'white', high = 'forestgreen',
                           breaks = c(0, 0.25, 0.5, 0.75, 1)) +
       ggtitle("") + theme_bw() + facet_wrap(~ fday, ncol = 8) +
       theme(strip.text.x = element_text(size = 18),
             panel.grid.minor = element_blank(),               
             panel.grid.major = element_blank(),
             strip.text = element_text(size = 35),
             legend.text = element_text(size = 21),
             legend.title = element_text(size = 25),
             legend.key.size = unit(0.95, "cm"),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
ggsave(G2, file = paste(graphs,"/RM.pdf", sep = ""),height = 17, width = 25, 
       dpi = 350)

