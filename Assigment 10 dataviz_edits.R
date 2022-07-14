###### ASSIGNMENT 10: Make a map of the study area#####

rm (list=ls())

#setwd
setwd("/Users/alexandrareich/Desktop/DataVIzFish/Work_dir/Assgn10 workdir")

#load stugg
library(ggplot2)
library(dplyr)
library (tidyverse)
library (cowplot)
library (ggmap)
library (mapdata) #maps and mapdata contain outlines of geographic features
library (maps)
library (viridis)
library (stringr)
library (magick)
library (sp)
library (rgdal)
library (ggspatial)
library (patchwork)
library(ggrepel)
#library(tidykml)

devtools::install_github("briatte/tidykml")
library(tidykml)


##Getting started making basic maps in ggplot
#We use ggplot's 'map_data()' function to convert 
##the map data from the 'maps' package into dataframes that ggplot can deal with; 
##e.g., 'usa'

usa <- map_data ("usa")
head (usa)

ggplot (data = usa) + aes (x = long, y = lat, group = group) + geom_polygon() + 
  coord_quickmap()


#NOT ALAKSA

w2hr <- map_data ("world2Hires")
alaska <- w2hr %>% filter (subregion == "Alaska")
ggplot (data = alaska) + aes (x = long, y = lat, group = group) + geom_polygon() + 
  coord_quickmap()


#NOT LPW
#how to zoom to LPW and Juneau...
#create a basemap
world180 <- map_data("worldHires")
studyarea <- world180 %>% filter(lat>45) %>%
  filter (long < -100) %>%
  filter (long > -160)

base.study <- ggplot (data = studyarea) + aes (x = long, y = lat) + 
  geom_polygon (aes (group = group), fill = "grey45") + 
  coord_quickmap(xlim = c(-136, -130), ylim = c(55, 58))

base.study
#MAybe I'll choose to use watercolor isntead. This is ugly


#add the arrow
arrow <- image_read ("compass-rose.png")
ggdraw (alaska.map) + draw_image (arrow, x = 0.27, y = -0.3, scale = 0.15)
#have to insert actual map name for this

###MAKE PLOTS PRETTY
#Natural Earth database: free vector and raster map data
#for rivers and shit

#using ggmap
alaskabox <- c(left = -136, bottom = 55, right = -130, top = 58)
alaska.terrain <- ggmap::get_stamenmap (bbox = alaskabox, zoom = 5, maptype = "watercolor", where = "cache")
alaska.pretty.base <- ggmap (alaska.terrain)
alaska.pretty.base


#try zoom experiment
#study.box <- c(left = -135.7, bottom = 56, right = -132, top = 58.5)
#alex.base <- ggmap::get_stamenmap (bbox = study.box, zoom = 9, maptype = "terrain-background", where = "cache")
#ggmap(alex.base)


study.box2 <- c(left = -137, bottom = 56, right = -133, top = 59)
alex.base2 <- ggmap::get_stamenmap (bbox = study.box2, zoom = 9, maptype = "terrain-background", where = "cache")
ggmap(alex.base2)
#beautiful base

#add my field sites 
field.sites <- tibble (
  long = c(-134.660727, -134.6523011, -134.7161446,-134.465883, -133.669948),
  lat = c(56.2975338, 56.3795486, 56.389321, 58.3261395,58.5487107),
  names = c("Port Armstrong Hatchery", "Sashin Creek", "Lovers Cove", "Macaulay Hatchery", "Fish Creek")
)

#convert to data frame?
field.sites2 <- as.data.frame(field.sites)
field.sites2$Species <- c("Pink", "Pink", "Pink", "Coho", "Coho")


#MAKE THE MAP
BaranofJuneau <- ggmap (alex.base2) + geom_point (data = field.sites2, aes (x = long, y = lat, shape=Species), size = 3) + 
  xlab(NULL) + ylab (NULL)+
  theme(legend.position=c(0.14, 0.13))+
  scale_x_continuous (breaks = c(-136, -135, -134, -133), labels = c("136°W", "135°W", "134°W", "133°W"), expand = c(0,0)) + scale_y_continuous (breaks = c(56, 57, 58, 59), labels = c ("56°N", "57°N", "58°N","59°N" ), expand = c(0,0))+ 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1.5)) + theme (axis.text = element_text (size = 12)) + 
  theme (plot.margin = unit(c (0,0,0,0), "cm"))
    #legend!
    

BaranofJuneau #map almost done!!

arrow <- image_read ("compass-rose.png")
ggdraw (BaranofJuneau) + draw_image (arrow, x = -.25, y = .43, scale = 0.14)

#add scale bar and north arrow

#############################################
#Ok, so adding the scale bar is complicated
study.box3 <- c(left = -137, bottom = 56, right = -133, top = 59)
alex.base3 <- ggmap::get_stamenmap (bbox = study.box2, zoom = 9, maptype = "terrain-background", where = "cache")

field.sites3 <- field.sites2[-c(1:3),]

Baranof_w_scalebar <- ggmap (alex.base3) + geom_spatial_point (data = field.sites2, aes (x = long, y = lat, shape=Species), size = 3) +
  geom_text_repel (data = field.sites2, aes (x = long, y = lat, label = names), size = 4, color = "black") + 
  scale_x_continuous (expand = c(0,0)) + scale_y_continuous (expand = c(0,0)) + theme_void() + 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1.5)) + 
  theme (plot.margin = unit(c (0,0,0,0), "cm")) + coord_sf(crs = 4326) + 
  annotation_scale(style = "ticks", location = "bl", height = unit(2, "mm"), tick_height = 1, text_cex = 1, line_width = 2)

Baranof_w_scalebar2 <- ggmap (alex.base3) + geom_spatial_point (data = field.sites2, aes (x = long, y = lat, shape=Species), size = 3) +
  geom_text_repel (data = field.sites3, aes (x = long, y = lat, label = names), size = 4, color = "black") + 
  scale_x_continuous (expand = c(0,0)) + scale_y_continuous (expand = c(0,0)) + theme_void() + 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1.5)) + 
  theme (plot.margin = unit(c (0,0,0,0), "cm"), legend.box.margin = margin(0,0,0,0)) + coord_sf(crs = 4326) + 
  annotation_scale(style = "ticks", location = "bl", height = unit(2, "mm"), tick_height = 1, text_cex = 1, line_width = 2)

Baranof_w_scalebar

Baranof_w_scalebar2 #NEW JULY
#Great

#now adjust legend and add N arrow
Legend_map<- Baranof_w_scalebar +   theme(legend.position=c(0.12, 0.13)) 

Legend_map2 <- Baranof_w_scalebar2 +   theme(legend.position=c(0.12, 0.13)) +
  theme(legend.title=element_blank(), legend.box.background = element_rect(),
        legend.box.margin = margin(0, 3, 2, 2)) #JULYNEW

Complete_Site_Map <-  ggdraw (Legend_map) + draw_image (arrow, x = -.25, y = .43, scale = 0.14)

##############################################

#for adding field sites, try chainging shape of geom_point to differentiate between wild and hatchery


#also, make a bigger map of alaska too
#alaska.box <- c(left = -137, bottom = 56, right = -133, top = 59)
#alaska.base <- ggmap::get_stamenmap (bbox = study.box2, zoom = 9, maptype = "terrain-background", where = "cache")

alaska.box2 <- c(left = -170, bottom = 54, right = -125, top = 71)
ak.base <- ggmap::get_stamenmap (bbox = alaska.box2, zoom = 6, maptype = "terrain-background", where = "cache")
ggmap(ak.base)

#box the important part
alaska.inset <- ggmap (ak.base) + geom_rect(xmin = -137, xmax = -133, ymin = 56, ymax = 59,
                                            fill = "transparent", color = "grey20", size = 0.5) + theme_void() + 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1.5)) +
  theme (plot.margin = unit(c (0,0,1,0), "mm"))

alaska.inset
#DONE, The large alaska map LOOKS GREAT!

#UGH, needs to meet afs guideslines

#plot data map with points and legend, scale bar, north arrow
#now, combine these maps somehow
# alaska.inset and Complete_Site_Map using patchwork
fish.layout <- "
AAAABB
AAAABB
AAAABB

"
Complete_Site_Map+ alaska.inset +
  plot_layout(design = fish.layout)


#######################3
#######################3?
#FURTHER TINKERING. I just need to fix the NORTH ARROW and add some margin spacing to my plots. See rmd for details

Baranof_w_scalebar2 <- ggmap (alex.base3) + geom_spatial_point (data = field.sites2, aes (x = long, y = lat, shape=Species), size = 3) +
  geom_text_repel (data = field.sites2, aes (x = long, y = lat, label = names), size = 4, color = "black") + 
  scale_x_continuous (expand = c(0,0)) + scale_y_continuous (expand = c(0,0)) + theme_void() + 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1.5)) + 
  theme (plot.margin = unit(c (0,0,0,0), "mm")) + coord_sf(crs = 4326) + annotation_north_arrow(style = north_arrow_nautical(), location="topleft")+ 
  annotation_scale(style = "ticks", location = "bl", height = unit(2, "mm"), tick_height = 1, text_cex = 1, line_width = 2)

Baranof_w_scalebar2
#Great

#now adjust legend and add N arrow
Legend_map<- Baranof_w_scalebar2 +   theme(legend.position=c(0.12, 0.13))

#Complete_Site_Map <-  ggdraw (Legend_map) + draw_image (arrow, x = -.25, y = .43, scale = 0.14)
fish.layout <- "
AABB
AABB

"
Map_final<- Legend_map + alaska.inset +
  plot_layout(design = fish.layout)
#perfect. let's save it and see what it looks like after saving

dev.new (width = 5.62, height = 3.53, units = "in", noRStudioGD = T); Map_final
ggsave ("StudyArea043021.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600)
dev.off()

dev.new (width = 7, height = 3.53, units = "in", noRStudioGD = T); Map_final
ggsave ("StudyArea0430212.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600)
dev.off()

dev.new (width = 5.62, height = 3.53, units = "in", noRStudioGD = T); Map_final
ggsave ("StudyArea0430213.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600)
dev.off()

dev.new (width = 5.62, height = 3.53, units = "in", noRStudioGD = T); Map_final
ggsave ("StudyArea0430214.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600)
dev.off()

dev.new (width = 6, height = 3.53, units = "in", noRStudioGD = T); Map_final
ggsave ("StudyArea0430215.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600)
dev.off()

dev.new (width = 5.62, height = 3.53, units = "in", noRStudioGD = T); Map_final
ggsave ("StudyArea0430217.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600)
dev.off()

dev.new (width = 6, height = 3.53, units = "in", noRStudioGD = T); Map_final
ggsave ("StudyArea0430218.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600)
dev.off()

dev.new (width = 4, height = 7, units = "in", noRStudioGD = T); Map_final
ggsave ("StudyArea0430219.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600)
dev.off()
