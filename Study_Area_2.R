##STUDY AREA
#07/14/22
rm (list=ls())

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



usa <- map_data ("usa")
head (usa)

ggplot (data = usa) + aes (x = long, y = lat, group = group) + geom_polygon() + 
  coord_quickmap()



w2hr <- map_data ("world2Hires")
alaska <- w2hr %>% filter (subregion == "Alaska")
ggplot (data = alaska) + aes (x = long, y = lat, group = group) + geom_polygon() + 
  coord_quickmap()

#create a basemap
world180 <- map_data("worldHires")
studyarea <- world180 %>% filter(lat>45) %>%
  filter (long < -100) %>%
  filter (long > -160)

base.study <- ggplot (data = studyarea) + aes (x = long, y = lat) + 
  geom_polygon (aes (group = group), fill = "grey45") + 
  coord_quickmap(xlim = c(-136, -130), ylim = c(55, 58))

base.study

base.study.2 <- ggplot (data = studyarea) + aes (x = long, y = lat) + 
  geom_polygon (aes (group = group), fill = "grey45") + 
  coord_quickmap(xlim = c(-136, -132), ylim = c(56, 59))
base.study.2




arrow <- image_read ("compass-rose.png")
ggdraw (base.study.2) + draw_image (arrow, x = 0.27, y = -0.3, scale = 0.15)
#base.study.2 is a good option



###MAKE PLOTS PRETTY

#using ggmap
alaskabox <- c(left = -136, bottom = 55, right = -130, top = 58)
alaska.terrain <- ggmap::get_stamenmap (bbox = alaskabox, zoom = 5, maptype = "watercolor", where = "cache")
alaska.pretty.base <- ggmap (alaska.terrain)
alaska.pretty.base



study.box2 <- c(left = -137, bottom = 56, right = -133, top = 59)
alex.base2 <- ggmap::get_stamenmap (bbox = study.box2, zoom = 9, maptype = "terrain-background", where = "cache")
ggmap(alex.base2)
#beautiful base #looks solid base for my purposes

#add my field sites 
field.sites <- tibble (
  long = c(-134.660727, -134.6523011, -134.7161446,-134.465883, -133.669948),
  lat = c(56.2975338, 56.3795486, 56.389321, 58.3261395,58.5487107),
  names = c("Port Armstrong Hatchery", "Sashin Creek", "Lovers Cove", "Macaulay Hatchery", "Fish Creek")
)

field.sites2 <- as.data.frame(field.sites)
field.sites2$Species <- c("Pink", "Pink", "Pink", "Coho", "Coho")


BaranofJuneau <- ggmap (alex.base2) + geom_point (data = field.sites2, aes (x = long, y = lat, shape=Species), size = 3) + 
  xlab(NULL) + ylab (NULL)+
  theme(legend.position=c(0.14, 0.13))+
  scale_x_continuous (breaks = c(-136, -135, -134, -133), labels = c("136°W", "135°W", "134°W", "133°W"), expand = c(0,0)) + scale_y_continuous (breaks = c(56, 57, 58, 59), labels = c ("56°N", "57°N", "58°N","59°N" ), expand = c(0,0))+ 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1.5)) + theme (axis.text = element_text (size = 12)) + 
  theme (plot.margin = unit(c (0,0,0,0), "cm"))#+
  #theme(legend.title = NULL)
#legend!

#I wand to change:
#coho and pink to coho collection sites and pink collection sites
#zoom in on pink, make additional map
#big map of alaska

BaranofJuneau 
arrow <- image_read ("compass-rose.png")
ggdraw (BaranofJuneau) + draw_image (arrow, x = -.25, y = .43, scale = 0.14)



#new map, grey


#ugh. Start fresh
#big alaska map
world <- map_data ("worldHires")
alaska <- world %>% filter (lat >=50) %>%
  filter (long >= -180) %>%
  filter (long <= -125)

just_alaska <- ggplot (alaska, aes (x = long, y = lat, group = group)) + geom_polygon (fill = "grey70") + coord_quickmap()


#make a compound figure

