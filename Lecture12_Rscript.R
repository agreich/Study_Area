#Lecture 12 - making pretty maps and compound figures

library (tidyverse)
library (cowplot)


library (ggmap)
library (mapdata)
library (maps)
library (viridis)
library (stringr)
library (ggsn)
library (magick)
library (sp)
library (rgdal)

#I do not know much about shapefiles; their structure, where to find the right ones, how to make them work
#but for very basic mapping, here's how you can add features from natural earth data to a map:
#let's make a map of Alaska with some of its major rivers
# repeating some of what we learned on Monday....
world <- map_data ("worldHires")
alaska <- world %>% filter (lat >=50) %>%
  filter (long >= -180) %>%
  filter (long <= -125)

ggplot (alaska, aes (x = long, y = lat, group = group)) + geom_polygon (fill = "grey70") + coord_quickmap()

#download rivers shapefiles (medium scale) from Natural Earth Data, unzip, and store in your working directory 
# https://www.naturalearthdata.com/downloads/50m-physical-vectors/

#note: for code below, you'll need to type your correct filepath
ne.rivers <- rgdal::readOGR ('L12_maps/ne_50m_rivers_lake_centerlines_scale_rank/ne_50m_rivers_lake_centerlines_scale_rank.shp', 
                             layer = 'ne_50m_rivers_lake_centerlines_scale_rank') 

ak.river.map <- ggplot (data = alaska) + aes (x = long, y = lat, group = group) + geom_polygon(fill = "grey70") + 
  geom_path (data = ne.rivers, color = "#538AB1") + 
  coord_quickmap(xlim = c(-180, -128), ylim = c(50, 73))

#let's add some lakes:
ne.lakes <- rgdal::readOGR ('L12_maps/ne_50m_lakes/ne_50m_lakes.shp', layer = 'ne_50m_lakes')
ak.river.map + geom_polygon (data = ne.lakes, fill = "#538AB1")

#let's zoom in on Bristol Bay: you can see it's pretty coarse scale
ak.river.map + geom_polygon (data = ne.lakes, fill = "#538AB1") + coord_quickmap (xlim = c(-170, -150), ylim = c(57,62))

#let's try the large data from NE (1:10m)
ne.rivers10 <- readOGR ('L12_maps/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp') 
ak.rivers.10m.map <- ggplot (data = alaska) + aes (x = long, y = lat, group = group) + geom_polygon(fill = "grey70") + 
  geom_path (data = ne.rivers10, color = "#538AB1") + 
  coord_quickmap(xlim = c(-180, -128), ylim = c(50, 73))

#now let's add the "North American supplement" for rivers
ne.rivers.na10 <- readOGR ('L12_maps/ne_10m_rivers_north_america/ne_10m_rivers_north_america.shp') 
m2 <- ak.rivers.10m.map + geom_path (data = ne.rivers.na10, color = "#538AB1") 

ne.lakes10 <- readOGR ('L12_maps/ne_10m_lakes/ne_10m_lakes.shp') 
m3 <- m2 + geom_polygon (data = ne.lakes10, fill = "#538AB1")

ne.lakes.na10 <- readOGR ('L12_maps/ne_10m_lakes_north_america/ne_10m_lakes_north_america.shp')
final.ak.map <- m3 + geom_polygon (data = ne.lakes.na10, fill = "#538AB1")

#**************************************************** make pretty maps with Stamen map **************************************

#making pretty maps - by getting terrain and watercolor backgrounds from Stamen map
#maps.stamen.com

#first, let's create a lat/long box for Alaska
alaskabox <- c(left = -180, bottom = 50, right = -125, top = 73)
#then, let's grab a terrain background from stamen map using the ggmap package
alaska.terrain <- ggmap::get_stamenmap (bbox = alaskabox, zoom = 5, maptype = "terrain-background", where = "cache")
alaska.pretty.base <- ggmap (alaska.terrain)

alaska.pretty.base + geom_path (data = ne.rivers10, aes (x = long, y = lat, group = group), color = "#538AB1") 

#we could also get stamen map's watercolor background:
alaska.watercolor <- ggmap::get_stamenmap (bbox = alaskabox, zoom = 5, maptype = "watercolor", where = "cache")
ggmap (alaska.watercolor)

#and we can add layers using ggplot commands just as we have in the past
some.ak.places <- tibble (
  long = c(-147.7164, -165.4064, -161.7558, -166.5332),
  lat = c(64.8378, 64.5011, 60.7922, 53.8844), 
  names = c("Fairbanks", "Nome", "Bethel", "Unalaska")
)

ak.places.pretty <- ggmap (alaska.watercolor) + geom_point (data = some.ak.places, size = 4, aes (x = long, y = lat), color = "#A9097A") +
  geom_text (data = some.ak.places, aes (x = long, y = lat, label = names), color = "#A9097A", size = 6, vjust = -1,
  nudge_x = c(0, 3, 0, -5)) + scale_x_continuous(breaks = c(-170, -150, -130), labels = c("170°W", "150°W", "130W"),
  expand = c(0,0)) + scale_y_continuous (breaks = c(60, 70), labels = c("60°N", "70°N"), expand = c(0,0)) +
  xlab ("") + ylab ("") + theme_bw(base_size = 16)

arrow <- image_read ("compass-rose.png")
ggdraw (ak.places.pretty) + draw_image (arrow, x = 0.3, y = -0.3, scale = 0.15)


#************************************* making compound map figures in cowplot ****************************************************


#let's make a field site map for TAFS, mapping the Icy Strait transect from NOAA's SECM project
#lat/long converted to decimal degrees from info in Orsi & Fergusson 2016
# https://repository.library.noaa.gov/view/noaa/19447

#we'll need 3 maps to make this figure: 1) Icy Strait stations; 2) Icy Strait relative to Alexander Archipelago;
#Alexander Archipelago relative to Alaska

#for TAFS, we'll do a 2-column figure, so the total width will be 5.62"
#probably the best way to get our figures ready, then, would be to create the figures close to right text size,
#by setting the plot window to the width we plan the plots to end up as

#referring to slide 14, we want main map = 2/3(5.62) = 3.75"
#and our other two plots to be 5.62-3.75 = 1.87" wide
#this doesn't leave any room for margins in the real plot, but that should scale close enough


#I find it's easiest to start with the largest map (in terms of area covered) so you can set the inset boxes
#to coordinates that are visible on your map
alaska.box2 <- c(left = -170, bottom = 54, right = -125, top = 71)
ak.base <- ggmap::get_stamenmap (bbox = alaska.box2, zoom = 6, maptype = "terrain-background", where = "cache")

alaska.inset <- ggmap (ak.base) + geom_rect(xmin = -137, xmax = -134, ymin = 57.5, ymax = 59,
   fill = "transparent", color = "grey20", size = 0.75) + theme_void() + 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1.5)) +
   theme (plot.margin = unit(c (0.1,0.1,0,0), "cm"))

ggsave (alaska.inset, file = "alaska.inset.tiff", width = 1.87, height = 1.55, units = "in", dpi = 600)

#next, let's make a map that matches the rectangle we placed on the alaska inset
#this is going to be our primary figure, so its width will be ~ 3.75"
alexander.box <- c(left = -137, bottom = 57.5, right = -134, top = 59)
alex.base <- ggmap::get_stamenmap (bbox = alexander.box, zoom = 9, maptype = "terrain-background", where = "cache")

#let's add our stations to this map
icy.stations <- tibble (
  long = c(-135.5293, -135.4877, -135.4442,-135.3997),
  lat = c(58.22083, 58.237, 58.25467, 58.273),
  names = c("A", "B", "C", "D")
)

archipelago <- ggmap (alex.base) + geom_point (data = icy.stations, aes (x = long, y = lat), size = 0.6, color = "grey20") + xlab(NULL) +
  ylab (NULL) + scale_x_continuous (breaks = c(-136.5, -135.5, -134.5), labels = c("136.5°W", "135.5°W", "134.5°W"), expand = c(0,0)) +
  scale_y_continuous (breaks = c(57.75, 58.25, 58.75), labels = c ("57.74°N", "58.25°N", "58.75°N"), expand = c(0,0)) +
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1.5)) + theme (axis.text = element_text (size = 12)) +
  geom_rect(xmin = -135.67, xmax = -135.25, ymin = 58.1266, ymax = 58.375,
            fill = "transparent", color = "grey20", size = 0.75) + theme (plot.margin = unit(c (0.1,0,0.1,0.1), "cm"))

ggsave (archipelago, file = "alex-archipelago-map.tiff", width = 3.75, height = 3.19, units = "in", dpi = 600)


#last map: zoomed in version of Icy Strait
icy.box <- c(left = -135.67, bottom = 58.1266, right = -135.25, top = 58.375)
icy.terrain <- ggmap::get_stamenmap (bbox = icy.box, zoom = 12, maptype = "terrain-background", where = "cache")

stations.inset <- ggmap (icy.terrain) + geom_point (data = icy.stations, aes (x = long, y = lat), size = 2, color = "grey20") +
  geom_text (data = icy.stations, aes (x = long, y = lat, label = names), size = 4, color = "grey20", vjust = 1, nudge_y = -0.01) +
  theme_void() + theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1.5)) +
  theme (plot.margin = unit(c (0,0.1,0.1,0), "cm")) +
  ggsn::scalebar (location = "bottomright", x.min = -135.66, x.max = -135.5, y.min = 58.15, y.max = 58.2,
                  dist = 4, dist_unit = "km", height = 0.15, box.fill = c("grey20", "white"), box.color = "grey20",
                  transform = TRUE, model = "WGS84", st.bottom = FALSE, st.color = "grey20", st.dist = 0.25, st.size = 4)
#(for help with scalebar, ?ggsn::scalebar)

ggsave (stations.inset, file = "stations.tiff", width = 1.87, height = 2.09, units = "in", dpi = 600)

#************** now let's assemble these using cowplot and plot_grid
#note: we have to make the subgrid first
right.pane <- plot_grid (alaska.inset, stations.inset, labels = NULL, ncol = 1, rel_heights = c(1, 1.315))

#then, we can assembe this next to our main plot
plot_grid (archipelago, right.pane, labels = NULL, rel_widths = c(2.65,1))

#we come up against a limitation here - plot_grid wants to change the distance
#between the two plots in the right pane depending on relative width of the 2 panes

#so, we're going to cheat a little and use draw_image to make our final figure!
two_pane <- plot_grid (archipelago, NULL, ncol = 2, rel_widths = c(2.1, 1))
rightupper <- image_read ("alaska.inset.tiff")
rightlower <- image_read ("stations.tiff")
ggdraw (two_pane) + draw_image (rightupper, scale = 0.38, x = 0.32, y = 0.29) + draw_image(rightlower,scale = 0.50, x = 0.32, y = -0.16) +
  draw_image (arrow, x = -0.32, y = -0.30, scale = 0.2)

ggsave (file = "final-composite-map.tiff", width = 5.62, height = 3.37, units = "in", dpi = 600)

#but, that is really tedious, and it might be a lot easier to build your composite plot within pages (Mac) or Powerpoint (PC)
#but the size and resolution can be challenging.
