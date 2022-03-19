#' ---
#' title: "Cartography"
#' author: "steppe"
#' date: "2/6/2022"
#' output:
#'    pdf_document:
#'       fig_caption: true
#'       number_sections: true
#'       includes:
#'         in_header: float_free.tex
#' ---
#' 
#' \tableofcontents 
#' \listoffigures
#' \listoftables
#' 
#' # More cartography
#' 
## ------------------------------------------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages
shhh(library(tidyverse))
shhh(library(sf))
shhh(library(spData))
shhh(library(rcartocolor))
shhh(library(ggspatial))
shhh(library(ggrepel))
rm(shhh)

#' 
#' We have already covered a great number of mapping applications. For example we have plotted Rasters using both the plot() function from the Raster Package, and tmap. We have also formed grids of rasters using tmap, and we have added vector features on top of rasters. Throughout the last two scripts we have also prepared multi-panel plot's of vector data. 
#' 
## ----Download Data for Maps, warning = F, message = F, comment = "", fig.align="center"----------------------------

data(world) 
data("us_states", package = "spData")
north_carolina <-  read_sf(system.file("shape/nc.shp", package = "sf")) %>% 
  mutate(SIDS_RATE = (SID74/BIR74) * 100)

NC_places <- tigris::places("North Carolina", progress_bar = FALSE)
NC_places <- NC_places %>% 
  filter(str_detect(NAMELSAD, "city")) %>% # too many! 
  filter(str_detect(NAMELSAD, "Asheville|Raleigh|Charlotte|Greensboro")) %>% 
  ## oops there was a Name colummn....
  dplyr::select(NAME) %>% 
  st_centroid() %>% 
    mutate(longitude = unlist(map(.$geometry,1)),
           latitude = unlist(map(.$geometry,2)))

nc_bb <- st_transform(north_carolina, 32017) %>% 
  st_buffer(150000) %>% 
  st_bbox()
nc_bb <-  nc_bb %>%
  st_as_sfc() %>%
  st_transform(crs = 4326) 

nc_bb1 <- st_bbox(nc_bb)
world <- world %>% st_crop(nc_bb)


#' 
#' ### Chloropeth
#' 
#' As R is primarily used for statistics and numerical analysis, if one is creating a map in here, I presume you are trying to express a property. 
## ----Vector Chloropeth map, fig.cap = "Vector Chloropeth map", fig.align="center"----------------------------------

ggplot() + 
  geom_sf(data = north_carolina, aes(fill = SIDS_RATE)) +
  scale_fill_carto_c(palette = "Burg") +
  theme_light() +
  labs(fill="Birth Rate 1974",
       title = "All Births with SIDS 1974") +
  theme(plot.title = element_text(hjust = 0.5))


#' 
#' ## Vector With Cartographic Elements
#' 
## ----Chloropeth Map with Cartographic Elements, fig.cap = "Chloropeth Map with Cartographic Elements", fig.align="center"----

ggplot() + 
  geom_sf(data = us_states, fill = "moccasin") +
  geom_sf(data = north_carolina, aes(fill = SIDS_RATE)) +
  geom_sf(data = NC_places) +
  scale_fill_carto_c(palette = "Burg") +
  theme_classic() +
  labs(fill="Proportion", title = "All Births with SIDS 1974") +
  
  # Define extent of map
  coord_sf(xlim = c(nc_bb1[1],nc_bb1[3]), ylim = c(nc_bb1[2],nc_bb1[4])) +
  
  # Add the Ocean to the map, add labels for cities.
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "royalblue1")) +
  geom_text_repel(data = NC_places, aes(x = longitude, y = latitude, label = NAME),
                  nudge_x = c(-0.5, -1.0, -0.5, 0.5), 
                  nudge_y = c(-0.5,  0.5,  1.0, 1.3)) +
  
  # Add Scale bar and North Arrow
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
        pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
        style = north_arrow_fancy_orienteering)


#' 
#' ### Insets
#' 
#' Inset maps are commonly used to specify the locality of a study area. For example, if it would be beneficial for the audience to understand where North Carolina is located within the US, setting the extent of the finer map on the larger map in an inset pane can help.
#' 
## ----Vector Inset Map, fig.cap = "Chloropeth Map with an Inset", fig.align="center"--------------------------------

# Create the map of the USA
geom1 <- ggplot() + 
  geom_sf(data = us_states, fill = "white") + 
  geom_sf(data = nc_bb, fill = NA, color = "red", size = 1.2) +
  theme_void()

#Create the map of NC
geom2 <- ggplot() + 
  geom_sf(data = north_carolina, aes(fill = SIDS_RATE)) +
  scale_fill_carto_c(palette = "Burg") +
  theme_void() +
  labs(fill="Proportion", title = "All Births with SIDS 1974")+
  theme(legend.position = c(0.25, 0.1),
        plot.title = element_text(hjust = 0.7),
        legend.direction = "horizontal",
        legend.key.width = unit(10, "mm"))

cowplot::ggdraw() +
  cowplot::draw_plot(geom2) +
  cowplot::draw_plot(geom1, x = 0.04, y = 0.7, width = 0.3, height = 0.3)

#' 
#' ### Backgrounds
#' 
#' We can also add imagery to the background of a map. 
#' 
## ----Vector Map Background, cache = T, fig.cap = "Chloropeth Map with a Basemap", fig.align="center", message = F, warning = F----
site_bbox <- north_carolina %>% 
  st_buffer(250) %>% 
  st_transform(4326) %>% 
  st_bbox(north_carolina)
names(site_bbox) <- c('left', 'bottom', 'right', 'top')

basemap <- ggmap::get_stamenmap(site_bbox, zoom = 8, maptype = "terrain")

ggmap::ggmap(basemap) +
  geom_sf(data = north_carolina,
          alpha = 0.8,
          inherit.aes = FALSE, aes(fill = SIDS_RATE)) +
  coord_sf(crs = st_crs(4326))  +
  scale_fill_carto_c(palette = "Burg") +
  theme_void() +
  labs(fill="Proportion", title = "All Births with SIDS 1974")+
  theme(legend.position = c(0.235, 0.1),
        legend.background = element_rect(fill="cornsilk2", size=.5),
        plot.title = element_text(hjust = 0.5),
        legend.direction = "horizontal",
        legend.key.width = unit(10, "mm"))

#' 
## ----Clear environment, echo = F, warning = F----------------------------------------------------------------------
rm(geom1, geom2, nc_bb, nc_bb1, NC_places, nc1, north_carolina, world, us_states, shhh, basemap, site_bbox)

#' 
#' # Works Cited
#' 
#' https://geocompr.github.io/post/2019/ggplot2-inset-maps/ Accessed 1.22.2022
#' 
#' Moreno, M., Basille, M. https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html Accessed 1.22.2022
#' 
