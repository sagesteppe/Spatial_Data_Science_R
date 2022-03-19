#' ---
#' title: "R Data Science: Spatial Data Science 1"
#' author: "steppe"
#' date: "2/14/2022"
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
#' Assigned Reading: Geocomputation with R Chapter 2.
#' 
#' # Example Data
#' 
#' ## Libraries
#' 
## ----Load Libraries, message = F, warning = F, echo = F------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages 

shhh(library(sp))
shhh(library(sf))
shhh(library(raster))
shhh(library(tidyverse))
shhh(library(terra))

rm(shhh)

#' 
#' ## Example Data Methods 
#' 
#' I used the United States Geological Surveys 'Earth Explorer' to view images taken from the Sentinel 2 Satellite over the last year at a location on the border of California and Nevada near Reno. I downloaded several images which both A) covered the area of analysis well, and B) did not have much cloud cover. I used the Open Source QGIS, which has a graphical user interface (GUI), to manually geo-reference the images to locations on the earths surface. I then manually marked the edges of bodies of water, and created polygons which cover them. These data were saved as polygon vector files, in a format know as a 'shapefile' and imported them to R. 
#' 
#' These steps can all be done programmatically, but I only wanted a few good images, so went through the process by hand. 
#' 
#' Here we import our vector data, immediately create a 'Simple Feature' object, and add some attributes regarding the data
## ----import Vector Data, echo = F, warning = F---------------------------------------------------------------------------------------
# getwd()
path <- './spatial_lecture_data/sentinel_imagery'
files.shp <- list.files(path, ".shp")

dec_lakes_v <- read_sf(paste0(path,"/",files.shp), quiet = T) %>% 
  mutate(Data_source = 'Sentinel2') %>% 
  mutate(Processing = 'DL from Earth Explorer, manually georeferenced, and mapped') %>% 
  mutate(Date = as.POSIXct('2021/12/05', '%Y/%m/%d', tz = "US/Pacific-New"))

st_precision(dec_lakes_v) <- 50 # note I am saying each of the points I drew were within 50 meters of the true location. This is not important but I populate for an example below. We are in meters because we are in UTM. 

oct_lakes_v <- dec_lakes_v %>% filter(id %in% c(1,2,3,5,6,12)) %>% 
  mutate(Date = as.POSIXct('2021/10/16', '%Y/%m/%d', tz = "US/Pacific-New"))# gui's are confusing. 
# these bodies of water were not present in the earlier time period, but I am too stupid to make the GUI work and so remove them programmatically.

rm(files.shp, oct_lakes_v)

#' 
#' We import the Sentinel 2 images here. We will use them as a template to create a raster. 
## ----import Raster data, echo = F, warning = F, message = F--------------------------------------------------------------------------

files.tif <- list.files(path, ".tif")
dec_lakes_r <- raster(paste0(path,"/",files.tif[1]), )
oct_lakes_r <- raster(paste0(path,"/",files.tif[3]))

#crs(dec_lakes_r)
#newproj <- CRS("+proj=utm +zone=10 +datum=WGS84")
#dec_lakes_r <- projectRaster(dec_lakes_r, crs=newproj)


rm(files.tif)

#' 
#' We will very quickly reclassify the input images to populate our new example rasters with data. 
## ----Quickly Reclassify Rasters for an example, echo = F-----------------------------------------------------------------------------
reclass_first <- matrix(
  c(0, 1, NA,
    1 ,50, 0,
  50, 250, 1),
        nrow = 3, 
        ncol = 3,
       byrow = T) 

dec_lakes_r_classified_c <- reclassify(dec_lakes_r, reclass_first)
oct_lakes_r_classified_c <- reclassify(oct_lakes_r, reclass_first)

#plot(dec_lakes_r_classified_c) # check this has an intermediate 
# step we will be able to remove some of
#barplot(dec_lakes_r_classified_c,
#        main = "Number of pixels in each class")

dec_lakes_r_classified_f <- aggregate(dec_lakes_r_classified_c, 7.0)
oct_lakes_r_classified_f <- aggregate(oct_lakes_r_classified_c, 7.0)

reclass_second <- matrix(
  c(0, 0.5, 0,
    0.5, 1, 1),
        nrow = 2, 
        ncol = 3,
       byrow = T)

dec_lakes_r_classified_f <- reclassify(dec_lakes_r_classified_f, reclass_second)
oct_lakes_r_classified_f <- reclassify(oct_lakes_r_classified_f, reclass_second)

rm(reclass_first, reclass_second, dec_lakes_r_classified_c, oct_lakes_r_classified_c, oct_lakes_r)


#' 
## ----Define some raster helper functions, echo = F-----------------------------------------------------------------------------------
cell_count <- function(x){
  obj_name <- deparse(substitute(x))
  writeLines(paste0('This ', obj_name, ' contains: ', nrow(x) * ncol(x), ' elements'))
}

resolution_of_raster <- function(x){
  writeLines(paste0('The width of each raster cell is: ',
               round((x@extent@xmax - x@extent@xmin)/x@ncols, 5),
               ' meters'))
  writeLines(paste0('The height of each raster cell is: ',
               round((x@extent@ymax - x@extent@ymin)/x@nrows, 5),
               ' meters'))
}


#' 
#' We will create empty example rasters. 
## ----Create Empty Raster and Populate, echo = F, comment = ""------------------------------------------------------------------------

ext_rast <- st_bbox(dec_lakes_r)
grid <- st_make_grid(ext_rast,  # create a fishnet to emulate the idea of an empty raster ! :-)
  n = c(118, 118),
  what = "polygons",
  square = TRUE,
  flat_topped = FALSE)
# st_write(grid, 'Example_HoneyLake_Grid.shp')

empty_raster <- raster(
  # A raster is composed of four perpendicular lines.
  # Here we define each 'edge' of the raster'
                         xmn = 697129.7, 
                         xmx = 811775.7, 
                         ymn = 4388466,
                         ymx = 4502382,
                         
                         nrows = 118, # we are creating 100 cells.  
                         ncols = 118, # We can calculate the resolution of these below.
                         
                         crs = "+proj=utm +zone=10 +datum=WGS84", 
                         # set the rasters Coordinate Reference System
                         )

rast_vals_num <- as.integer(as.vector(as.numeric(dec_lakes_r_classified_f@data@values)))

raster_matrix <- matrix(rast_vals_num, # fill matrix with values, 
                       nrow = empty_raster@nrows, # create matrix same dimensions as raster
                       ncol = empty_raster@ncols, # create matrix same dimensions as raster
                       byrow = T) #ensure filling of matrix goes from upper left to lower right.

example_raster_dec <- setValues(empty_raster, raster_matrix)
example_raster_oct <- setValues(empty_raster, oct_lakes_r_classified_f@data@values )
# plot(example_raster_dec)
# plot(example_raster_oct)

fake_data <- matrix(c(0,1,1,0,1,0),
                    nrow = 6,
                    ncol = 8,
                    byrow=T)

rm(fake_data, ext_rast, grid)

#' 
#' # What is a Geographic Information System?
#' 
#' ## What is a GIS
#' 
#' - **G**eographic **I**nformation **S**ystem is a system for producing, managing, displaying, and analyzing geographic information.
#' - Spatial Data Science is an emergent field which utilizes data science approaches in a **GIS**, and is a natural extension of a **GIS**
#' 
#' When we think about it, nearly all data has a spatial dimension, it just tends to be ignored in much of science. Historically this is sensible as performing these analysis is computationally expensive, and requires considerable expertise. 
#' 
#' 
## ---- out.width = "35%", fig.show='hold', echo = F, fig.align="center", fig.cap = "A Visualization of a GIS, by Anne Sexton"---------
knitr::include_graphics("./pictures/USGS_GIS_Anne_Sexton.jpg")

#' 
#' This is I think really one of the best and simplest way's of showcasing at heart what a GIS is. A GIS is a system wherein we can explore and study the relationships of different attributes on a process in a spatial context.
#' 
#' ## A Brief History
#' 
#' - Dr. John Snow suspected Cholera was spread by water
#' - 1854 outbreak of Cholera in the Soho neighborhood of London kills 616 persons
#' - Snow used both *mapping* and *statistics* to identify the water source and stop the outbreak.
#' - Essentially founded both Epidemiology and GIS, and stopped the outbreak in one stroke
#' 
## ---- out.width = "35%", fig.show='hold', echo = F, fig.align="center", fig.cap = "The Broad Street Pump, A Cholera Outbreak in London, By: John Snow and digitized by National Geographic"----
knitr::include_graphics("./pictures/John_Snow_Cholera_by_National_Geographic.jpg")

#' 
#' A good article with a quote by Tufte, anXKCD, and meaningful insight by a journalist at the Guardien is below:
#' 
#' https://www.theguardian.com/news/datablog/2013/mar/15/john-snow-cholera-map
#' 
#' - Since then GIS has continually made more use of computers and come to be it's own discipline as computers have become more available. 
#' 
#' ## What is Spatial Data Science?
#' 
#' - The application of Geographic insights, and geospatial analyses to big data sets
#' - Including spatial terms in statistical models
#' - Bringing Geographic information to data science questions
#' - Developing your own spatial products and pipelines
#' 
#' ## Why Use R as a GIS and for Spatial Data Science ?
#' 
#' - Work Flow Automation
#' - Rich Ecosystem (packages, functions, code-sharing, etc.)
#' - Reproducible
#' - Self Documenting
#' - Computationally Efficient
#' - Parallel Processing/HPC interfaces
#' 
#' There is a great range of computer Geographic Information Systems to choose from. Many of you are likely to be familiar with a software program call 'ArcGIS' and to a lesser extent 'ArcMap' produced by the ESRI company. These products are widely used in nearly all branches of the federal and many state governments, as well as at many large companies especially environmental consultants. I think that this is a good system for you to learn about geospatial operations and workflows in, but nearly all of the work that can be accomlished in ESRI products can be accomplished in R. If you go into several environmental fields, you will need to learn to interface with ESRI products. I advise you to familiarize  yourself with them.
#' 
#' On the other hand, I encourage you all to use R as the central piece of your geospatial analysis. If you are not developing new spatial products, you can run all of your analyses in R. If you are interested in making maps - cartography, R is now quite capable for making publication quality maps, as well as for reports and projects. It does lack slightly in cartography aspects, but you can create the data you want to style in another software, such as the open source QGIS, here and export it. But as we will see, cartography is not equivalent to GIS, and GIS is not equivalent with  cartography. Do not expect to be able to make *National Georgraphic* quality maps no matter which GIS you use. 
#' 
#' If you are very interested in geospatial work than you will see that you use a number of software programs to perform your research. In most cases you will still keep R as the centerpiece, but will likely work within a Linux environment and make good use of Unix and Bash scripts, Python, and QGIS to help fill in some parts of work-flows which R cannot address as well. You will find that R, Python, and QGIS all use many of the same software components and libraries such as GDAL/OGR, GEOS, PROJ, GRASS GIS, which have been developed by the Open Source Geospatial Foundation which was actually founded in Chicago around 20 years ago. 
#' 
#' We will draw heavily from the Open Source Geospatial Foundation in these lectures.
#' 
#' # Geodesy 
#' 
#' - 'Geodesy is the science of accurately measuring and understanding the Earth's geometric shape, orientation in space, and gravity field.' - NOAA
#' 
#' This is a very highly specialized field, of which I have no formal training in when it comes to the theory of it. I do believe we have a couple specialists in Earth and Planetary Sciences, but I am far from them. Here I will cover the most basic facets of this field as they pertain to geospatial analysis among non-specialists. A more apt title for this section could be *'Geodesy taught by a dummy'*
#' 
#' ## Earth is not a **perfect** sphere
#' 
#' - Circumference at equator:	40,075 km (24,901 mi)
#' - Circumference along meridians: 40,009 km (24,860 mi)
#' 
#' Earth is not a sphere; it is ever so slightly longer than wide. The earth is actually around 43 km/27miles wider at the equator. Note the average radius of the earth is around 6371 km/3959 mi so this is quite small! However, trying to represent the Earth as a perfect sphere in geospatial models will lead to inaccurate representation of the location of objects on it.
#' 
## ---- out.width = "35%", fig.show='hold', echo = F, fig.align="center", fig.cap = "Blue Marble 2012, by Suomi NPP", fig.asp=0.50-----
knitr::include_graphics('./pictures/NASA_Blue-Marble_2012_Suomi_NPP.jpg')

#' 
#' ### The Earth can be represented as an Ellipsoid
#' 
#' - A simple 3d geometric shape
#' - An ellipsoid is a slightly to greatly ovaliform shape
#' - Modeling the Earth as an ellipsoid increases the accuracy of point locations
#' 
#' Because the earth is slightly wider than long, the earth is technically and can be modeled practically as an ellipsoid. This model of representing the earth assumes no terms of gravity, winds, or tides. I.e. it is a perfect geometric shape with bilateral symmetry. 
#' 
#' You can imagine that there are certain limitations to the accuracy of our model of the earth without including gravity.
#' 
## ---- out.width = "50%", fig.show='hold', echo = F, fig.align="center", fig.cap = "Ellipsoids", fig.asp=0.50-------------------------
knitr::include_graphics('./pictures/Spheroids.png')

#' 
#' ### However, the Earth's surface is not smooth - Geoid
#' 
#' - Types of models to represent planet Earth
#' - Include gravity, excluding winds and tides
#' - Highly accurate since application of GPS technology
#' 
#' The Earth can also be represented as a geoid, which is a model of earth still lacking the effects of wind and tides on the earth – but which includes the major force - gravity. Because the effect of gravity is retained, the earths overall shape is both elliptical however the surface is irregular in regard to distance from the center. The surface of the Geoid is oftentimes roughly equivalent to what we could call a global mean sea level.
#' 
## ---- out.width = "35%", fig.show='hold', echo = F, fig.align="center", fig.cap = "Left: Geoid Cross Section. Right: IGCM Geoid"-----
knitr::include_graphics('./pictures/geoid_cross_section.png')
knitr::include_graphics('./pictures/Geoid_undulation_10k_ICGEM.jpg')

#' 
#' Oftentimes now, very finely resolved geoids are combined with satellite measurements, and these observations are then fit with ellipsoid models, which we then use as our models of the earths surface.
#' 
#' By International Centre for Global Earth Models (ICGEM) - http://icgem.gfz-potsdam.de/vis3d/longtime / Ince, E. S., Barthelmes, F., Reißland, S., Elger, K., Förste, C., Flechtner, F., Schuh, H. (2019): ICGEM – 15 years of successful collection and distribution of global gravitational models, associated services and future plans. - Earth System Science Data, 11, pp. 647-674,DOI: http://doi.org/10.5194/essd-11-647-2019., CC BY 4.0, https://commons.wikimedia.org/w/index.php?curid=81462823
#' 
#' ## Geodetic Datums
#' 
#' - Reference frame established to represent locations within the frame. 
#' - Historically these were locally focused and based on Geoids. 
#' - A datum can serve either horizontal (X & Y) or vertical (Z) features. 
#' 
#' - Components:
#'     - reference ellipsoid or geoid
#'     - origin point (from which measurements run)
#'     - control points very strictly measured from the origin, 
#'   
#' Other locations are then measured in relation to the control points. 
#' 
## ---- out.width = "50%", fig.show='hold', echo = F, fig.align="center", fig.cap = "Number one City Datum. by: Cosmo1976", fig.asp=0.50----
knitr::include_graphics("./pictures/Number_One_City_Datum_Cosmo1976.jpg")

#' When we think of the Origins of datum points, the Equator and Prime Meridian are the most famous. However, there have been a great many datums in history. 
#' 
## ----Map of Chicago Datum, fig.align="center", fig.cap = "Control Points Number one City Datum", out.width = "75%", echo = F---------
chicago_cntrl_pts <- read_sf('./spatial_lecture_data/Elevation_Benchmarks.csv')[c(1:4,8:12)] %>% 
  st_as_sf(coords = c(y = 'LONGITUDE', x = 'LATITUDE'), crs = 4326) %>% 
  janitor::clean_names() %>% 
  mutate(elevation = as.numeric(elevation))

origin_point <- chicago_cntrl_pts %>% 
  filter(benchmark_number == 1)

files <- paste0('./spatial_lecture_data/Chicago_Neighborhoods/' , 
                list.files('./spatial_lecture_data/Chicago_Neighborhoods', ".shp"))
chicago_nghbrh <- read_sf(files)

ggplot() +
  geom_sf(data = chicago_nghbrh, alpha = 0.4) +
  geom_sf(data = chicago_cntrl_pts, aes(color = elevation)) +
  geom_sf(data = origin_point, shape = 8, size = 3, color = "red") +
  labs(title = 'Control Points for Number One City Datum') +
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = c(0.001, 0.001), legend.justification = c(0, 0), 
        legend.box.background = element_rect(color="black", size=1), 
        legend.box.margin = margin(0, 0, 4, 0))

rm(chicago_cntrl_pts, files, chicago_nghbrh, origin_point)

#' 
#' In fact the city of Chicago established it's own elevation datum in the late 1920's. If we look at this quick map, we can see where the Origin point (also pictured) and control points are located.
#' 
#' Also do you really think Chicago is only 80 feet at most above sea level? No of course not! The lowest elvations in the city are roughly 580 feet above sea level, but in the 1920's, it was much easier to define elevation from an adjacent location. So all the values in the map should be X + ~580 ! This is exactly why so many datums are out there. 
#' 
#' ### Geodetic Coordinates
#' 
#' Since our area is 3-dimensional, we use a system related to Cartesian coordinates, however given the curve of the earths surface curvilinear rather than linear coordinates are used.
#' 
#' "The geodetic (or geographic) latitude is the angle between the equatorial plane and the normal (vertical) to the ellipsoid surface at the considered point." - Proj
#' 
#' - phi = geodetic latitude (north/south)
#' - lambda =longitude (east/west)
#' - *h* = ellipsoidal height 
#' - N = Normal (A plane at a right angle to the surface of the ellipsoid)
#' 
#' In the sphere image we have a latitude of 40*, but we do not know to which hemisphere it is associated with. 
#' 
## ---- out.width = "25%", fig.show='hold', fig.align="center", echo = F, fig.cap = "Angles on Ellipsoid and Geodetic Coordinates. by: Peter Mercator"----
knitr::include_graphics("pictures/Geodetic_Coordinates1_Peter_Mercator.png")
knitr::include_graphics("pictures/geodetic_coordinates_Peter_Mercator.png")

#' 
#' By Peter Mercator - Own work, CC BY-SA 3.0,  https://commons.wikimedia.org/w/index.php?curid=17717979
#' 
#' ### Coordinate Notation
#' 
#' - Trigonometric
#'   - Degrees Minutes Seconds (DMS), e.g. 42°03'27.7" N
#'     - Sexagesimal/base 60 (think: minutes in an hour) 
#'   - Decimal Degrees (DD), e.g. 42.05759 N
#'     - Decimal Fractions of a Degree (portion of 360/2)
#'     
#' - DMS; seldom used in digital formats
#' - DD; almost exclusively used
#'   
#' 
#' Recorded in many forms DMS, DD, (trigonometry) UTM.
#' 
#' As a circle has 360 degrees, so does the earth. Given the nature of datums we split the world into a positive and negative sign for both latitude and longitude; we refer to the 0 lines as '0 meridians'. We have 180 positive and 180 negative degrees, and as these are of a course resolution we retain the decimals of degrees to better locate objects. As you all know, the Western Hemisphere is the negative component of a 180 degree half.  
#' 
#' 
#' $$
#' \text{Decimal Degrees} = \text{Degrees } + \frac{\text{Minutes}}{60} + \frac{\text{Seconds}}{3600} 
#' $$
#' $$
#' \text{Latitude of Tech} =  42°03'27.7" N
#' $$
#' 
#' $$
#' 42 + (\frac{03'}{60}) + (\frac{27.7"}{3600})
#' $$
#' 
#' $$
#' 42 + 0.05 + 0.007694 = 42.05759 \text{ Decimal Degrees}
#' $$
#' 
#' - Universal Transverse Mercator (UTM)
#'   - Divides the world into 60 zones
#'   - Flattens each zone
#'   - Measures distances in Meters
#'   
#' UTM - Common for field work, planar measurements of meters. 
#' 
## ----UTM Zones of the Continental USA,   fig.align="center", echo = F, message = F, warning=F, fig.cap = "UTM Zones of the Conterminous USA. data by ESRI"----

utm_grid <- read_sf('./spatial_lecture_data/World_UTM_Grid/0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1.shp') %>% 
  filter(ZONE > 9 & ZONE < 20) %>% 
  filter(ROW_ %in% LETTERS[18:21]) %>% 
  group_by(ZONE) %>% 
  summarize(geometry = st_union(geometry))

bound <- st_bbox(utm_grid)
bound <- c(bound[1]+2,
            bound[2]+2,
            bound[3]-2,
            bound[4]-2)

world <- st_read(system.file("shapes/world.gpkg", package="spData"), quiet = T)

ggplot(utm_grid) +
  geom_sf(data = world) +
  geom_sf(fill = NA, color = "black") +
  geom_sf_label(aes(label = ZONE)) +
  theme_classic() +
  labs(title="UTM Zones of the Continental USA") +
  coord_sf(xlim = c(bound[1],bound[3]), ylim = c(bound[2],bound[4])) +
  theme(plot.title = element_text(hjust = 0.5))

rm(utm_grid, bound, world)

#' 
#' ## Coordinate Reference System
#' 
#' - System for specifying a location on Earth's surface
#' - Composed of:
#'   - a model of earths shape (Geoid or Ellipsoid)
#'   - geodetic datum
#'   - generally also a projection
#' 
## ---- out.width = "40%", fig.align="center", fig.show='hold', echo = F, fig.cap = "Geographic Coordinate Reference System. by: Anna Krystalli"----
knitr::include_graphics("pictures/Geographic_Coordinate_Reference_System.jpg")

#' 
#' ## Problems with working on flat surfaces
#' 
#' 3D to 2d does not work well. But we have worked around this. **sorta**
#' 
## ---- out.width = "50%", fig.align="center", fig.show='hold', echo = F, fig.cap = "Orange Peel. by: Nathan Belz"---------------------
knitr::include_graphics("pictures/Orange_peel_Nathan_Belz.png")

#' 
#' ### Geographic Coordinate System 
#' 
#' - Location on a three-dimensional model of Earths Surface
#' 
#' Where the location is on located earth. We are able to pinpoint locations, quite easily using one of the Global Navigation Satellite Systems, for example the 'Global Positioning System' or GPS. We have a coordinate system for this known as the World Geographic Datum. As we are merely recording the location of an object on a 3d object this is now essentially a trivial process. 
#' 
#' ### Projected Coordinate System 
#' 
#' - Location on a two-dimensional model of Earths Surface
#' 
#' Most of us work on flat desks and flat computer screens. We need to represent locations in space on these surfaces. We project coordinates from their geographic locations on earth to locations on flat representations of earth.
#' 
#' ### Major Map Projections
#' 
#' Many different ways to create two dimensional maps – thousands of projections. None are perfect. There are four main types of projections, each with their own strengths and examples. One of the most common examples of each of these is included in the table below. 
#' 
#' - Equal Area: Lambert Cylindrical equal-area
#'   - Pro: No distortion of area near equator (here)
#'   - Con: Distorts area at the Polar regions
#' - Equal Distance: Equi-rectangular (plate carrée projection)
#'   - Pro: Looks good in mapping applications
#'   - Con: Distorts both shape and directions
#' - Conformal: 
#'   - Pro: Boundaries are accurate
#'   - Con: Distorts Polar area
#' - Compromise: 
#'   - Pro: Sensitive to both area and direction
#'   - Con: Sensitive to both area and direction
#' 
## ---- out.width = "75%", fig.align="center", fig.show='hold', echo = F, fig.cap = "Map Projections. by: Daniel Strebe"---------------
knitr::include_graphics("./pictures/Projection_Maps.png" )

#' 
#' ## Geodesy Takeaways
#' 
#' Unless you really focus on GIS, hardly anything I have said this lecture will matter to you.
#' 
#' - There are various models (geoids & ellipsoids) to represent the shape of the earth
#' - Different datums are used to represent different parts of the earth. This is in part due to legacy effects. 
#' - You will almost always use WGS 84 (based on a ellipsoid – which is fit through a special model of earths gravitational fields a geoid) NAD83 (based on a ellipsoid) for a geographic coordinate system.  These +/-1 m from each other across much of North America. More useful than a geoid.
#' - You will usually want to use a UTM grid  based on WGS for or State Planes based on NAD83 for projections.
#' - Different coordinates notation systems are used, focus on Decimal degrees. 
#' 
#' # An Introduction to Geographic Data Models
#' 
#' - **Vector Data Model** represents discrete features on the planet using geometries such as: points, lines, and polygons. 
#' - **Raster Data Model** represents (usually) continuous features on the plant using continuous surfaces, like a tile. 
#' 
#' Vector data tends to only include features of interest, e.g. bodies of water; whereas a Raster will include, **explicitly**, the absence of features (e.g. both water and terrestrial areas).
#' 
## ---- out.width = "85%", fig.align="center", fig.show='hold', echo = F, fig.cap = "Vector and Raster Data Models"--------------------
knitr::include_graphics("./pictures/vector_raster.png")

#' 
#' Talking:
#' 
#' In our field, we generally take values from rasters to serve as predictors to our sample unit, many of us store our sample as spatial points. We also tend to predict our models back onto raster surfaces.
#' 
#' # R Data Types Reviewed
#' 
#' Remember that different data types take up different amounts of memory in pretty much all software systems. 
#' 
## ----Larger Data Set Integer vs Numeric, echo = F, comment = "", warning = F, message = F--------------------------------------------
numeric_vals <- round(rnorm(n = 10000000, mean = 25, sd = 5), 4)
int_vals <- as.integer(numeric_vals * 4) # remove the decimal places.

# barplot()
writeLines(paste0("When the values in raster cells are stored as integers as opposed to floats (with decimal points) they take up only ", 
       round(as.numeric(object.size(int_vals) / as.numeric(object.size(numeric_vals))),2),
        ' as many bytes.'))

rm(numeric_vals)

#' 
## ----Larger Data Set Integer vs Character, echo = F, comment="", warning =F, message = F---------------------------------------------
int_vals <- as.integer(round(rnorm(n = 10000000, mean = 10, sd = 1), 0)) 

random_words <- OpenRepGrid::randomWords(n = max(int_vals) - min(int_vals))
lookup_numbers <- seq(from = min(int_vals), to = max(int_vals), by = 1)
character_vals <- random_words[match(int_vals, lookup_numbers)]

writeLines(paste0("When the values in raster cells are stored as integers as opposed to words they take up only ", 
       round(as.numeric(object.size(int_vals) / as.numeric(object.size(character_vals))),2),
        ' as many bytes.'))

rm(int_vals, random_words, lookup_numbers, character_vals)

#' 
#' Please note that our Raster in this scenario is *very* small, small enough we can wrap our brains around it with only minimal inspection.
#' 
#' Without knowing much about a raster (yet!) We can simulate some of these comparisons to give a clue as to why it contains values like they do. 
#' 
#' Rasters generally come in 8-bit signed (unsigned are not uncommon) integers. Pictures are huge amounts of data, these values help reduce them. 
## ----The size of Raster Components, message = F, warning = F, comment = "", out.width = "85%", fig.align="center", echo = F----------

writeLines(paste0('The size of the our empty raster is ', as.numeric(object.size(empty_raster)/1000000), ' MB'))
size_rast_empty <- as.numeric(object.size(empty_raster))

rast_vals_char <- as.vector(as.character(dec_lakes_r_classified_f@data@values))
rast_vals_char <- ifelse(rast_vals_char == 0, 'Water', 'Terrestrial')
writeLines(paste0('The size of the data for our raster as a character vector is ', 
             as.numeric(object.size(rast_vals_char)/1000000), ' MB'))
size_vector_char <- as.numeric(object.size(rast_vals_char))

writeLines(paste0('The size of the data for our raster as a numeric (integer !) vector is ', 
             as.numeric(object.size(rast_vals_num)/1000000), ' MB'))
size_vector_num <- as.numeric(object.size(rast_vals_num))

writeLines(paste0('The size of the data for our raster in matrix form is ', 
             as.numeric(object.size(raster_matrix)/1000000), ' MB'))
size_rast_matr <- as.numeric(object.size(raster_matrix))

raster_dataframe <- as.data.frame(raster_matrix)
colnames(raster_dataframe) <- c(1:ncol(raster_dataframe)) 
writeLines(paste0('The size of the data for our raster in dataframe form is ', 
             as.numeric(object.size(raster_matrix)/1000000), ' MB'))
size_rast_df <- as.numeric(object.size(raster_dataframe))

writeLines(paste0('The size of our complete raster is ', 
             as.numeric(object.size(example_raster_dec)/1000000), ' MB'))
size_example_rast <- as.numeric(object.size(example_raster_dec))

sizes <- rbind(size_rast_empty, size_vector_num, size_vector_char, size_rast_matr, size_rast_df, size_example_rast)

sizes <- as.data.frame(cbind(rownames(sizes), sizes))
rownames(sizes) <- c(1:nrow(sizes))
colnames(sizes) <- c('Variable','Bytes')
sizes$Bytes <- as.numeric(sizes$Bytes)
sizes <- sizes[order(sizes$Bytes),]
sizes$Variable <- c('Spatial Raster', 'Vector (Int.)', 'Matrix (Int.)', 'Complete Raster', 'Dataframe (of Int.)', 'Vector (Char.)')

par(mar=c(5,7.5,2.5,5))
barplot(height = sizes$Bytes, names = sizes$Variable,  
        main="Size of Realized and Potential Raster Components",
        ylab = "",
        xlab="Bytes", 
        horiz = T, 
        las = 1, 
        density=c(50,50,50,50,50,50), 
        angle=c(45, 45, 90,45,45,135), 
        col=c("darkslategray4","darkslategray4", 'firebrick1',"darkslategray4", 'firebrick1', 'firebrick1'))

# writeRaster(example_raster_dec, 'Honey_lake_ex.tif')

rm(size_rast_empty, size_vector_char, size_vector_num, size_rast_matr, size_rast_df, size_example_rast, sizes)

#' 
#' 
## ---- echo = F, warning = F----------------------------------------------------------------------------------------------------------
rm(raster_dataframe, raster_matrix, rast_vals_char, rast_vals_num)

#' 
#' # Vector Data in R
#' 
#' - **Vector Data Model** represents discrete features on the planet using geometries such as: points, lines, and polygons. 
#' 
#' ## Simple Features - Standards
#' 
#' - Open Geospatial Consortium ISO 19125-1:2004: Currently adhered to in ESRI, used in GDAL.
#' - Features have *geometries* describing their locality on Earth, and properties described by *attributes.*
#' - If the geometry of a feature is a polygon, it is composed of points, connected by straight lines.
#' - Lines composing polygons cannot intersect
#' 
#' ### Simple Features 1 - Attributes
#' 
#' - Attributes of the feature, theoretically devoid of spatial context.
#' - Described in text, numbers, stored in a data frame type object.
#' 
## ----Simple Features 1 - Attributes, echo = F----------------------------------------------------------------------------------------

attributes <- tibble(
  TAXON = c('Robinia pseudoacacia', 'Quercus alba'), 
  DBH = c(40, 32), 
  HEIGHT = c(24, 21)
)

knitr::kable(attributes)

#' 
#' ### Simple Features 2 - Coordinates form a Point 
#' 
#' - all geometries are composed of points.
#' - points only require two coordinates, X & Y. 
#' 
#' - Y = Latitude (necessary), X = Longitude (necessary)
#' - Z = Elevation (somewhat uncommon)
#' - M = Time or Uncertainty of Measurement (rather uncommon)
#' 
## ----Simple Features 2 - Coordinates form a Point, echo = F, fig.align="center", out.width = "75%"-----------------------------------

ex <- data.frame(X = c(-7, -5, 0, 5, 9), Y = c(-8, 6, 0, -4, 5))
ex$Name <- paste0("c(", "x = ",  ex$X, ", y = ", ex$Y, ")")

ggplot(ex, aes(x = X, y = Y, label = Name), size = 5) +
  geom_point() +
  xlim(-10, 10) +
  ylim(-10, 10) +
  ggrepel::geom_label_repel(aes(label = Name),
                  box.padding   = 0.5, 
                  point.padding = 0.5,
                  segment.color = 'purple') 
  
rm(ex)

#' 
#' ### Simple Features 3 - Points are/form a SF Geometry (sfg's)
#' 
#' - SFG is the spatial topology associated with a feature
#' 
#' - POINT - A true dimensionless 1 dimensional point
#' - LINESTRING - Sequence of points connected by strings.
#' - POLYGON - Sequence of points connected by strings, which close upon themselves. 
#' - i.e. the first and last point of the polygon are the same.
#' 
## ----Simple Features 3 - Points are/form a SF Geometry (sfg), echo = F---------------------------------------------------------------
# Input example coordinates here and draw them in a facet grid via par
ex_point <- st_point(c(0,0))

ex_linestring <- st_linestring(
  rbind(
    c(-1.5,-2.5), c(-2,-2.25), c(-2.0,2.25), c(-1.5,2.5)
  )
)

ex_polygon <- st_polygon(
  list(
    rbind(
      c(0,-5), c(-2.5,-2.5), c(-2.5,2.5), 
      c(0,5),c(2.5,2.5), c(2.5, -2.5), 
      c(0,-5) # note we have to close the POLYGON, the last pt is same as the first!
    )
  )
)

ex_point_plot <- ggplot(ex_point) +
  geom_sf() +
  xlim(-5, 5) +
  ylim(-5, 5) +
  labs(title="Point") +
  theme(plot.title = element_text(hjust = 0.5))
ex_linestring_plot <- ggplot(ex_linestring) +
  geom_sf() +
  xlim(-5, 5) +
  ylim(-5, 5) +
  labs(title="Linestring") +
  theme(plot.title = element_text(hjust = 0.5))
ex_polygon_plot <- ggplot(ex_polygon) +
  geom_sf() +
  xlim(-5, 5) +
  ylim(-5, 5) +
  labs(title="Polygon") +
  theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(ex_point_plot, ex_linestring_plot, ex_polygon_plot, labels = NULL, ncol = 3)

#' 
#' ### Simple Features 4 - A Geometry can be combined to form Geometries (sfg's)
#' 
#' - Two or more Points, and sets of Linestring, and polygons can be the geometries of a single feature
#' - Multi(POINT), multi(LINESTRING), multi(POLYGON) - Forming collections of geometries
#' - GEOMETRYCOLLECTION - A mixed set of these three or more geometries in the same geometry
#' 
## ----Simple Features 4 - Geometries can form Geometries (sfg), echo = F--------------------------------------------------------------

ex_multipoint <- st_multipoint(
  rbind(
    c(0,-4.0), c(-3,-2.5), c(-3, 2.5), 
    c(0, 4.0), c( 3, 2.5), c( 3,-2.5)
  )
)

ex_multilinestring <- st_multilinestring(
  list(
    rbind(c(-1.5,-2.5), c(-2,-2.25), c(-2.0,2.25), c(-1.5,2.5)),
    rbind(c( 1.5,-2.5), c( 2,-2.25), c( 2.0,2.25), c( 1.5,2.5))
  )
)
  
ex_multipolygon <- st_multipolygon(
  list(
    list(
      rbind(c(-3,-1), c(-3,1), c(-4,1), c(-4,-1), c(-3,-1))
    ), 
    list(
      rbind(c( 3,-1), c( 3,1), c( 4,1), c( 4,-1), c( 3,-1))
    )
  )
) 

ex_geometrycollection <- st_geometrycollection(list(ex_linestring, ex_polygon, 
                                                    ex_multipoint, ex_multilinestring, ex_multipolygon, ex_point))

ex_multipoint_plot <- ggplot(ex_multipoint) +
  geom_sf() +
  xlim(-5, 5) +
  ylim(-5, 5)  +
  labs(title="Multipoint") +
  theme(plot.title = element_text(hjust = 0.5))
ex_multilinestring_plot <- ggplot(ex_multilinestring) +
  geom_sf() +
  xlim(-5, 5) +
  ylim(-5, 5)  +
  labs(title="Multilinestring") +
  theme(plot.title = element_text(hjust = 0.5))
ex_multipolygon_plot <- ggplot(ex_multipolygon) +
  geom_sf() +
  xlim(-5, 5) +
  ylim(-5, 5)  +
  labs(title="Multipolygon") +
  theme(plot.title = element_text(hjust = 0.5))
ex_geometrycollection_plot <- ggplot(ex_geometrycollection) +
  geom_sf() +
  xlim(-5, 5) +
  ylim(-5, 5)  +
  labs(title="Geometycollection") +
  theme(plot.title = element_text(hjust = 0.5))

#ex_polygon[["data"]]$geometry
#ex_multilinestring[["data"]]$geometry

cowplot::plot_grid(ex_multipoint_plot, ex_multilinestring_plot, ex_multipolygon_plot, ex_geometrycollection_plot, labels = NULL, ncol = 4)

rm(ex_point, ex_linestring, ex_polygon, ex_multipoint, ex_multilinestring, ex_multipolygon, ex_multipoint_plot, ex_multilinestring_plot, ex_multipolygon_plot, ex_geometrycollection_plot, ex_point_plot, ex_geometrycollection, ex_linestring_plot, ex_polygon_plot)

#' 
#' ### Simple Features 5 - Geometries and Spatial Information form an SF Collection (sfc)
#' 
#' - A list column of S3 type containing certain parameters regarding the Geometry/Geometries
#' 
#' - Coordinate Reference System ('crs')
#' - Precision ('precision')
#' - Bounding box ('bbox')
#' - Number Empty ('n_empty')
#' 
#' Wherein precision refers to the precision of the coordinates forming the geometries. the bbox is a rectangle which includes all pairs of coordinates in the geometry. N_empty, denotes whether the geometry is missing coordinates in a position.
#' 
#' ### Simple Features 6 - Recapped. 
#' 
## ----Simple Features 6 Recapped,  echo = F, message = F, comment = ""----------------------------------------------------------------

attributes <- attributes %>% 
  mutate(LONG =  c(-87.676068, -87.673992)) %>% 
  mutate(LAT = c(42.056629, 42.057151)) %>% 
  st_as_sf(coords = c(x = 'LONG', y = 'LAT'), crs = 4326, remove = F)

knitr::kable(attributes)

st_precision(attributes) <- 2
attributes$geometry[1]
str(attributes$geometry[1])

rm(attributes)

#' 
#' - We now finally have *both* attributes and coordinates which are forming a point geometry.
#' - This is a Simple Feature. 
#' 
#' ## sp
#' 
#' - Predecessor to the 'SF' R package
#' - Many spatial statistics programs still only take sp objects as input
#' - A good introduction to S4 objects; popular in new spatial packages too!
#' 
#' ### sp - History
#' 
#' - Released in 2005 
#' - First package to hold all major vector types of geometries
#' - Unified Spatial class allowed for support in many spatial statistics packages
#' - Improved mapping of spatial objects
#' 
#' - Formed the centerpiece of spatial statistics in R for over a dozen years.
#' 
#' ### sp - Spatial Classes for Topology
#' 
#' - SF holds different geometries in list columns, SP has *many* different types of objects/classes for holding geometries.
#' 
#' - SpatialPoints
#' - SpatialLines
#' - SpatialPolygons
#' - SpatialGrids
#' 
#' Similar in theory to the simple feature geometries we learned about...
#' 
#' ### sp - Structures of The Spatial* Class - Topology Only
## ----Structure of Spatial class, echo = F--------------------------------------------------------------------------------------------
xc <- round(runif(15), 2)
yc <- round(runif(15, min = 0, max = 2), 2)
xy <- cbind(xc, yc)

sPoints <- SpatialPoints(xy)
str(sPoints)

#' 
#' # sp - Attributes - DataFrame
#' 
#' - Attachment of attributes, in a data frame, to a Spatial topology can create:
#' 
#' - SpatialPointsDataFrame
#' - SpatialLinesDataFrame
#' - SpatialPolgonsDataFrame
#' - SpatialGridsDataFrame
#' 
#' - Each S*DF is accessed the same way:
#'   - SPDF@data[['col_name']]
#'   - SPDF$'col_name'
#'   
## ----Attributes of SP classes, echo = F----------------------------------------------------------------------------------------------
df <- data.frame(temperature = round(5 + rnorm(15), 2), aspect = sample(1:45, size = 15, replace = F))
SPDF <- SpatialPointsDataFrame(sPoints, df)
str(SPDF)
knitr::kable(head(SPDF@data))

#' 
#' We see here that SF contains all of the spatial information in a Simple Feature Collection which can be tidily tucked away in the geometry list column, in SP the analogue to the SFC is the object itself. 
#' 
#' SP is mostly geometry with a data slot, SF is mostly data frame with a geometry column. 
#' 
#' # sp Overview of a Spatial*DataFrame
#' 
#' - Data frame is held in a different slot from the geometry and topology  
#' - Data frame columns may be subset  
#' - Data frame columns accessed via object@data[['colname']] indexing 
#' 
## ----Creat some Spatial Classes, echo = F--------------------------------------------------------------------------------------------

plot(SPDF, col=SPDF@data[['temperature']], pch = 25, xlim = c(0,1))
plot(gridlines(SPDF), add = TRUE, col = grey(.8))
text(labels(gridlines(SPDF)), col = grey(.7))
title("Example SP Plot")

rm(xc, yc, df, SPDF, xy, sPoints)

#' 
#' ### sp - Recapped.
#' 
#' - If you need to use spatial statistics, you will come across these objects.
#' - Don't sweat them **too** much; you can always convert from and to sf to run 
#' certain analyses
#' 
#' # Raster data in R
#' 
#' - Reminder: the Raster data Model is a way to represent (continuous) features on the planet using grids
#' - Excellent format for storing and sharing data
#' - Divides space into continuous grids
#' - Raster data sets are often distributed in *tiles*
#' 
## ---- out.width = "75%", fig.show='hold', echo = F, fig.align="center", fig.cap = "Western Plant Predictors. Note each cell is the extent of a single raster tile, each tile contains cells."----
knitr::include_graphics("pictures/Western_Plant_Predictors.png")

#' 
#' ## Raster Components
#' 
#' - Bounding Coordinate(s)
#' - Cell Resolution (Size of cell)
#' - Dimensions (No of cells in rows and columns)
#' - Coordinate Reference System (CRS)
#' 
#' Note that both *cell sizes*, and the *resolution of the values* in cells can, within reason, be converted to finer and coarser resolution. We will discuss these types of calculations next class. 
#' 
#' ### Example Raster 1 Create Frame
#' 
#' Rasters tend to confuse people. We are going to create our own example here before we talk about them much more.
#' 
#' Fairy shrimp live in bodies of water which dry out in late Spring, and refill in early Fall. We seek to determine where suitable habitat for fairy shrimp are in the Great Basin. In order to do so, we will use satellite imagery to detect valleys which fill with water in Fall, and dry out in late Spring.
#' 
## ---- out.width = "85%", fig.align="center", fig.show='hold', echo = F, fig.cap = "Suprise Valley by: John Glen"---------------------
knitr::include_graphics('./pictures/SurpriseValley_JohnathonGlen_USGS.jpg')

#' 
## ----Create Empty Raster and Populate - 2 with output,  comment = "", warning = F----------------------------------------------------
empty_raster <- raster(
  
  # rasters have 4 bounding edges
  # Here we define each 'corner' of the raster'
    xmn = 697129.7, 
    xmx = 811775.7,
    ymn = 4388466,
    ymx = 4502382,
    
  # Here we set the number of cells 118*118
    nrows = 118, 
    ncols = 118,
    
    # we do NOT manually specify the cell size here.
    
    crs = "+proj=utm +zone=10 +datum=WGS84", 
    # set the rasters Coordinate Reference System
)

#' 
#' In the code above, we are going to define all four of the essential components of a raster. Clearly, we are defining three explicitly (Bounding Coordinates, Dimensions, CRS), and the remaining one implicitly (Cell resolution). 
#' 
## ----Create Empty Raster and Populate - 3 with output, fig.align="center", echo = F, comment = "", warning = F-----------------------

e <- extent(dec_lakes_r)
ext_rast <- st_bbox(dec_lakes_r)

# create a fishnet to emulate the idea of an empty raster ! :-)
grid <- st_make_grid(
  ext_rast,  
  n = c(118, 118),
  what = "polygons",
  square = TRUE,
  flat_topped = FALSE
  ) %>% 
  st_transform(32610)
# st_write(grid, 'Example_HoneyLake_Grid.shp')

ggplot(grid) +
  geom_sf(fill = NA) +
  coord_sf(datum = st_crs(grid)) +
  theme_classic() +
  labs(x = 'Easting', y = 'Northing')

rm(e)

#' 
#' We can imagine that the raster we are currently creating looks like this. A frame without content. We can see where the bounding coordinates are, 
#' 
## ----Create Empty Raster and Populate - 4 with output, echo = F, comment = "", warning = F-------------------------------------------
rast_vals_num <- as.integer(as.vector(as.numeric(dec_lakes_r_classified_f@data@values)))

#' 
#' ### Example Raster 2 Set Values
#' 
## ----Create Empty Raster and Populate - 5 with output,  comment = "", warning = F----------------------------------------------------
raster_matrix <- matrix(rast_vals_num, # # fill matrix with values, 
                       nrow = empty_raster@nrows, # create matrix same dimensions.
                       ncol = empty_raster@ncols, # create matrix same dimensions
#ensure filling of matrix goes from upper left to lower right.
                       byrow = T) 

raster_matrix[90:118,112:118] <- 1 # fix the clipping image at edge. 

#' 
## ----Create Empty Raster and Populate - 6 with output, echo = F, comment = "", warning = F-------------------------------------------
fake_data <- matrix(c(0,1,1,0,1,0),
                    nrow = 6,
                    ncol = 8,
                    byrow=T)
knitr::kable(fake_data, caption = "Example Matrix showing values underlaying a raster layer.")

#' 
#' 
#' I think it is easiest to imagine that the values within a raster are in the shape of a matrix. Hence, using the code above we could take a vector of values, and bend them, so that each position within the vector matches up with the beginning of a new  row. 
#' 
## ----Create Empty Raster and Populate - 7 with output, comment = "", warning = F-----------------------------------------------------
example_raster_dec <- setValues(empty_raster, raster_matrix)

#' 
#' ### Example Raster 3 
#' 
## ----Create Empty Raster and Populate - 8 with output, fig.align="center",  comment = "", warning = F, echo = F----------------------
raster_matrix <- raster::as.matrix(oct_lakes_r_classified_f)
raster_matrix[90:118,112:118] <- 1 

example_raster_oct <- setValues(empty_raster, raster_matrix)

tmap::tm_shape(example_raster_oct) +
tmap::tm_raster(style= "cat", title="Standing Water", 
                labels = c("0 = Open Water", "1 = Upland"), 
                palette = c("deepskyblue", "beige")) +
#tmap::tm_compass(type="arrow", position=c(.03, .87))+
#tmap::tm_scale_bar(position = c(0.15, .9), size=.8) +
tmap::tm_layout(legend.outside = TRUE)

resolution_of_raster(example_raster_dec)
cell_count(example_raster_dec)

rm(resolution_of_raster, cell_count, fake_data, ext_rast, grid, empty_raster, dec_lakes_r_classified_f, oct_lakes_r_classified_f, dec_lakes_r, rast_vals_char, raster_matrix)

#' 
#' ## Raster Package - in R! 
#' 
#' - Does not need to load all files into active memory at once
#' - Many functions based on functions in 'base' R. 
#' 
#' ### Attributes
#' 
#' - Cells
#' - Values
#' 
#' ### Raster Layer
#' 
#' A single raster layer.
#' 
#' ### Raster Stack 
#' 
#' In general these are rasters which have been classified and we want to extract values from or run calculations with.  Now what is great about layers, is that we can do one big thing bricks cannot do, we can load in many layers from many files and create a stack of attributes we are interested in studying on the fly. 
#' 
## ----Raster Stack, echo=F, warning = F, message = F----------------------------------------------------------------------------------
path <- 'spatial_lecture_data/sentinel_imagery'
files.jpg <- list.files(path, ".jpg$")
oct_lakes_img <- terra::rast(paste0(path,"/",files.jpg[2]))
dec_lakes_img <- terra::rast(paste0(path,"/",files.jpg[4]))
my_cols <- c("Skyblue","Beige" )

par(mfrow= c(2,2))
plot(example_raster_oct, col=my_cols, main = "October")
plot(example_raster_dec, col=my_cols, main = "December")
terra::plotRGB(oct_lakes_img)
terra::plotRGB(dec_lakes_img)

#' 
#' The main use of Raster Stacks is to hold layers of similar themes.
#' 
#' Two usage examples:  
#' 
#' 1) Each layer is the same variable from a different time. 
#'   - e.g. mean monthly temperature from January -> December (12 layers per stack)
#' 2) Each layer is a different variable (theme) in an analysis. 
#'   - e.g. yearly mean temperature, mean precipitation, etc.
#' 
#' - Do not need be held in memory
#' 
#' ### Raster Brick
#' 
#' - Multiple bands of imagery held in the same file
#' - The sensors of a camera, e.g. Red, Green, and Blue
#' - Used for performing image classification to produce data raster layers
#' - Now bricks of over 100 sensor bands exist...
#' 
#' As I have mentioned rasters are often generated from satellite imagery; we will discuss rasters which are not developed this way next lecture. Historically most pictures are imaged via the use of three bands. These having spectral values of Red, Green, and Blue (RGB) associated with them. For example our .tif file, is composed of three bands. Note that a Rasterbrick is most often used for loading in these types of imaging data, which can then be processed to form a more typical 'raster' dataset.
#' 
## ----Raster Brick, echo=F, warning = F, message = F----------------------------------------------------------------------------------

dec_lakes_brick <- raster::brick(paste0(path,"/",files.jpg[4]))
dec_lakes_img <- terra::rast(paste0(path,"/",files.jpg[4]))

grayscale_colors <- gray.colors(150, start = 0.0, end = 1.0, gamma = 2.2, 
# [gamma] correction between how a digital 
# camera sees the world and how human eyes see it
                                alpha = NULL) 
# this code from NEON, see citations

par(mfrow=c(2,2))
plot(dec_lakes_brick, 1, col=grayscale_colors, main = "band 1 - Red")
plot(dec_lakes_brick, 2, col=grayscale_colors, main = "band 2 - Green")
plot(dec_lakes_brick, 3, col=grayscale_colors, main = "band 3 - Blue")
plotRGB(dec_lakes_img)

rm(path, files.jpg, my_cols, oct_lakes_img, dec_lakes_img, example_raster_oct, dec_lakes_brick, grayscale_colors, example_raster_oct,  dec_lakes_v)

#' 
#' Rasters are often built from satellite imagery. Historically most pictures are split into three copies, each one having spectral values of Red, Green, and Blue (RGB) associated with them. For example our .tif file, is composed of three layers. Note that a Rasterbrick is most often used for loading in these types of data.
#' 
#' Bricks are very important for processing and classifying raw image data. While we have three bands here, nowadays LIDAR equipped with Hyperspectral sensors are likely to have many more bands up to a couple hundred depending on the application. 
#' 
#' One important technical point to note with the Rasterbrick is that each band of the brick loads from the same individual file, for example picture. And that  bands are not typically combined from different picture sources. 
#' 
#' Raster bands also do not need to be held in memory, allowing one to work through large amounts of them. 
#' 
#' 
#' ## Terra
#' 
#' - Developed by the same team as the Raster Package
#' - Functionally virtually identical to Raster, but calculations run more quickly ! :-)
#' - Rasterbricks/layers no longer need specification, all objects are Spatstats ?
#' - Will supercede Raster, but see points 1 & 2. 
#' 
#' # Cartography
#' 
#' - Focus on sf here
#' - You have scripts to do many types of mapping in your course resources
#' - sf objects are ggplot2 compliant
#' - the order of mapping operations is more important than code
#' 
#' - Import example data sets which come with two packages
## ----sf with ggplot2 - 1-------------------------------------------------------------------------------------------------------------
data("us_states", package = "spData")
us_states <- st_as_sf(us_states)

north_carolina <- read_sf(system.file("shape/nc.shp", package = "sf"))

#' 
#' - Map an sf object using their own geom 'geom_sf'
## ----sf with ggplot2 - 2, out.width = "75%", fig.show='hold', fig.align="center"-----------------------------------------------------
ggplot(north_carolina) +
  geom_sf() + 
  theme_bw() # we will use this theme for class.

#' 
#' - Fill the interior of polygons by a variable
## ----sf with ggplot2 - 3, out.width = "75%", fig.show='hold', fig.align="center"-----------------------------------------------------
ggplot(north_carolina) +
  geom_sf(aes(fill = BIR79))  + # fill the interior of polygons by a variable
  theme_bw()

#' 
#' - Color the borders of each polygon in an sf object
## ----sf with ggplot2 - 4, out.width = "75%", fig.show='hold', fig.align="center"-----------------------------------------------------
ggplot(north_carolina) +
  geom_sf(aes(color = BIR79),  # color the borders of polygons by a variable
          lwd = 1 # just making the borders thicker.
          )  +
  scale_color_viridis_c(option = "plasma", trans = "sqrt") +
  # just using a very colourful color scheme so you can see it. 
  theme_bw()

#' 
#' - Color the border and remove any fill from the polygon interior
## ----sf with ggplot2 - 5a, out.width = "75%", fig.show='hold', fig.align="center"----------------------------------------------------
ggplot(north_carolina) +
  geom_sf(aes(color = BIR79),
          fill = NA  # set to 'NA' to 'remove' the interior of polygons
          ) +
  scale_color_viridis_c(option = "plasma", trans = "sqrt") +
  theme_bw()

#' 
#' - Color the interior of polygons and remove the border
#' - Can be useful to declutter maps
## ----sf with ggplot2 - 5b, out.width = "75%", fig.show='hold', fig.align="center"----------------------------------------------------
ggplot(north_carolina) +
  geom_sf(aes(fill = BIR79),
          color = NA  # set to 'NA' to 'remove' the interior of polygons
          ) +
  scale_color_viridis_c(option = "plasma", trans = "sqrt") +
  theme_bw()

#' 
#' 
#' - Both fill the interior of polygons and color the borders
## ----sf with ggplot2 - 6, out.width = "75%", fig.show='hold', fig.align="center"-----------------------------------------------------
ggplot(north_carolina) +
  geom_sf(aes(fill = BIR79, 
              color = FIPS)
          ) + # both fill and color
  guides(color = 'none') + # removed the categorical legend (is too big!)
  theme_bw()

#' 
#' - all of this could also have been done as: 
## ----sf with ggplot2 - 7, out.width = "75%", fig.show='hold', fig.align="center"-----------------------------------------------------
ggplot() + # leave empty
  geom_sf(data = north_carolina, # put data here
          aes(fill = BIR79, 
              color = FIPS)
          ) + # both fill and color
  guides(color = 'none') + # removed the categorical legend (is too big!)
  theme_bw()

#' 
#' - Use two data sets to create one map
## ----sf with ggplot2 - 8, out.width = "75%", fig.show='hold', fig.align="center"-----------------------------------------------------
ggplot(us_states) + # two data sets.
  geom_sf() +
  geom_sf(data = north_carolina, 
          fill = 'purple', 
          color = NA # hash this line out in your own time to see the difference
          ) +
  theme_bw()

#' 
#' - Be diligent about the order of data sets
## ----sf with ggplot2 - 9, out.width = "75%", fig.show='hold', fig.align="center"-----------------------------------------------------
ggplot(north_carolina) + # oh no we drew over North Carolina!
  geom_sf(fill = 'purple') +
  geom_sf(data = us_states) +
  theme_bw()

#' 
#' - remember to build plots from the 'bottom' up
## ----sf with ggplot2 - 10, out.width = "75%", fig.show='hold', fig.align="center"----------------------------------------------------
ggplot() + # two data sets.
  geom_sf(data = us_states, fill = NA) +
  geom_sf(data = north_carolina, fill = 'purple') +
  theme_bw()

#' 
#' - coord_sf is a helpful modifier to geom_sf
#' - Can set a CRS for the map, modify extent, and change some rendering styles
#' - remember: '?coord_sf' for arguments!
## ----sf with ggplot2 - 11, out.width = "75%", fig.show='hold', fig.align="center"----------------------------------------------------
ggplot() +
  geom_sf(data = us_states) +
  geom_sf(data = north_carolina) +
  coord_sf(crs = 4267, # we can convert each dataset to the same CRS 
           datum = NA # we can remove the grid lines/graticules from plot
           )

#' 
#' - clip the extent of a map using the bbox of the data set of interest
## ----sf with ggplot2 - 12, out.width = "75%", fig.show='hold', fig.align="center"----------------------------------------------------
bound <- st_bbox(north_carolina) # retrieve the bbox from the sfc list column

ggplot() +
  geom_sf(data = us_states) +
  geom_sf(data = north_carolina, aes(fill = BIR79)) +
  coord_sf( # use the bbox to 'crop' the extent of the map
           xlim = c(bound[1], bound[3]), 
           ylim = c(bound[2], bound[4])
           ) +
  theme_bw()

#' 
## ---- echo = F-----------------------------------------------------------------------------------------------------------------------
rm(bound, us_states, north_carolina)

#' 
#' # Assignments for the Duration of the Spatial Data Science Module:
#' 
#' **For next Lab: **
#' 
#' *Please* install these packages (if you have not done so already):
## ----For Lab and Lecture, eval = F---------------------------------------------------------------------------------------------------
## install.packages("sf", "raster", "terra", "sp", "tmap", "leaflet", "ggmap")
## # optional packages for using parallel processing at a step
## # (in our example it won't actually speed up the analyses
## # they actually may slow them down!)
## install.packages('snow','parallel')

#' 
#' Download the labs .R script from the course website. 
#' 
#' If you are interested in how Drone/Lidar data are collected please check out the 'RMBL Spatial Data Science Webinar Series':
#' 
#' - https://github.com/ikb-rmbl/SpatialDataScienceWebinars2020 
#' - Collecting UAS Data (Video): https://youtu.be/Pq8btEZRCvM (1.25 hours)
#' 
#' **For next Lecture:**
#' Assigned Reading: Chapter 3 of Spatial Data Science
#' 
#' **Future Bonus SDS Office Hours**
#' Wednesday Night at 5:00 - 6:00. 
#' 
#' **Notes on this Lab**
#' My lecture notes are in the R script I used to generate all of the novel figures for this presentation. Likewise this presentation is an .HTML file and can be launched from your computer (it was rendered directly from R using the script). 
#' 
#' # Works Cited
#' 
#' Krystalli, A. https://annakrystalli.me/intro-r-gis/gis.html Accessed 01.20.2022
#' 
#' https://geocompr.robinlovelace.net/spatial-class.html Accessed 01.09.2022
#' 
#' Hijman, R. 05.12.2019 'The raster Package'
#' 
#' https://rspatial.org/raster/RasterPackage.pdf Accessed 01.09.2022
#' 
#' https://www.neonscience.org/resources/learning-hub/tutorials/dc-multiband-rasters-r Accessed 01.19.2022
#' 
#' Pebesma, E. https://r-spatial.github.io/sf/articles/sf1.html Accessed 01.10.2022
#' 
#' https://proj.org/operations/conversions/geoc.html Accessed 01.20.2022.
#' 
#' https://rspatial.org/raster/spatial/8-rastermanip.html Accessed 01.11.2022
#' 
#' https://en.wikipedia.org/wiki/Open_Source_Geospatial_Foundation Accessed 01.09.2022
#' 
#' NOAA. What is geodesy? National Ocean Service website, https://oceanservice.noaa.gov/facts/geodesy.html, 1/25/17.
#' 
#' Geocomputation with R. Lovelace, R., Nowosad, J., Muenchow, J. 2022-01-25.
#' 
#' ## Packages Cited:
## ----Packages cited, comment = ""----------------------------------------------------------------------------------------------------
c("raster", "sp", "sf", "tidyverse", "terra") %>%
  map(citation) %>%
  print(style = "text", na.print = '')

