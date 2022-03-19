#' ---
#' title: "R Data Science: Spatial Data Science 2"
#' author: "steppe"
#' date: "2/14/2022"
#' output:
#'    pdf_document:
#'      latex_engine: xelatex
#'      fig_caption: true
#'      number_sections: yes
#'      toc: yes
#' header-includes:
#'  \usepackage{float}
#' 
#' ---
#' 
#' \tableofcontents 
#' \listoffigures
#' \listoftables
#' 
#' Assigned Reading: Chapter 3 of Spatial Data Science
#' 
## ----Load Libraries, message = F, warning = F, echo = F------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages #  silence messages.

shhh(library(tidyverse))
shhh(library(raster))
shhh(library(terra))
shhh(library(sf))
shhh(library(spData))
shhh(library(parallel))
shhh(library(fields))
shhh(library(spdep))
shhh(library(gstat))
shhh(library(spatstat))

rm(shhh)

#' 
## ----Import Chicago Neighborhoods Polygons, warning = F, echo = F--------------------------------------------------------------------
files <- paste0('./spatial_lecture_data/Chicago_Neighborhoods/',
                list.files('./spatial_lecture_data/Chicago_Neighborhoods', ".shp"))
chi_neighb <- read_sf(files)
rm(files)

#' 
## ----Import Chicago Park District Polygons, warning = F, echo = F, cache = T---------------------------------------------------------
files <- paste0('./spatial_lecture_data/Chicago_Park_District_Park_Boundaries/' ,
                list.files('./spatial_lecture_data/Chicago_Park_District_Park_Boundaries', ".shp"))
chi_parks <- read_sf(files) %>% 
  dplyr::select(acres, park, park_class) %>% 
  st_make_valid() %>% 
  st_simplify(dTolerance = 100)

chi_neighb1 <- st_intersection(chi_neighb, chi_parks)
park_areas <- chi_neighb1 %>% 
  mutate(areas = as.numeric(st_area(.))/1000) %>% 
  group_by(pri_neigh) %>% 
  mutate(total_park_area = sum(areas)) %>% 
  distinct(pri_neigh, .keep_all = T) %>% 
  st_drop_geometry() %>% 
  dplyr::select(pri_neigh, total_park_area)

rm(files, chi_neighb1)

#' 
## ----Create Museum Dummy Data, echo = F, message = F, warning = F--------------------------------------------------------------------
AM <- as_tibble(cbind(attribute = 'American',
                latitude = 40.781563, 
                longitude = -73.974751)) %>% 
  st_as_sf(coords = c(x = 'longitude', y = 'latitude'), 
           crs = 4326, remove = F)
FM <- as_tibble(cbind(attribute = 'Field',
                latitude = 41.866662,
                longitude = -87.616433)) %>% 
  st_as_sf(coords = c(x = 'longitude', y = 'latitude'), 
           crs = 4326, remove = F)
SM <- as_tibble(cbind(attribute = 'Smithsonian',
                latitude = 38.890766,
                longitude = -77.025666)) %>% 
  st_as_sf(coords = c(x = 'longitude', y = 'latitude'), 
           crs = 4326, remove = F)


#' 
## ----Define Functions, echo = F------------------------------------------------------------------------------------------------------
journeys_to_sf <- function(journeys_data, # By Charlie
                           start_long = longitude,
                           start_lat = latitude,
                           end_long = longitude.end,
                           end_lat = latitude.end) {
  quo_start_long <- enquo(start_long)
  quo_start_lat <- enquo(start_lat)
  quo_end_long <- enquo(end_long)
  quo_end_lat <- enquo(end_lat)

  journeys_data %>%
    dplyr::select(
      !! quo_start_long,
      !! quo_start_lat,
      !! quo_end_long,
      !! quo_end_lat
    ) %>%
    transpose() %>%
    map(~ matrix(flatten_dbl(.), nrow = 2, byrow = TRUE)) %>%
    map(st_linestring) %>%
    st_sfc(crs = 4326) %>%
    st_sf(geometry = .) %>%
    bind_cols(journeys_data) %>%
    dplyr::select(everything(), geometry)
}


#' 
## ----Create Polygons to test spatial predicates, echo  = F---------------------------------------------------------------------------

A <- st_polygon( # complete
  list(
    rbind(
      c(1,2), c(3,4), c(3.5,3), c(3.5,2),
      c(3,1), c(1,2) 
    )
  )
)

B_disjoint <- st_polygon( # complete
  list(
    rbind(
      c(3.25,4.25), c(3.75,3.25), c(3.75,2.25),
      c(5.25,4.25), c(3.25,4.25)
    )
  )
)

B_touches <- st_polygon( # complete
  list(
    rbind(
      c(3,4), c(3.5,3), c(3.5,2),
      c(5,4), c(3,4)
    )
  )
)

B_overlap <- st_polygon( # complete
  list(
    rbind(
      c(3,2.5), c(3.5,1.5), c(3.5,0.5),
      c(5,2.5), c(3,2.5)
    )
  )
)

B_covers <- st_polygon( # complete
  list(
    rbind(
      c(2.75,3), c(3.5,3), c(3.5,2),
      c(2.75,2), c(2.75,3)
    )
  )
)

B_contains <- st_polygon( # complete
  list(
    rbind(
      c(2.25,3), c(3,3), c(3,2),
      c(2.25,2), c(2.25,3)
    )
  )
)

B_equals <- st_polygon( # complete
  list(
    rbind(
      c(1,2), c(3,4), c(3.5,3), c(3.5,2),
      c(3,1), c(1,2) 
    )
  )
)


disjoint_plot <- ggplot() +
   geom_sf(data = A, fill = "deeppink1", lty = 3) +
   geom_sf(data = B_disjoint, fill = "deepskyblue2", lty = 5) +
   xlim(0, 5) +
   ylim(0, 5) +
   labs(title="Disjoint") +
   theme_classic() +
   theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
   
touches_plot <- ggplot() +
   geom_sf(data = A, fill = "deeppink1", lty = 3) +
   geom_sf(data = B_touches, fill = "deepskyblue2", lty = 5) +
   xlim(0, 5) +
   ylim(0, 5) +
   labs(title="Touches") +
   theme_classic() +
   theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

overlap_plot <- ggplot() +
   geom_sf(data = A, fill = "deeppink1", lty = 3) +
   geom_sf(data = B_overlap, fill = "deepskyblue2", lty = 5) +
   xlim(0, 5) +
   ylim(0, 5) +
   labs(title="Overlaps") +
   theme_classic() +
   theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

covers_plot <- ggplot() +
   geom_sf(data = A, fill = "deeppink1", lty = 3) +
   geom_sf(data = B_covers, fill = "deepskyblue2", lty = 5) +
   xlim(0, 5) +
   ylim(0, 5) +
   labs(title="Covers") +
   theme_classic() +
   theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

contains_plot <- ggplot() +
   geom_sf(data = A, fill = "deeppink1", lty = 3) +
   geom_sf(data = B_contains, fill = "deepskyblue2", lty = 5) +
   xlim(0, 5) +
   ylim(0, 5) +
   labs(title="Contains") +
   theme_classic() +
   theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

equals_plot <- ggplot() +
   geom_sf(data = A, fill = "deeppink1", lty = 3) +
   geom_sf(data = B_equals, fill = "deepskyblue2", lty = 5) +
   xlim(0, 5) +
   ylim(0, 5) +
   labs(title="Equals") +
   theme_classic() +
   theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#' 
#' # Topology
#' 
#' While in the last lecture we emphasized the geographic components of data on in space, we will know turn our attention to some simple geometric properties of two and three dimensional objects. 
#' 
#' Essential to most GIS operations is Topology, 
#' 
#' ## Theoretical Relations
#' 
#' Most sets of relations between objects in two dimension are considered below by the Dimensionally Extended 9-Intersection Model (DE-9IM). The DE-9IM was developed to query spatial databases and still serves as the standard for describing relations. 
#' 
## ---- out.width = "50%", fig.show='hold', echo = F, fig.cap = "DE-9IM, By Krauss", fig.align="center"--------------------------------
knitr::include_graphics('./pictures/DE-9IM_By_Krauss.png')

#' 
#' In the example in the Upper Middle panel 
#' 
#' The dimensions which contain the intersection of the 'Interior' of 'A' to the 'Boundary' of 'B' is equal to a line. 
#' 
#' $$
#' dim[I(a)∩B(b)] = 1
#' $$
#' 
#' <br>
#' 
#' - where 'I' is the 'Interior' (of 'a') & 'B' is the 'Boundary' (of 'b')
#' 
#' $$
#' \text{∩ is the 'Intersection'}
#' $$
#' 
#' <br>
#' 
#'  - '1' denotes that the product of the intersection is a line
#' 
#' In the example in the Upper Middle panel 
#' 
#' The dimensions which contain the intersection of the 'Interior' of 'A' to the 'Boundary' of 'B' is equal to a line. 
#' 
#' ## Applications
#' 
#' When identifying solutions to a problem we are generally interested in how features affect each other. This schema provides us a framework for organizing our area of analyses, and subsetting the appropriate data. 
#' 
#' We are generally interested in : 
#' 
#' <br>
#' 
#' - *dim[I(a)∩I(b)] = 2* Which values are in both polygons 
#' - *dim[I(a)∩B(b)] = 1* Which values are in 'a', and at the boundary of 'b' 
#' - *dim[I(a)∩E(b)] = 2* Which values are in 'a', but not in 'b' 
#' 
#' <br>
#' 
#' - *dim[B(a)∩I(b)] = 1* Which values are at the boundary of 'a', and in 'b' 
#' - *dim[B(a)∩B(b)] = 0* Which values are at the shared boundaries of 'a' and 'b' 
#' - *dim[B(a)∩E(b)] = 1* Which values are at the boundary of 'a', and outside 'b' 
#' 
#' <br>
#' 
#' - *dim[E(a)∩I(b)] = 2* Which values are in 'b' but not in 'a' 
#' - *dim[E(a)∩B(b)] = 1* Which values are at the boundary of 'b' and outside 'a' 
#' - *dim[E(a)∩E(b)] = 2* Which values are outside both 'a' and 'b' 
#' 
#' <br>
#' 
#' # Geometric Operations on Spatial Predicates
#' 
#' ## Theoretical Properties
#' 
## ----Plot Topology, echo = F, fig.cap = "Spatial Predicates after Egenhofer and Herring", fig.align="center"-------------------------
cowplot::plot_grid(disjoint_plot, touches_plot, overlap_plot, covers_plot, contains_plot, equals_plot, labels = NULL, ncol = 3)

rm(contains_plot, covers_plot, disjoint_plot, equals_plot, overlap_plot, touches_plot)

#' 
#' **Disjoint** Neither 'A' nor 'B' touch at any position
#' 
#' **Touches** The boundaries of 'A' and 'B' touch
#' 
#' **Covers** Feature 'A' is encapsulated by 'B' on many sides, at least a boundary of 'A' shares at least a partial border with a border of 'B'
#' 
#' **Contains** All borders of one feature are contained by another
#' 
#' **Overlap** Portions of the interior of feature 'A' overlap the interior of 'B'
#' 
#' **Equals** 'A' and 'B' have identical extents.
#' 
#' In addition to these logical tests, postGIS implements the following three tests. 'Intersects' obviously tends to find the most applications, as it generalizes from 3 other logical tests. 
#' 
#' **Intersects** (includes: 'st_touches', 'st_overlaps', 'st_covers', 'st_contain', 'st_equals')
#' 
#' **Within** (includes: 'st_contains')
#' 
#' **CoveredBy** (includes: 'st_equals')
#' 
#' ## Applications
#' 
#' We may test these relationships quite readily using the sf package
#' 
## ----Spatial Predicates in SF, comment = "", echo = F--------------------------------------------------------------------------------

writeLines(paste0('st_disjoint(A, B_disjoint, sparse = F): ', st_disjoint(A, B_disjoint, sparse = F)[1,1]))
writeLines(paste0('st_touches(A, B_touches, sparse = F): ', st_touches(A, B_touches, sparse = F)[1,1]))
writeLines(paste0('st_equals(A, B_overlap, sparse = F): ', st_overlaps(A, B_overlap, sparse = F)[1,1]))
writeLines(paste0('st_covers(A, B_covers, sparse = F): ',  st_covers(A, B_covers, sparse = F)[1,1]))
writeLines(paste0('st_contains(A, B_contains, sparse = F): ', st_contains(A, B_contains, sparse = F)[1,1]))
writeLines(paste0('st_equals(A, B_equals, sparse = F): ', st_equals(A, B_equals, sparse = F)[1,1]))
writeLines(paste0('st_equals(A, B_disjoint, sparse = F): ', st_equals(A, B_disjoint, sparse = F)[1,1]))


#' 
#' - postGIS implementation
#' 
## ----Spatial Predicates in SF 2, comment = "", echo = F------------------------------------------------------------------------------
writeLines(paste0('st_intersects(A, B_touches, sparse = F): ', st_intersects(A, B_touches, sparse = F)[1,1]))
writeLines(paste0('st_intersects(A, B_overlap, sparse = F): ', st_intersects(A, B_overlap, sparse = F)[1,1]))
writeLines(paste0('st_intersects(A, B_covers, sparse = F): ', st_intersects(A, B_covers, sparse = F)[1,1]))
writeLines(paste0('st_intersects(A, B_contains, sparse = F): ', st_intersects(A, B_contains, sparse = F)[1,1])) 
writeLines(paste0('st_intersects(A, B_equals, sparse = F): ', st_intersects(A, B_equals, sparse = F)[1,1]))

writeLines(paste0('st_within(A, B_contains, sparse = F): ', st_within(A, B_contains, sparse = F)[1,1])) # opposite!
writeLines(paste0('st_within(B_contains, A, sparse = F): ', st_within(B_contains, A, sparse = F)[1,1])) # there we go!

writeLines(paste0('st_coveredby(A, B_equals, sparse = F): ', st_covered_by(A, B_equals, sparse = F)[1,1]))

rm(A, B_disjoint, B_contains, B_covers, B_equals, B_overlap, B_touches)

#' 
#' ## Spatial Join
#' 
#' - Combine two spatial objects, based on their spatial relations. 
#' - May use nearly all of the predicates above, and more!
#' - Allows left or inner join (only records present in both objects)
#' 
## ----Spatial Joins 1, comment = "", message = F, warning = F-------------------------------------------------------------------------

nghbrhd_parks <- st_join(chi_neighb, chi_parks, 
               join = st_intersects, 
               left = TRUE 
               ) %>% 
  count(pri_neigh) 

#' 
## ----Spatial Joins 2, comment = "", message = F, warning = F, echo = F---------------------------------------------------------------
bound <- st_bbox(chi_neighb)
    
a <- ggplot(chi_neighb) +
  geom_sf(fill = "#B3DDF2") + 
  coord_sf(xlim = c(bound[1],bound[3]), ylim = c(bound[2],bound[4])) +
  labs(title = 'Neighborhoods') +
  theme_void()  

b <- ggplot(chi_parks) + 
  geom_sf(fill = "#FF0000") +
  coord_sf(xlim = c(bound[1],bound[3]), ylim = c(bound[2],bound[4])) +
  labs(title = 'Parks') +
  theme_void()
  
c <- ggplot() + 
  geom_sf(data = nghbrhd_parks, aes(fill = n)) + 
  coord_sf(xlim = c(bound[1],bound[3]), ylim = c(bound[2],bound[4])) +
  theme_void() +
  labs(title = 'Park per Neighborhood') +
  theme(legend.position = "none")

cowplot::plot_grid(a, b, c, ncol = 3)

rm(bound, a, b, c)

#' 
#' 
#' # Assorted Spatial Vector Operations
#' 
#' This is really a grab bag of what I and others seem to use most the often. We are going to focus on vector data. 
#' 
#' ## Import a Vector file
#' 
#' - Many formats of vector files may be imported to R. 
#' 
## ----Import a Vector file, eval = F--------------------------------------------------------------------------------------------------
## files <- paste0('./spatial_lecture_data/Chicago_Neighborhoods/' ,
##                 list.files('./spatial_lecture_data/Chicago_Neighborhoods', ".shp"))
## chi_neighb <- read_sf(files)
## rm(files)

#' 
#' ## Convert to and from sp objects
#' 
## ----convert from-to sp-sf 1, echo = F-----------------------------------------------------------------------------------------------
data("us_states", package = "spData")
us_states.sp <- as(us_states, 'Spatial')

#' 
#' - To convert from an sp to an sf object:
## ----convert from-to sp-sf 2, comment = ""-------------------------------------------------------------------------------------------
us_states.sf <- st_as_sf(us_states.sp)
class(us_states.sf)

#' - to convert from an sf object to an sp object:
## ----convert from-to sp-sf 3, comment = ""-------------------------------------------------------------------------------------------
us_states.sp <- as(us_states.sf, 'Spatial')
class(us_states.sp)

#' 
#' ## Make tabular point data spatial
#' 
## ----make point data spatial 1, echo = F---------------------------------------------------------------------------------------------
museums <- as_tibble(
  cbind(
    attribute = c('Field Museum', 'Science and Industry'),
    latitude  = c( 41.86666,  41.79006),
    longitude = c(-87.61643, -87.58227)
    )
  ) 

#' 
#' - Specify columns holding coordinates  
#' - Specify the original crs the data were collected in  
#' - Should the coordinate holding columns be removed?  
#' 
## ----make point data spatial 2-------------------------------------------------------------------------------------------------------
museums.sf <- st_as_sf(museums, 
                       coords = c(x = 'longitude', y = 'latitude'), 
                       crs = 4326, 
                       remove = F
                       )

#' 
## ----make point data spatial 3, echo = F---------------------------------------------------------------------------------------------
knitr::kable(museums.sf)

#' 
#' ## st transform
#' 
#' - Determine the CRS of a simple feature
## ----transform crs 1-----------------------------------------------------------------------------------------------------------------
st_crs(museums.sf)

#' 
#' - Change coordinates from one Coordinate Reference System to another
## ----transform crs 2-----------------------------------------------------------------------------------------------------------------
museums.conus_albers <- st_transform(museums.sf, crs = 5070)

#' 
## ----transform crs 3, echo = F, comment = ""-----------------------------------------------------------------------------------------
st_crs(museums.conus_albers)[['input']]

#' ## Remove simple feature list column
#' 
## ----drop geometry column 1----------------------------------------------------------------------------------------------------------
museums <- st_drop_geometry(museums.sf)
st_geometry(museums.sf) <- NULL

#' 
## ----drop geometry column 2, echo = F------------------------------------------------------------------------------------------------
rm(museums, museums.sf, museums.conus_albers)

#' 
#' ## Make a geometry valid as a Simple Feature
#' 
#' You may import vector data which is not compliant with the Standards of the geometry for simple features. If you do this you will get a shocking warning, but the fix is quite simple. 
#' 
## ----Make Geometry Valid as a Simple Feature 1---------------------------------------------------------------------------------------
p1 = st_as_sfc("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))")
# create a geometry which will not be valid

#' 
## ----Make Geometry Valid as a Simple Feature 2, eval = F-----------------------------------------------------------------------------
## st_is_valid(p1)
## st_is_valid(p1, reason = TRUE)

#' 
## ----Make Geometry Valid as a Simple Feature 3, echo = F, comment = ""---------------------------------------------------------------
writeLines(paste0("st_is_valid(p1): ", st_is_valid(p1))) 
writeLines(paste0("st_is_valid(p1, reason = TRUE): ", st_is_valid(p1, reason = TRUE)))

#' 
#' We see that this geometry contains a line which intersects itself. Ff we refer to our first lecture in our introductory slides on Simple features we can across a rule *"Lines composing polygons cannot intersect"* - the feature we created violates this rule. 
#' 
#' In this case we can retain all geometric features by simply applying the function st_make_valid()
## ----Make Geometry Valid as a Simple Feature 4---------------------------------------------------------------------------------------
p2 <- st_make_valid(p1)

#' 
#' We can plot both the old and new features side by side, and label the coordinates, to see how SF made our geometry valid. 
## ----Make Geometry Valid as a Simple Feature 5, echo = F, fig.align="center", comment = ""-------------------------------------------
ex <- data.frame(X = p1[[1]][[1]][,1], Y = p1[[1]][[1]][,2])
ex$Name <- paste0("c(", "x = ",  ex$X, ", y = ", ex$Y, ")")
  
ex2 <- data.frame(rbind(X = p2[[1]][[1]][[1]],
                  Y = p2[[1]][[2]][[1]]))
colnames(ex2) <- c('X', 'Y')
ex2$Name <- paste0("c(", "x = ",  ex2$X, ", y = ", ex2$Y, ")")
  
a <- ggplot() +
  geom_sf(data = p1, fill = "darkseagreen")+
  geom_point(data = ex, aes(x = X, y = Y), size = 3) +
  xlim(-10, 15) +
  ylim(-10, 15) +
  theme_bw() +
  ggrepel::geom_label_repel(data = ex, aes(x = X, y = Y,label = Name),
                  box.padding   = 0.5, 
                  point.padding = 0.5,
                  segment.color = 'purple') +
  labs(title = "Non-Valid Geometry") +
  theme(plot.title = element_text(hjust = 0.5))

b <- ggplot() +
  geom_sf(data = p2, fill = "darkseagreen")+
  geom_point(data = ex2, aes(x = X, y = Y), size = 3) +
  xlim(-10, 15) +
  ylim(-10, 15) +
  theme_bw() +
  ggrepel::geom_label_repel(data = ex2, aes(x = X, y = Y,label = Name),
                  box.padding   = 0.5, 
                  point.padding = 0.5,
                  segment.color = 'purple') +
  labs(title = "Valid Geometry") +
  theme(plot.title = element_text(hjust = 0.5))

writeLines(paste0("If we call class(p1), we can see that our original simple feature collection (sfc) contained: ", class(p1)[1])) 
writeLines(paste0("If we call class(p2), we can see that our valid simple feature collection (sfc) contains: ", class(p2)[1])) 

cowplot::plot_grid(a, b, ncol = 2)
rm(p1, p2, ex, ex2, a,b)

#' As you see from our initial non-valid polygon, a second was created which resolves the geometry problem. Now our feature is represented by two polygons. From our earlier lecture please recall that the start and end points composing the line and polygon simple feature geometries are always the same coordinate pair.  Hence you can see that the coordinates of contention, x = 5 & y = 5, have become the new start and end points of our second polygon.
#' 
#' ## Aggregate Simple Features via 'group_by' & 'summarize'
#' 
#' - Can be used to combine the geometries of features
#' 
## ----Summarize 1, echo = F-----------------------------------------------------------------------------------------------------------
knitr::kable(head(us_states[,c(2:3,7)]))

#' 
## ----Summarize 2---------------------------------------------------------------------------------------------------------------------
regions <- us_states %>%
  group_by(REGION) %>% 
  summarize(geometry = st_union(geometry))

#' 
## ----Summarize 3, echo = F-----------------------------------------------------------------------------------------------------------
a <- ggplot(us_states) +
  geom_sf() +
  theme_void()

b <- ggplot(regions) +
  geom_sf() +
  theme_void()

cowplot::plot_grid(a, b, ncol = 1)

#' 
## ----Summarize 4, echo = F-----------------------------------------------------------------------------------------------------------
knitr::kable(regions)

rm(a, b, regions)

#' 
#' 
#' # Area Calculations
#' 
#' SF is readily capable of calculating the area of polygon features.
#' 
## ----Area 1, message = F, eval = F---------------------------------------------------------------------------------------------------
## st_area(chi_neighb)

#' 
## ----Area 2, message = F, echo = F, comment=""---------------------------------------------------------------------------------------
st_area(chi_neighb)[1:5]

#' 
#' These data are always returned in units of meters, regardless of the coordinate system specifications. However, different Coordinate Systems will give different results 
#' 
## ----Area 3, message = F, comment = "", echo = F, fig.align="center", out.width = "50%"----------------------------------------------
areas_proj <- chi_neighb %>% 
  st_transform(32610) %>% 
  st_area() 
# projected CRS UTM

areas_geog <- chi_neighb %>% 
  st_transform(4326) %>% 
  st_area() 
# Geographic CRS

# ensure these are in the same order
areas_proj <- sort(areas_proj)
areas_geog <- sort(areas_geog)

# are all values the same??
which(areas_proj == areas_geog)

#  what is the correlation between the two methods of measurement?
res <- cor.test(areas_proj, areas_geog,  method="kendall") # tau of 1, perfect correlation

# what is the relationship.
plot(x = areas_geog, y = areas_proj, pch = 18, xlim = c(min(areas_geog),max(areas_proj)), 
     ylim = c(min(areas_geog), max(areas_proj)), 
     main = "Area of Geographic CRS Polgyons plotted against Projected CRS")  
  abline(0,1, lty = 2) 
  text(min(areas_geog)*25, max(areas_proj)*0.975, 
       paste0("tau = ", res[["estimate"]][["tau"]])) +
  text(min(areas_geog)*60, max(areas_proj)*0.90,
       paste0("p-value = ", " < 0.001"))
  

#' 
#' When we use the 'which' logical test, we see that all areas in the results from both sets of area calculations are different. When we plot the values we see that both sets have nearly perfect correlation. 
#' 
#' But neither of these values are as accurate as they could be. We want to use an Equal Area projected coordinate system instead. 
#' 
#' Here we use the NAD83 Conus Albers, a favorite of the United States Geological Survey (USGS). 
## ----Area 4, message = F, comment = ""-----------------------------------------------------------------------------------------------
equal_areas_proj <- chi_neighb%>% 
  st_transform(5070) %>% 
# We want to use an equal area projection! 
  st_area()

equal_areas_proj <- sort(equal_areas_proj)

#' 
#' While metric values are easy to convert by 'hand', we can also convert them using the 'units' package.
## ----Area 5, message = F, comment = ""-----------------------------------------------------------------------------------------------
units(equal_areas_proj) <- units::make_units(km^2)
head(equal_areas_proj)

#' 
#' 
## ----Area - 6, message = F, echo = F, fig.align="center"-----------------------------------------------------------------------------

equal_areas_proj <- chi_neighb%>% 
  st_transform(5070) %>% 
# if for some reason need a project system - use equal area! 
  st_area()
areas <- as.numeric(equal_areas_proj)

areas <- cbind(chi_neighb, areas) 
largest <- areas %>% 
  arrange(-areas) %>% 
  slice_head(n = 5) %>% 
  st_drop_geometry() %>% 
  dplyr::select(1,5) 
  
smallest <- areas %>% 
  arrange(areas) %>% 
  slice_head(n = 5) %>% 
  st_drop_geometry() %>% 
  dplyr::select(1,5) %>% 
  dplyr::rename(Neigh = pri_neigh,  
                Area = areas) 

cbind(smallest, largest) %>% knitr::kable( 
      booktabs = T, 
      align = c("r"), 
      caption = "Size of the Largest and Smallest Neighborhoods in Chicago", 
      col.names = c("Neighborhood", "Area (m^2)","Neighborhood", "Area (m^2)")) %>% 
  kableExtra::kable_styling("striped", full_width = F, 
                position = "left", font_size = 12) %>% 
  kableExtra::add_header_above(c("Smallest" = 2, "Largest" = 2)) 

rm(areas, smallest, largest, areas_geog, areas_proj, equal_areas_proj, res)

#' 
#' ## Length calculations
#' 
#' - Calculate the length of a linestring
#' 
## ----Length 1, message = F, echo = F-------------------------------------------------------------------------------------------------
NU_campuses <- st_linestring( 
    rbind(
      c(-87.676,42.057), # tech
      c(-87.621,41.895) # loop campus
    )
  ) %>% 
  st_sfc() %>% 
  st_as_sf(crs = 4326)

#' 
## ----Length 2, message = F, comment = ""---------------------------------------------------------------------------------------------
st_length(NU_campuses)

#' 
#' ## Boundary calculations
#' 
#' - 'convert' the exterior of a polygon to a linestring (more on this later...)
#' - apply st_length
#' - Edzar makes you do spatial problem solving **quite often**
#' 
## ----Boundary 1, message = F, warning = F, comment = ""------------------------------------------------------------------------------
rogers_park <- chi_neighb %>% 
  filter(pri_neigh == 'Rogers Park')

rogers_park <- st_cast(rogers_park, to = 'MULTILINESTRING')
rogers_park <- st_cast(rogers_park, to = 'LINESTRING')

#' 
## ----Boundary 2, message = F, warning = F, comment = ""------------------------------------------------------------------------------
st_length(rogers_park)

#' 
## ---- echo = F-----------------------------------------------------------------------------------------------------------------------
rm(rogers_park, NU_campuses)

#' 
#' # Centroids & Point on Surface
#' 
#' - Centroid the "geometric center of mass of a geometry" of an object.  
#' 
#' - If you just need an arbitrary point on the surface of an object Point on Surface will suffice. 
#' 
## ----Centoid and POS, message = F, warning = F---------------------------------------------------------------------------------------
chi_cent <- st_centroid(chi_neighb)
chi_pos <- st_point_on_surface(chi_neighb)

#' 
## ----Centroid and POS Plot, echo = F, fig.align="center", out.width = "75%"----------------------------------------------------------

c_line <- st_linestring( 
    rbind(
      c(1,1), c(4,1), c(4,4),c(1,4)
    )
  )

c_polygon <- st_polygon( 
  list(
    rbind(
      c(1,1), c(4,1), c(4,4),c(1,4), 
      c(0.5, 4.5), c(4.5, 4.5), c(4.5, 0.5), c(0.5,0.5), c(1,1) 
    )
  )
)

c_line_cen <- st_centroid(c_line)
c_poly_cen <- st_centroid(c_polygon)
c_line_pos <- st_point_on_surface(c_line)
c_poly_pos <- st_point_on_surface(c_polygon)

ggplot() +
  geom_sf(data = chi_neighb, fill = "#B3DDF2", color ="#FFFFFF") +
  geom_sf(data = chi_cent, color = "#FF0000") +
  labs(title = "Centroids of Neighborhoods") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 

a <- ggplot(c_line) +
  geom_sf()+
  geom_sf(data = c_line_cen, size = 4, color = 'red') +
  labs(title = 'Centroid') +
  theme_void()

b <- ggplot(c_polygon) +
  geom_sf() +
  geom_sf(data = c_poly_cen, size = 4, color = 'red') +
  theme_void()

c <- ggplot(c_line) +
  geom_sf()+
  geom_sf(data = c_line_pos, size = 4, color = 'red') +
  labs(title = 'Point on Surface') +
  theme_void()

d <- ggplot(c_polygon) +
  geom_sf() +
  geom_sf(data = c_poly_pos, size = 4, color = 'red') +
  theme_void()

cowplot::plot_grid(a, c, b,  d,  nrow = 2)

ggplot() +
  geom_sf(data = chi_neighb, fill = "#B3DDF2", color ="#FFFFFF") +
  geom_sf(data = chi_pos, color = "#FF0000") +
  labs(title = "Point on Surface of Neighborhoods") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

rm(chi_pos, chi_cent, a, b, c,d, c_line, c_polygon, c_line_cen, c_poly_cen, c_line_pos, c_line_pos)

#' 
#' # Combine & Union Features
#' 
#' - *Combine* will form a multipolygon
#'   - Retain all geometries as a multipolygon collection
#'   - Lose the attributes of each feature 
#'   
#' - *Union* will form a multipolygon
#'   - Retain only outer boundary geometries of features
#'   - Lose the attributes of each feature
#' 
## ----Combine and Union---------------------------------------------------------------------------------------------------------------
chi_combine <- st_combine(chi_neighb)
chi_union <- st_union(chi_neighb)

#' 
## ----Combine Plots, echo = F, out.width = "75%", fig.align="center"------------------------------------------------------------------

ggplot() +
  geom_sf(data = chi_combine, fill = "#B3DDF2", color ="#FFFFFF") +
  labs(title = "Combine forms a Multipolygon without feature attributes") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

rm(chi_combine)

#' 
## ----Union Plots, echo = F, out.width = "75%", fig.align="center"--------------------------------------------------------------------
ggplot() +
  geom_sf(data = chi_union, fill = "#B3DDF2", color ="#FFFFFF") +
  labs(title = "Union also forms a multi*polygon without feature attributes") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

#' 
#' ## Cast
#' 
#' - Can be used to extract types of geometries from a multi(polygon, line, point)geometry, or geometry collection. 
#' 
## ----Cast 1, echo = F, message = F, warning = F, message = F-------------------------------------------------------------------------
nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = T)
mpl <- nc$geometry[[4]]

cast_all <- function(xg) { # by Edzar
  lapply(c("MULTIPOLYGON", "MULTILINESTRING", "MULTIPOINT", "POLYGON", "LINESTRING", "POINT"), 
      function(x) st_cast(xg, x))
}

r <- st_sfc(cast_all(mpl))

a <- ggplot(r[[1]], fill = 'red') +
  geom_sf() +
  theme_void() +
  labs(title = class(r[[1]])[2])

b <- ggplot(r[[2]]) +
  geom_sf() +
  theme_void() +
  labs(title = class(r[[2]])[2])

c <- ggplot(r[[3]]) +
  geom_sf() +
  theme_void() +
  labs(title = class(r[[3]])[2])

d <- ggplot(r[[4]]) +
  geom_sf() +
  theme_void() +
  labs(title = class(r[[4]])[2])

e <- ggplot(r[[5]]) +
  geom_sf() +
  theme_void() +
  labs(title = class(r[[5]])[2])

f <- ggplot(r[[6]]) +
  geom_sf() +
  theme_void() +
  labs(title = class(r[[6]])[2])

cowplot::plot_grid(a,b,c,d,e,f)
rm(a,b,c,d,e,f, nc, mpl, cast_all)

#' 
#' - Divide a Multipolygon into individual polygons. 
#' 
## ----Cast 2, message = F, warning = F------------------------------------------------------------------------------------------------
chi_sub <- chi_neighb[sample(size = 30, 1:nrow(chi_neighb)),]
chi_union <- st_union(chi_sub)
chi_cast <- st_cast(chi_union, to = "POLYGON")

#' 
## ----Cast 3, message = F, echo = F, out.width = "75%", fig.align="center"------------------------------------------------------------

a <- ggplot() +
  geom_sf(data = chi_sub, fill = "#B3DDF2", color = "#FF0000") +
  labs(title = "Random Sample") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

b <- ggplot() +
  geom_sf(data = chi_union, fill = "#B3DDF2", color = "#FF0000") +
  labs(title = "Union") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

c <- ggplot() +
  geom_sf(data = chi_cast, fill = sf.colors(length(chi_cast), categorical = TRUE)) +
  labs(title = "Cast") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(a, b, c, ncol = 3)

#' 
#' 
## ----Cast 4, echo = F, message = F, warning = F, comment = "", fig.align='center'----------------------------------------------------
knitr::kable(head(chi_sub[,c(1,5)]), 
             col.names = c('Neighborhood', 'geometry'),
             caption = "Original data")

knitr::kable(chi_union,
             caption = "Unioned Data")

knitr::kable(head(chi_cast),
             caption = "Data Cast to Polygons")

rm(chi_sub, chi_cast, chi_union, a, b, c)
chi_union <- st_union(chi_neighb)

#' 
#' # Buffers & Convex Hulls
#' 
#' - Buffer: Enlarge a feature by a specified distance in X & Y dimensions
#' - Convex Hull: Encapsulate a feature in X & Y dimensions.
#' 
## ----Buffer and Convex Hull 1, message = F, warning = F------------------------------------------------------------------------------
chi_buffer_3k <- st_buffer(chi_union, dist = 3000)
chi_union_sp <- as(chi_union, 'Spatial')

chi_ch_by_neigh <- st_convex_hull(chi_neighb)
chi_ch_cit <- st_convex_hull(chi_union)
chi_ch_buf <- st_convex_hull(st_buffer(chi_union, dist = 3000))

#' 
#' 
## ----Buffer and Convex Hull 2, message = F, warning = F, echo = F, out.width = "75%", fig.align="center", fig.cap = "Buffer"---------
ggplot() +
  geom_sf(data = chi_buffer_3k, fill = "#FF0000") +
  geom_sf(data = chi_neighb, fill = "#B3DDF2", color ="#FFFFFF") +
  labs(title = "Chicago Buffered by 3 kilometers") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

#' 
## ----Buffer and Convex Hull 3, message = F, warning = F, echo = F, out.width = "75%", fig.align="center", fig.cap = "Convex Hull of Chicago Neighborhoods, The City, and a buffer of 3km"----
ggplot() +
  geom_sf(data = chi_ch_buf, color = NA, fill = "#FF0000", line = 2) +
  geom_sf(data = chi_ch_cit, color = NA, fill = "#B3DDF2", lty = 5, line = 2) +
  geom_sf(data = chi_neighb, fill = 'white', border = NA, color = NA) +
  geom_sf(data = chi_ch_by_neigh,  fill = NA, lwd = 1) +
  labs(title = "Convex Hulls of Three Features") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))
  
rm(chi_buffer_3k, chi_ch_cit, chi_ch_buf, chi_union, chi_ch_by_neigh)

#' 
#' # Measuring Distances
#' 
#' Theory: Remember the earth is a ellipsoid, distances must be calculated along the earths curved surface
#' 
## ---- out.width = "35%", fig.align="center", fig.show='hold', echo = F, fig.cap = "Great Circle Distance, by: ChecCheDaWaff"---------
knitr::include_graphics("./pictures/Great_Circle_Distance_CheCheDaWaff.png")

#' 
#' - *P* Point of origin 
#' - *Q* destination
#' - *u* & *v* : antipodal points, positions across the ellipsoid from each other
#' 
## ----Pairwise Distance between three Natural History Museums 1-----------------------------------------------------------------------
AM_FM <- st_distance(AM, FM)
AM_SM <- st_distance(AM, SM)
FM_SM <- st_distance(FM, SM)

#' 
## ----Pairwise Distance between three Natural History Museums 2, echo = F-------------------------------------------------------------
AM_FM <- as.numeric(units::set_units(AM_FM, 'kilometer')) 
AM_SM <- as.numeric(units::set_units(AM_SM, 'kilometer'))
FM_SM <- as.numeric(units::set_units(FM_SM, 'kilometer'))

#' 
## ----Distance between three natural history museums 3, echo = F, fig.align='center'--------------------------------------------------
st_dist_results <- data.frame(rbind(
  c(AM_FM, 'American to Field'), 
  c(AM_SM, 'American to Smithsonian'),
  c(FM_SM, 'Field to Smithonian')
  )
)
colnames(st_dist_results) <- c('Distance_st', 'Journey')
st_dist_results$Distance_st <- round(as.numeric(st_dist_results$Distance_st), 1)

knitr::kable(st_dist_results)
rm(AM_FM, AM_SM, FM_SM, st_dist_results)

#' 
#' We can visualize this with some help from Charlie Joey Hadley whom some sf compliant great circle distance code
#' 
## ----Create Plot of Great Circle Distances, echo = F, fig.align="center", out.width = "75%"------------------------------------------
museums <- rbind(AM, FM, SM) %>%  # we want to map this
  mutate(
    across(
      .cols = c(latitude,longitude), 
      ~as.numeric(as.character(.x))
      )
    )
data(us_states) # we want to map this
us_states <- st_transform(us_states, st_crs(museums)) # we print museums and feed it into states.
bound <- st_bbox(st_buffer(museums, 450000))

journey_data <- as_tibble(rbind(cbind(AM, FM), 
                                cbind(AM, SM),
                                cbind(FM, SM))) %>% 
  mutate(journey = paste0(attribute, " to ", attribute.1)) %>% 
  dplyr::select(-attribute, -attribute.1, -geometry, -geometry.1) %>% 
  rename_with(.cols = everything(), ~ str_replace(.x, "1", "end")) %>% 
  mutate(across(.cols = latitude:longitude.end, ~as.numeric(as.character(.x))))

great_circle_routes <- journey_data %>% 
  journeys_to_sf() %>% 
  st_segmentize(units::set_units(100, km)) 

data(world)
world <- filter(world, name_long == 'Canada')

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = us_states) +
  geom_sf(data = museums, size = 3) +
  geom_sf(data = great_circle_routes, lwd = 1) +
  coord_sf(xlim = c(bound[1],bound[3]), ylim = c(bound[2],bound[4])) +
  labs(title="Great Circle Distance Between Three Museums of Natural History") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))
  #geom_sf_label(data = museums, aes(longitude, latitude, label = attribute), colour = "black")

#' 
## ----Length on Great Circle Distance Linestrings-------------------------------------------------------------------------------------
gcr_distance <- st_length(great_circle_routes)

#' 
## ----Clear environment after Great Circle, echo = F----------------------------------------------------------------------------------
rm(AM, FM, SM, bound, museums, journey_data, great_circle_routes, gcr_distance, us_states, journeys_to_sf, world)

#' 
#' In this example we calculate the distance between each of the 98 neighborhoods in Chicago, sum up the total distance of each neighborhood from each other, and divide by the number of neighborhood (98) minus itself (1) to collect and visualize a mean distance between all neighborhoods. 
#' 
## ----A dense distance matrix, message=F, warning = F---------------------------------------------------------------------------------
neighborhood_centroid_dist <- st_distance(st_centroid(chi_neighb))

#' 
## ----A dense distance matrix B, echo = F, out.width = "75%", fig.align="center"------------------------------------------------------
neighborhood_centroid_dist <- units::drop_units(neighborhood_centroid_dist)
mean_distance <- colSums(neighborhood_centroid_dist)/(nrow(neighborhood_centroid_dist)-1)
chi_neighb1 <- cbind(chi_neighb, mean_distance) 

ggplot() +
  geom_sf(data = chi_neighb1, aes(fill = mean_distance), alpha = 0.95) +
  scale_fill_viridis_c(option = "plasma") +
  labs(fill="Mean Distance (m)") +
  labs(title="Mean Distance Between all Neighborhoods in Chicago") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

chi_neighb1 %>% 
  st_drop_geometry() %>% 
  arrange(mean_distance) %>% 
  slice(n = 1:5) %>% 
  dplyr::select(pri_neigh, mean_distance) %>% 
  knitr::kable(
  caption = "The five most Central Chicago Neighborhoods ",
  col.names = c("Neighborhood",'Distance'))

rm(neighborhood_centroid_dist, mean_distance, chi_neighb1)

#' 
#' # An introduction to some types of Spatial Analyses
#' 
#' - Two very popular and simple linear models
#' - **S**patial **A**utoregressive **M**odels (SAR), and **C**onditional **A**utoregressive **M**odels (CAR)
#' - Both require knowledge of the distance between observations to weigh values 
#' 
#' ## Identifying Distance Based Neighbors
#' 
#' - One can find neighboring objects via distance thresholds using the spdep package
#' - these neighbors can be used in many types of linear regression for spatial analyses
#' 
#' Earlier in the lecture we saw that we could develop a list of neighbors from polygon geometries based on adjacency and whether the polygons touched. We can also identify the neighbors of geometries based on the distance between them. Here we will use a set of Points to do this. 
#' 
#' Each of these points represents the location of a coastal dune loving plant in Northern California. 
#' 
## ----Distance Based Neighbors, echo = F, comment = "", fig.align="center"------------------------------------------------------------
plants <- read.csv("./spatial_lecture_data/Solidago_Erigeron_Lanphere.csv") %>% 
  st_as_sf(coords = c(y = 'longitude', x ='latitude'), crs = 4269)  %>% 
  st_transform(32610) %>% 
  distinct(individual, .keep_all = T) 

dat1 <- plants %>%
    mutate(longitude = unlist(map(plants$geometry,1)),
           latitude = unlist(map(plants$geometry,2))) %>% 
  st_drop_geometry() %>% 
  dplyr::select(-taxon)

neighborhood_object <- dnearneigh(plants, d1 = 1, d2 = 200) # identify neighbors
summary(neighborhood_object)

links <- neighborhood_object %>% 
  purrr::map(., as.integer) %>% 
  purrr::map(., as_tibble) %>% 
  map2_df(plants$individual,~mutate(.x,Focal_id=.y)) %>% 
  rename(Neighbor = value) %>% 
  left_join(., dat1, by = c('Focal_id' = 'individual')) %>% 
  rename(longitude.id = longitude,
         latitude.id = latitude) %>% 
  left_join(., dat1, by = c('Neighbor' = 'individual')) %>% 
  rename(longitude.nb = longitude,
         latitude.nb = latitude)  %>% 
  drop_na() %>% 
  mutate(Link = as.numeric(paste0(Focal_id, Neighbor)))

links <- as.matrix(links)
  
ex_linestring <- list(0)
for (i in 1:nrow(links)){
  ex_linestring[[i]] <- rbind(
        c(links[i, 3], links[i, 4]), 
        c(links[i, 5], links[i, 6]),
        c(links[i, 3], links[i, 4])
      )
}

ex_linestring <- lapply(ex_linestring, st_linestring)

link_collection <- ex_linestring %>%
  st_sfc(crs = 32610) %>%
  st_sf(geometry = .)

link_collection <-  link_collection %>% 
  mutate(length = as.numeric(st_length(link_collection))) %>% 
  filter(length >= 1 & length <= 200) 

ggplot() +
  geom_sf(data = link_collection, color = "lightblue2") + 
  geom_sf(data = plants, color = "darkorchid3", size = 1.5) + 
  coord_sf(ylim = c(4527046, 4527830), xlim = c(403150,403700)) + 
  labs(title = "Neighbors within 200 meters") + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) 

rm(ex_linestring, neighborhood_object, i, links)

#' 
#' From a distance neighbor based linkage analysis, we can for, example determine how many individual plants of this species are located in a close enough proximity that a pollinator may cross pollinate individuals. In the readout from this analysis we see that each plant has the potential to 
#' 
#' ## Kernel Density Estimation of a Point Pattern
#' 
#' - estimate the *intensity* in a sense, the density of a process in space
#' - more widely used for building models than estimating densities via projections
#' 
#' Based on the occurrence's of where individual plants are located, we may be interested in estimating how many plants grow in each unit of area across this landscape. Kernel density estimation may be used to accomplish this. Here this calculation will generate it's predictions on a surface which we may map. Areas which are more orange, have higher *intensity* that is the estimated average density of points per cell. 
#' 
## ----Kernel Density of a Point Pattern Process, echo = F, fig.align="center"---------------------------------------------------------
plants <- read.csv("./spatial_lecture_data/Solidago_Erigeron_Lanphere.csv") %>% 
  filter(longitude <= -124.1332, latitude >= 40.88581) %>% 
  st_as_sf(coords = c(y = 'longitude', x ='latitude'), crs = 4269)  %>% 
  st_transform(32610) %>% 
  distinct(individual, .keep_all = T) 

pp1 <- as.ppp(st_geometry(plants))
q <- density(pp1)

plot(density(pp1), main = "Intensity of a Plant Across a Landscape")

rm(q)

#' 
#' Note that our field data do not by themselves indicate the abundance or density of individuals in the landscape. While it may appear we are just shading based on the number of points, we actually have developed a new smoothed prediction of intensity. 
#' 
#' ## Cluster a Point Pattern
#' 
#' - **D**ensity-**B**ased **S**patial **C**lustering of **A**pplications with **N**oise (DBSCAN).
#' - Classification algorithm for grouping objects in space
#' - Developed in the mid 90's, still used and highly cited!
#' 
#' - Requires two terms
#'   - minium points: the number of points required to form a cluster
#'   - eps: loosely related to distance for which neighbor searches can occur
#' 
#' - Ripley's K
#'   - Determine whether a point process is not randomly distributed
#'   - Points can be fit with models of distributions
#'   - segments of a models fit above the poisson curve have clustered distributions
#'   - a process which can be worth modeling.
#' 
#' If we combine the ideas of aggregating individuals in space based upon their proximity, with the estimation of the intensity of points, we come out with the ability to aggregate spatial objects into groups based upon nothing more than their relationships in space. Here I showcase the ability to use the DBSCAN algorithm to quickly assign each individual point to a group. 
#' 
#' This is a bit of a low hanging fruit for this algorithm, i.e. we could have readily assigned most of these groups accurately ourselves. But do take note of the few connections which are not assigned to groups, and understand the power of this technique to identify tangentially associated localities. 
#' 
#' For example, if after calculating Ripley's K, we see that the distribution of our data differ in any way from a random point process, we have evidence that our points are not randomly distributed. When our line is above the curve of a poisson point process their is statistically significant evidence of clustering of points, when the line is beneath the poisson curve, than their is evidence of overdispersion. 
## ----Cluster Points in Space Ripleys K, echo = F, warning = F, message = F, out.width="50%", fig.align="center"----------------------
ripleys_K <- Kest(pp1, correction="all")
plot(ripleys_K)

#' 
## ----Cluster Points in Space - DBSCAN, echo = F, warning = F, message = F, fig.align="center"----------------------------------------

dat1 <- data.frame(dat1[,2:3])
clustered <- fpc::dbscan(dat1, eps = 70, MinPts = 3)
dat1$cluster <- clustered$cluster

dat1 <- dat1 %>% st_as_sf(coords = c(y = 'longitude', x ='latitude'), crs = 32610) 
hulls <- dat1 %>% 
  filter(cluster != 0) %>% 
  st_buffer(25) %>% 
  group_by(cluster) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_convex_hull() %>% 
  mutate(cluster = as_factor(cluster))

dat1 <- dat1 %>% 
  mutate(cluster = as_factor(cluster))
ggplot() +
  geom_sf(data = hulls, aes(fill = cluster), alpha = 0.5) +
  geom_sf(data = dat1,  color = "darkorchid3") +
  coord_sf(ylim = c(4527046, 4527830), xlim = c(403150,403700)) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#d95f02','#e7298a'))+
  labs(title = "DBSCAN Clusters") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

rm(pp1, ripleys_K)

#' 
## ----Visualize Clusters and Distance based Neighbors, echo = F, warning = F, message = F, fig.align="center"-------------------------
ggplot() +
  geom_sf(data = link_collection) +
  geom_sf(data = hulls, aes(fill = cluster), alpha = 0.5) +
  geom_sf(data = dat1,  color = "darkorchid3") +
  coord_sf(ylim = c(4527046, 4527830), xlim = c(403150,403700)) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#d95f02','#e7298a'))+
  labs(title = "DBSCAN Clusters with Neighbors") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

#' 
## ----Clear environment from main lecture, echo = F, warning = F, message = F---------------------------------------------------------
rm(chi_neighb, clustered, dat1, hulls, link_collection, r, plants)

#' 
#' 
#' # Wrapping up the Spatial Data Science Module:
#' 
#' **Future Bonus SDS Office Hours**
#' Wednesday Night at 5:00 - 6:00 in F-413.
#' 
#' **Notes on this Lab**
#' My lecture notes are in the R script I used to generate all of the novel figures for this presentation. This script is much longer and more complex than Lecture 1's script. Likewise this presentation is an .HTML file and can be launched from your computer (it was rendered directly from R using the script). 
#' 
#' # Works Cited
#' 
#' https://cran.r-project.org/web/packages/gstat/vignettes/gstat.pdf Accessed 1.21.2021
#' 
#' Bivand, R. https://cran.r-project.org/web/packages/spdep/vignettes/nb_sf.html Accessed 1.17.2021
#' 
#' Clementini, Eliseo; Di Felice, Paolino; van Oosterom, Peter (1993). "A small set of formal topological relationships suitable for end-user interaction". In Abel, David; Ooi, Beng Chin (eds.). Advances in Spatial Databases: Third International Symposium, SSD '93 Singapore, June 23–25, 1993 Proceedings. Lecture Notes in Computer Science. 692/1993. Springer. pp. 277–295. doi:10.1007/3-540-56869-7_16. ISBN 978-3-540-56869-8. Accessed 1.16.2021
#' 
#' Egenhofer, M.J.; Herring, J.R. (1990). "A Mathematical Framework for the Definition of Topological Relationships"
#' 
#' Ester, Martin; Kriegel, Hans-Peter; Sander, Jörg; Xu, Xiaowei (1996). Simoudis, Evangelos; Han, Jiawei; Fayyad, Usama M. (eds.). A density-based algorithm for discovering clusters in large spatial databases with noise. Proceedings of the Second International Conference on Knowledge Discovery and Data Mining (KDD-96). AAAI Press. pp. 226–231. CiteSeerX 10.1.1.121.9220. ISBN 1-57735-004-9.
#' 
#' Hadley, C.J. https://www.findingyourway.io/blog/2018/02/28/2018-02-28_great-circles-with-sf-and-leaflet/ Accessed 1.16.2021
#' 
#' Gimond, M. https://mgimond.github.io/Spatial/point-pattern-analysis-in-r.html Accessed 1.22.2022
#' 
#' Moreno, M., Basille, M. https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html Accessed 1.22.2022
#' 
#' Dennett, A. https://rstudio-pubs-static.s3.amazonaws.com/126356_ef7961b3ac164cd080982bc743b9777e.html Accessed 1.21.2022
#' 
#' ## Packages cited:
## ----Packages cited, comment = ""----------------------------------------------------------------------------------------------------
c("raster", "sp", "sf", "tidyverse", "terra", 'spData', 'spdep', 'gstat', 'spatstat', 'fields' ) %>%
  map(citation) %>%
  print(style = "text", na.print = '')

