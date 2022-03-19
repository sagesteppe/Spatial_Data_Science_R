#' ---
#' title: "Raster interpolation"
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
#' 
## ----Load Libraries, message = F, warning = F, echo = F------------------------------------------------------------
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
## ----Import Chicago Neighborhoods Polygons, warning = F, echo = F--------------------------------------------------
files <- paste0('./spatial_lecture_data/Chicago_Neighborhoods/',
                list.files('./spatial_lecture_data/Chicago_Neighborhoods', ".shp"))
chi_neighb <- read_sf(files)
rm(files)

#' 
## ----Import Chicago Park District Polygons, warning = F, echo = F, cache = T---------------------------------------
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
#' 
## ----Create Interpolation Visual Aids, echo = F, message = F, warning = F------------------------------------------

locations_pts <- as.data.frame(cbind(
  site = sample(letters, size = 20, replace = F),
  x = sample(-10:10, size = 20), 
  y = sample(-10:10, size = 20)))

locations_lines <- data.frame(cbind(
  locations_pts[,2:3],
  cen_x = rep(0.5, size = 20), 
  cen_y = rep(0.5, size = 20)))

locations_lines <- lapply(locations_lines, as.numeric)
locations_lines <- do.call(cbind, locations_lines)

ex_linestring <- list(0)
for (i in 1:nrow(locations_lines)){
  ex_linestring[[i]] <- rbind(
        c(locations_lines[i, 1], locations_lines[i, 2]), 
        c(locations_lines[i, 3], locations_lines[i, 4]),
        c(locations_lines[i, 1], locations_lines[i, 2])
      )
}

locations_lines <- st_multilinestring(ex_linestring)

locations_pts <- st_as_sf(locations_pts, coords= c(x='x', y ='y'), remove = F)
ex_point <- st_point(c(0.5,0.5))

locations_pts <- locations_pts %>% 
  mutate(distance = -log(st_distance(locations_pts, ex_point)))

r_ext <- raster::raster(
    xmn = -10, 
    xmx = 10,
    ymn = -10,
    ymx = 10
)
r_ext <- st_bbox(r_ext)

grid <- st_make_grid(r_ext,  # create a fishnet to emulate the idea of an empty raster ! :-)
  n = c(10, 10),
  what = "polygons",
  square = TRUE,
  flat_topped = FALSE)
grid <- st_multipolygon(grid)

rm(ex_point, i, ex_linestring, r_ext)

#' 
#' # Spatial autocorrelation 
#' 
#' ## Tobler's First Law
#' 
#' "I invoke the first law of geography: 
#' everything is related to everything else, 
#' but near things are more related than distant things." 
#' - Waldo Tobler
#' 
#' Accordingly, if we want to model processes in space, 
#' considerable information may be found from those locations in closet
#' proximity. 
#' 
#' ## Autocorrelation
#' 
#' The correlation between two values of a variable as function of the 'lags' between them. 
#' 
#' ### Temporal Autocorrelation
#' 
#' If we were to measure the height of a fast growing plant in a greenhouse every week for a year, each measured value would be strongly related to the value collected the week preceding it.
#' 
## ----Temporal Autocorrelation 1, echo = F, out.width="50%", comment = "", fig.align="center"-----------------------

plant_hgt <- sample(1000, 56, replace = F)
plant_hgt <- sort(plant_hgt)

a <- plant_hgt[-length(plant_hgt)]
b <- plant_hgt[-1]

plot(x = b, y = a, xlab = "Height in millimeters t1", 
     main = "Relationship between Plant height at week t and week t-1", 
     ylab = "Height in millimeters t-1")
  abline(0,1, lty = 2)

cor(a, b)
acf(plant_hgt)

rm(a, b, plant_hgt)

#' 
#' Until we go to around 15 lags away -  or in other words 15 weeks back, our data set is significantly auto-correlated. In statistics this will adversely affect the results of our models. However, this information can also be used. 
#' 
#' ## Spatial Autocorrelation
#' 
#' Spatial auto correlation is a 2 dimensional phenomenon, and is slightly harder to visualize. 
#' 
## ----Spatial autocorellation example, echo = F, fig.cap= "Example figure of spatial autocorrelation. From L to R, Positive, None, Negative"----

r1 <- raster(nrow = 8, ncol = 7)

m1 <- matrix(rep(0:1, each = 28), 
             nrow = 8,
             ncol = 7,
             byrow=TRUE)
r1a <- setValues(r1, m1)

m2 <- matrix(sample(0:1, size = 56, replace = T), 
             nrow = 8,
             ncol = 7,
             byrow=TRUE)
r2a <- setValues(r1, m2)

m3 <- matrix(rep(0:1, each = 1), 
             nrow = 8,
             ncol = 7,
             byrow=T)
r3a <- setValues(r1, m3)

a <- tmap::tm_shape(r1a) +
  tmap::tm_raster(style= "cat", title="Positive",
                  palette = c("black", "white"), legend.show = F)

b <- tmap::tm_shape(r2a) +
  tmap::tm_raster(style= "cat", title="None",
                  palette = c("black", "white"), legend.show = F)

c <- tmap::tm_shape(r3a) +
  tmap::tm_raster(style= "cat",  title="Negative",
                  palette = c("black", "white"), legend.show = F)

tmap::tmap_arrange(a, b, c, ncol = 3, outer.margins = NULL)

rm(r1, m1, m2, m3, r1a, r2a, r3a, a, b, c)

#' 
#' ### Calculate Morans I
#' 
#' Here we will see if there is a an spatial pattern in the presence of Park Areas in Chicago. First, we will intersect each park to the neighborhood they are in. Then we will determine what proportion of each neighborhood is Park.
#' 
## ----Spatial Autocorrelation 2, echo = F, comment = "", fig.cap = "Chicago Neighbors of Neighborhoods", fig.align="center"----

chi_neighb <- left_join(chi_neighb, park_areas, by = "pri_neigh") %>% 
  mutate(neigh_area = as.numeric(st_area(.)/1000)) %>% 
  mutate(prop_park = (total_park_area/neigh_area)*100) %>% 
  mutate(prop_park = replace_na(prop_park, 0)) %>% 
  dplyr::select(-total_park_area, neigh_area) 

chi_neigh_sp <- as(chi_neighb, 'Spatial') # convert so sp

neighborhood_object <- poly2nb(chi_neigh_sp, row.names=chi_neigh_sp$pri_neigh) 
# identify neighbors
chi_neighb_coords <- coordinates(chi_neigh_sp) # extract coords for plotting 

#plot(chi_neigh_sp, col = "#B3DDF2")
#plot(neighborhood_object, chi_neighb_coords, col="#FF0000", lwd=2, add=TRUE)
summary(neighborhood_object)

spatial_weights_list <-  nb2listw(neighborhood_object, style='B')

monte_carlo_moran<- moran.mc(chi_neigh_sp$prop_park, spatial_weights_list, nsim=999)

monte_carlo_moran

rm(monte_carlo_moran, neighborhood_object, chi_neighb_coords, spatial_weights_list, chi_neigh_sp, park_areas)

#' 
#' The null hypothesis of this test is that the data are randomly distributed
#' The statistic ranges from 1 (perfect clustering of similar values) to -1 (perfect dispersion; opposites values close).
#'  
#' This test tells us that there is some spatial structure in our data, but we do not know the geographic range to which this structuring is present. 
#' 
#' If we recall the location of the Chicago Parks, the most noticeable cluster of them is along the Lake Shore. I presume that the Moran.I index is slightly above random due to the presence of these. It also points out an interesting point regarding the history of the development of Chicago. A focus on restricting development along the Shore.  Chicago is quite unique in that it's coasts are publicly accessible, and can largely be owed to the contributions of Montgomery Ward.
#' 
#' This amount of spatial auto-correlation would not adversely affect analyses, not too mention, this is an real pattern of these data.
#' 
#' https://www.chicagotribune.com/news/ct-xpm-1995-10-19-9510190079-story.html 02.04.2022 By: Stephen Lee and Tribune Staff Writer. Chicago Tribune. 10.19.1995
#' 
#' https://www.fotp.org/lakefront-protection-and-public-trust.html 02.04.2022
#' 
#' # Change Grain of Raster Cells. 
#' 
#' - Spatial *Extent* of projects often constrained by areas funding (political boundaries via funds from agencies), or computation power. 
#' - *Grain* of projects almost always constrained by computer power, or data sets.
#' 
#' Within an *extent* an analyst wants to find a *grain* relevant to the their study. *Grain* can be, somewhat readily, altered. 
#' 
#' ## Raster Cell Aggregation (Coarser Grain)
#' 
#' Rasters may be of a resolution which is to fine to warrant use due to computational limits.
#' 
## ----Raster Cell Aggregation, echo = F, fig.cap = "Raster Aggregation", out.width = "75%", fig.align="center"------
mycores <- parallel::detectCores() - 1 # this process can be made parallel
# We will use a logical thread on our CPI's to work across the raster in chunks. 

val_RCA_ex <- seq(from = 1, to = 9801, by = 1)
r_RCA_ex <- raster::raster(nrows = 99, ncols = 99)
r_RCA_ex <- setValues(r_RCA_ex, val_RCA_ex)

ra_max <- raster::aggregate(r_RCA_ex, 
                fact=3, 
# factor to divide cells by 100 original cells / factor 10 = 10 cells left.
                fun=max, 
# which mathematical function to apply. 
                na.rm = T,
                cores = mycores, 
# make process parallel. Good for LARGE jobs. 
#filename = , # results can be written straight to file. 
                overwite = FALSE,
                )

ra_mean <- raster::aggregate(r_RCA_ex, fact= 9, fun=mean, na.rm = T, cores = mycores)
ra_mean_2 <- raster::aggregate(r_RCA_ex, fact= 18, fun=mean, na.rm = T, cores = mycores)

par(mfrow = c(2,2), mai = c(0.5, 0.5, 0.5, 1), oma = c(0,0,0,1.1))
plot(r_RCA_ex, legend = F, main = "Original Cell Size")

plot(ra_max, main = "Aggregated by factor of 3", 
     legend.args = list(text = 'Values', side = 4,
         font = 2, line = 2.5, cex = 0.8))

plot(ra_mean, legend = F, main = "Aggregated by factor of 9")

plot(ra_mean_2, main = "Aggregated by factor of 18", 
     legend.args = list(text = 'Values', side = 4,
         font = 2, line = 2.5, cex = 0.8))

rm(val_RCA_ex, r_RCA_ex, ra_max, ra_mean, ra_mean_2, mycores)

#' 
#' ## Raster Interpolation (Finer Grain)
#' 
#' - Predict the value of a variable at an un-sampled location
#' - Using the values of this variable at sampled locations
#' - Utilizes the property of spatial auto-correlation
#' 
#' While a great number of raster data sets are developed form satellite imagery and represent the classification of observed values at each location in space, other raster data sets have values which are predicted in space. The most evident example of these are raster products of climate variables. All rasters of climate variables are based on measurements taken from meteorological stations, and then the values between these stations are predicted using spatial interpolation.
#' 
## ---- out.width = "50%", fig.align="center", fig.show='hold', echo = F, fig.cap = "Surveying in the White Cloud Mtns. Idaho, by Hubert Szycygiel"----
knitr::include_graphics("./pictures/Moving_survey_equipment.HSzycygiel.JPG")

#' 
#' Two of the most commonly used spatial interpolation techniques are 'Inverse Distance Weighting' and 'Kriging' interpolation. Our interaction with these processes will be brief, but we will illustrate their uses so that you may recognize the source of spatial data products in the future. 
#' 
#' We will leverage simple versions of these techniques to create Rasters of finer resolution than they are currently. 
#' 
#' ### Inverse Distance Weighting Theory
#' 
#' - First Interpolation Method
#' - Values at points further in space have less weight
#' - Scale of weight generally decreases linearly or linear^2 
#' 
## ----Inverse Distance Weighting Interpolation Theory, echo = F, fig.align="center", fig.cap = "Inverse Distance Weighing Interpolation Theory", out.width = "75%"----

ggplot() +
  geom_sf(data = grid, aes(color = "mediumorchid4")) +
  geom_sf(data = locations_lines, alpha = 0.8) +
  geom_sf(data = locations_pts, aes(size=distance, color = "darkturquoise")) +
  xlim(-10, 10) +
  ylim(-10, 10) +
  labs(title="Inverse Distance Weighting", sub = "Point size is the inverse distance ^2 to focal location") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "") 

rm(locations_lines, locations_pts, grid)

#' 
## ----IDW Interpolation of a Raster, echo = F, warning = F, message = F, comment = ""-------------------------------
elevation_raster <- rast(system.file("ex/elev.tif", package="terra")) 
elevation_raster_coarse <- raster::aggregate(elevation_raster, 2) 
coords <- data.frame(xyFromCell(elevation_raster_coarse, 1:ncell(elevation_raster_coarse)))
rast_vals <- values(elevation_raster_coarse) 
index <- !is.na(rast_vals) 
coords <- coords[index,]

rast_vals <- rast_vals[index]
prediction_IDW <- rast(elevation_raster)

my_sample <-  sample(1:nrow(coords), round(nrow(coords)*0.7, 0))

rast_vals <- rast_vals[my_sample]
coords <- coords[my_sample,]
points <- cbind(rast_vals, coords) %>% 
  st_as_sf(coords = c(x = 'x', y = 'y'), crs = 4326) 
points <- as(points, 'Spatial')

tps <- fields::Tps(coords, rast_vals)
prediction_IDW <- interpolate(prediction_IDW, tps)
prediction_IDW <- mask(prediction_IDW, elevation_raster)

rm(tps, index, rast_vals, my_sample)

#' 
#' To read more about Inverse Distance Weighting, and view worked out calculations of the process please visit: 
#' https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/geostatistics/Inverse-Distance-Weighting/index.html
#' 
#' Inverse Distance Weighting is the original spatial interpolation technique and makes great use of Tobler's First Law. While using this technique, we want to predict the value of a variable, such as rainfall, in a location. 
#' 
#' ## Kriging Interpolation
#' 
#' https://mgimond.github.io/Spatial/interpolation-in-r.html
#' 
#' ### Semivariogram
#' 
#' A semivariogram is a method for measuring the correlation between observations at sets of distance.
#' 
## ----Example semivariogram, out.width = "50%", fig.align="center", echo = F----------------------------------------
semivar_data <- data.frame(
  Distance =     c(0,3,5,9.00,12,15, 20,23.0,25,28,31,34.0,36.0,38.0,40,42.1,44,47,55),
  Semivariance = c(4,6,7,10.0,14,17, 20,20.2,20,20,20,20.5,19.8,19.7,20,20.7,20,20,20)
)

ggplot(semivar_data, aes(Distance, Semivariance)) +
  geom_point(color = "#984ea3") +
  geom_smooth(method = 'loess',  se = FALSE, span = 0.8, col = "#e41a1c") +
  theme_bw() +
  
  xlim(0, 60) +
  ylim(0, 30) +
  
  labs(title = "Example Semivariogram") +
  theme(plot.title = element_text(hjust = 0.5)) +
  
  geom_segment(aes(x = 23, y =  0, xend = 23, yend = 20), lwd = 1.25, lty = 6) +
  geom_segment(aes(x =  0, y = 20, xend = 23, yend = 20), lwd = 1.25, lty = 6) +
  geom_segment(aes(x =  0, y =  0, xend =  0, yend =  3), lwd = 1.25, lty = 6) +
  geom_text(x=25, y=10, label = "Sill") +
  geom_text(x=10, y=21, label = "Range") +
  geom_text(x= 3, y= 1, label = "Nuggett") +
  
  #geom_segment(aes(x = 22, y = 25, xend = 0, yend = 25), arrow = arrow(length = unit(0.03, "npc"))) #+
  #geom_segment(aes(x = 25, y = 25, xend = 47, yend = 25),
  #  arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(x=10, y=27, label= "Autocorrelated") +
  geom_text(x=35, y=27, label = "Uncorrelated") 


rm(semivar_data)

#' 
#' - Nugget: Variation due to errors in the observed measurements. 
#' - Sill: Distance at which the maximum amount of variance in the data is reached.
#' - Range: Distance at which observations are independent. 
#' 
#' - Model: The red line indicates a model which has been fitted to the points (observations), in this example with fictitious data the model is closet to a spherical model. 
#' 
#' To determine the range of spatial auto-correlation we can use a Semivariogram
## ----Semivariogram on Elevation Data, fig.align="center"-----------------------------------------------------------
variogram_elevation <- variogram(rast_vals~1, data = points)

FittedModel <- fit.variogram(variogram_elevation, vgm(c( "Exp", "Sph", "Gau", "Mat"), 
                                 fit.kappa = TRUE))

a <- plot(variogram_elevation, main = "Variogram of Elevation Points")
b <- plot(variogram_elevation, model=FittedModel, main = "Fitted Variogram of Elevation Points", yaxt="n")
cowplot::plot_grid(a, b,  ncol = 1)

rm(variogram_elevation)

#' 
#' We can also view directions with predominant auto-correlation.
#' 
## ----Test for Anisotropy, warning = F, message = F, echo = F, out.width = "75%", fig.align="center"----------------

vgm.aniso <- variogram(rast_vals ~ 1, points, alpha = c(0, 45, 90, 135))
direction <- split(vgm.aniso, f= vgm.aniso$dir.hor)

fit_zero <- fit.variogram(direction[['0']], vgm(c( "Exp", "Sph", "Gau", "Mat"), 
                                 fit.kappa = TRUE))
fit_45 <- fit.variogram(direction[['45']], vgm(c( "Exp", "Sph", "Gau", "Mat"), 
                                 fit.kappa = TRUE))
fit_90 <- fit.variogram(direction[['90']], vgm(c( "Exp", "Sph", "Gau", "Mat"), 
                                 fit.kappa = TRUE))
fit_135 <- fit.variogram(direction[['135']], vgm(c( "Exp", "Sph", "Gau", "Mat"), 
                                 fit.kappa = TRUE))
  
a <- plot(direction[['0']],   model=fit_zero,  main = "0°", ylim = c(0,7000),  xlab = "",)
b <- plot(direction[['45']],  model=fit_45,    main = "45°", ylim = c(0,7000), ylab = "", yaxt="n",  xlab = "",)
c <- plot(direction[['90']],  model=fit_90,    main = "90°", ylim = c(0,7000))
d <- plot(direction[['135']], model=fit_135,   main = "135°", ylim = c(0,7000), ylab = "", yaxt="n")

# ggplot(aes(x = dist, y = gamma), data = vgm.aniso) + 
#   facet_wrap(~dir.hor) + 
#   geom_point() + 
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5)) + 
#   labs(x = 'Distance', y = 'Gamma', 
#        title = 'Detecting Directional Anisotropy with Variograms')

cowplot::plot_grid(a, b, c, d,  ncol = 2)

#fit_test <- vgm(as.character(FittedModel$model[2]), as.character(FittedModel$range[2]), anis = c(90, 0.5))

rm(vgm.aniso, direction, fit_zero, fit_45, fit_90, fit_135, a , b, c, d)

#' 
#' We see that heading in both 45 and 90 degree angles the effect of spatial auto-correlation deteriorates
#' 
#' ## Kriging Inteprolation applied
#' 
## ----Kriging Interpolation,  warning = F, message = F, echo = F----------------------------------------------------
coords1 <- data.frame(xyFromCell(elevation_raster, 1:ncell(elevation_raster)))
gridded(coords1) = ~x+y
crs(coords1) <- crs(points)

pred.reg <- krige(rast_vals ~ 1, points, coords1, model = FittedModel)
pred.reg <- rast(raster::raster(pred.reg))
pred.reg <- mask(pred.reg, elevation_raster)

rm(coords, coords1, FittedModel)

#' 
#' ## Compare Interpolated Values
#' 
#' We can visually assess the results of our two interpolations using maps. 
#' 
## ----Comparision of Interpolated Values 1, echo = F, warning = F, message = F,out.width = "75%", fig.cap="Comparision of Interpolated Values", fig.align="center"----

a <- tmap::tm_shape(elevation_raster) +
  tmap::tm_raster(style= "cont", title="Original", 
                  palette = "viridis", legend.reverse = T, 
                  breaks = c(200, 300, 400, 500)) +
  tmap::tm_layout(legend.position = c("right","top")) 

b <- tmap::tm_shape(elevation_raster_coarse) +
  tmap::tm_raster(style= "cont", title="Aggregate", palette = "viridis", 
                  legend.reverse = T, breaks = c(200, 300, 400, 500)) +
  tmap::tm_layout(legend.position = c("right","top")) 

c <- tmap::tm_shape(prediction_IDW) +
  tmap::tm_raster(style= "cont", title="IDW", 
                  palette = "viridis", legend.reverse = T, 
                  breaks = c(200, 300, 400, 500)) +
  tmap::tm_layout(legend.position = c("right","top"))

d <- tmap::tm_shape(pred.reg) +
  tmap::tm_raster(style= "cont", title="Krige", 
                  palette = "viridis", legend.reverse = T, 
                  breaks = c(200, 300, 400, 500)) +
  tmap::tm_layout(legend.position = c("right","top"))

b <- b + tmap::tm_shape(points) +
  tmap::tm_dots(title="Locations", col = 'mediumvioletred')

tmap::tmap_arrange(a, b, c, d, widths = c(.5, .5))

rm(a, b, c, d, points)

#' 
#' Remember after importing the *Original* dataset, we merged four cells into one to create our *Aggregate* dataset. We then sampled 1/4 of the aggregated cells, to compute both the *IDW* and *Krige* rasters. we can see that both of them 'smooth' the Aggregated raster. Based on visual inspection, both of our interpolated surfaces appear to match up with the Original raster quite well - for many applications.
#' 
## ----Comparision of Interpolated Values 2, warning = F, message = F, echo = F, comment = "", out.width="50%", fig.align="center"----

reference <- spatSample(elevation_raster, size=3000, cells=TRUE) %>% drop_na() %>% mutate(Type = "Original")
aggregated_ref <- spatSample(elevation_raster_coarse, size=3000, cells=TRUE) %>% drop_na() %>% mutate(Type = "Reference_Aggregated")
krige <- spatSample(pred.reg, size=3000, cells=TRUE) %>% drop_na() %>% mutate(Type = "Krige") %>% rename(elevation = var1.pred)
idw <- spatSample(prediction_IDW, size = 3000, cells = T ) %>% drop_na() %>% mutate(Type = "IDW")

r <- min(
  t(
    as.data.frame(
      (lapply
       (as.list(c(reference, aggregated_ref, krige, idw)), 
         length))
      )
    )
  )

reference <- reference[sample(1:r),]
aggregated_ref <- aggregated_ref[sample(1:r),]
krige <- krige[sample(1:r),]
idw <- idw[sample(1:r),]

compare <- rbind(reference, aggregated_ref, krige, idw)
compare <- compare[order(compare$elevation),]

kruskal.test(compare$elevation, g = compare$Type)
boxplot(elevation~Type, data=compare, notch=TRUE,
  col=(c("cadetblue","brown3", "darkorchid3", "olivedrab2")),
  main="Distribution of Elevation Values from Raster", xlab="Raster", ylab= "Elevation (m)")

rm(reference, aggregated_ref, krige, idw, compare, elevation_raster, elevation_raster_coarse, pred.reg, prediction_IDW)

#' 
#' We can actually check whether the values are similar to the Original raster, and they are. 
#' 
#' Kruskal Wallis null hypothesis: the means of each group is the same, we cannot reject the null hypothesis. We see that Krige may 'smooth' the results at either end more than IDW, this is a known parameter of it. 
#' 
#' # Works Cited
#' 
#' https://chicagoreader.com/news-politics/cityscape-how-the-lakefront-was-won/ 
#' 
#' Lee, S., and Tribune Staff Writer https://www.chicagotribune.com/news/ct-xpm-1995-10-19-9510190079-story.html 02.04.2022. Chicago Tribune. 10.19.1995
#' 
#' https://www.fotp.org/lakefront-protection-and-public-trust.html 02.04.2022
#' 
#' https://desktop.arcgis.com/en/arcmap/10.3/tools/3d-analyst-toolbox/how-kriging-works.html Accessed 1.17.2021
#' 
#' Tobler W., (1970) "A computer movie simulating urban growth in the Detroit region". Economic Geography, 46(Supplement): 234–240.
