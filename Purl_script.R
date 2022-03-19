setwd('C:/R_datsci_2022/Spatial_lecture_developement')
getwd()

knitr::purl('SDS_Laboratory.Rmd', documentation = 2)
knitr::purl('SDS_Day_1.Rmd', documentation = 2)
knitr::purl('SDS_Day_2.Rmd', documentation = 2)

knitr::purl('Raster_interpolation.Rmd', documentation = 2)
knitr::purl('Cartography.Rmd', documentation = 2)
knitr::purl('ObjectOrientedProgramming.Rmd', documentation = 2)
