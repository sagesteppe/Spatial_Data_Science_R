# Spatial_Data_Science_R
Materials created for 'R for Data Science' (Plant Biology and Conservation 470, Earth and Planetary Science, and Environmental Science) Winter 2022 at Northwestern University. If you need material listed in the .gitignore ( such as pictures if you would like to reproduce and add onto these materials) please reach out and I can send these over. If you need to use these for class Lectures are intended for 90 minute lecture periods, and the lab is for a 2.5 hour activity- but depending on questions will run from 1 1/2 hours to 2 1/4 hours.

See the other original README with the text file extension for more day to day operations with this.

## Notes 

The contents of this folder were prepared for Northwestern Universities 'R Data Science' PBC 470 and ENV 390, by (sage/)Steppe in 2022. 
All unattributed or un-referenced images in the presentations were collected by steppe, same with field data. 
Some of the content was inspired by the work of Yingying Xie in previous iterations of this course. Ikea exercise was developed by YyX.

These materials were developed to give peoples with beginner to beginner-intermediate skills in R a background on Spatial Data Science.

In order to use these materials please do the following:

1) Move the folder containing this file into your current directory you use for this class. 
2) launch an R script and determine which directory you are located in. If necessary change your directory to your course folder.
e.g. setwd('C:\R_datsci_2022')

Note there are two versions of these files. Please refer to the name of this directory to determine which you are using. 
In the very redundant inventory below files in the ess_SDS folder but not lite_SDS are prefaced with a *asterisk. 

3) If somehow you actually opened this file please install the following packages for your lab:
install.packages("sf", "sp", "tmap", "leaflet", "ggmap", "GGally")
 
optional packages for using parallel processing at a step in the lab activity
install.packages('snow','parallel')

If you want to follow along with the class script you will need the following additional programs:
install.packages('spData','fields','spdep','spatstat','gstat')

3a) The great Raster/terra problem of three weeks in early 2022, will be a hot topic that R historians will talk about
for years to come. If you are unable to get terra or raster to work please do the following.

a) Save all of your R scripts which you have open in Rstudio. 
b) close *every* R script in Rstudio
c) clear your global environment using the Broom or programmatically via: rm(list=ls())
d) close Rstudio, if prompted to 'Save workspace image to ~/.RData "Don't Save"
e) re-open R studio, and create a new empty script 'File > New File > R script'
run the following:
> unlink(".Rdata")
> rm(list=ls()) # even if you did it at step c
> sessionInfo()

Now you should only have 'attached base packages:' and NOT also 'other attached packages'
If you have already intalled these packages and errors have occurred please runthe following:
remove.packages('terra')
remove.packages('raster')

> install.packages('terra', repos='https://rspatial.r-universe.dev')
> install.packages("raster")

======

PDF files:
SDS_Day_1.pdf - lecture notes used for slides
SDS_Day_2.pdf - lecture notes used for slides
spatial_case_study - your homework assignment
*OOP_in_r.pdf - outtake notes on object orientated programming
*Cartography.pdf - outtake notes on more advanced cartography in r
*Raster_Interpolation.pdf - outtake notes on raster aggregation and interpolation

Markdown files:
SDS_Day_1.rmd - lecture notes used for slides
SDS_Day_1_slides.rmd - code to render slide show
SDS_Day_2.rmd - lecture notes used for slides
SDS_Day_2_slides.rmd - code to render slide show
SDS_Laboratory.rmd - notes used for the lab
*OOP_in_r.rmd - outtake notes on object orientated programming
*Cartography.rmd - outtake notes on more advanced cartography in r
*Raster_Interpolation.rmd - outtake notes on raster aggregation and interpolation

R Scripts:
SDS_laboratory.R - script for lab
SDS_Day_1.R - limited interactivity script for theory day
SDS_Day_2.R - interactive script for second day
*Cartography.R - interactive script for these materials
*Raster_interp.R - interactive script for these materials

Sub-folders:
spatial_lab_data - all data for the lab exercise
spatial_lecture_data - all data for interactive activities
spatial_homework_data - all data for the homework activity