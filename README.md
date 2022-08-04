---
title: "Hemp mapping and plotting"
author: "Michele Wiseman"
date: '2022-08-04'
---

# Code for figures and maps from our 2021 Hemp Powdery Milew survey

Learning R can be hard. I'm uploading this code in hopes of helping you learn; I'm no expert and I'm not a very efficient coder, but I hope you will find it helpful regardless. 

## Load necessary libraries
If you haven't installed any of these libraries, remember you can typically install them through the `install.packages('packagenamehere')` command. 

```r setup
library(tidyverse)          # manipulating and plotting data
library(usmap)              # preloaded us maps
library(scales)             # allows scale customization
library(ggnewscale)         # allows for two fill scales
library(readr)              # necessary for loading in csv data
library(lubridate)          # fast changing of dates
library(devtools)           # for loading packages not in CRAN.
library(readxl)             # for reading .xlsx files
library(sp)                 # special tools for spatial data
library(maps)               # loadable maps
library(gridExtra)          # making multi-plot figures
library(basemaps)           # loadable maps
library(sf)                 # special tools for spatial data
library(stars)              # special tools for spatiotemporal data
library(ggmap)              # mapping in a ggplot format
library(raster)             # reading spacial data
library(tidyUSDA)           # auto downloading data from USDA-SASS
library(rgdal)              # bindings to the 'Geospatial' Data Abstraction Library 
library(USAboundaries)      # loadable maps
```

## Load data
*You will have to change your path*

```r load in data

#For disease incidence and taxa layers
Pamb_Pmac_combined_PCR_Positives_Summary_2021 <- read_excel("~/2021-Pamb_Pmac_combined_PCR_Positives_Summary.xlsx")
x2022_OR_Raw_Data <- read_excel("~/2022 Hemp Disease Survey Field Info and Data.xlsx", sheet="OR Raw Data")
x2022_WA_Raw_Data <- read_excel("~/2022 Hemp Disease Survey Field Info and Data.xlsx", sheet="WA Raw Data")
x2021_datasheet <- read_excel("~/2021 Hemp PM Survey Data.xlsx")

#For sampling sites layer
x2022_Hemp_Disease_Survey_Field_Info_and_Data <- read_excel("~/2022 Hemp Disease Survey Field Info and Data.xlsx")
Hemp_fields_surveyed_2021_22 <- read_excel("~/2021-22_Hemp_fields_surveyed.xlsx", sheet= "Sampling_Sites")

#For acreage layer
hop_acreage <- read_csv("~/SASS_hop_data.csv")
Hop_acreage_2017 <- read_excel("~/2021-22_Hemp_fields_surveyed.xlsx", sheet= "Hemp_Hop_ROUGH_Acreage_Data")
OR_growers <- read_excel("~/2022_OR_Growers.xlsx")
WA_growers <- read_excel("~/2022_WA_growers.xlsx")
x2021_hemp_national_acreage <- read_excel("~/2021-Hemp_National_Acreage_Data.xlsx")
```
