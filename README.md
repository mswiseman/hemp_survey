---
title: "Hemp mapping and plotting"
author: "Michele Wiseman"
date: '2022-08-04'
---

# Code for figures and maps from our 2021 Hemp Powdery Milew Survey

Learning R can be hard. I'm uploading this code in hopes of helping you learn; I'm no expert and I'm not a very efficient coder, but I hope you will find it helpful regardless. 

## Load necessary libraries
If you haven't installed any of these libraries, remember you can typically install them through the `install.packages('packagenamehere')` command. 

* For notes on theme components, see [here](https://ggplot2.tidyverse.org/reference/theme.html).
* Map tips [here](https://cran.r-project.org/web/packages/osmplotr/vignettes/basic-maps.html)
* Custom bounding boxes [here](https://bigquerygeoviz.appspot.com/)

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
library(MASS)

```

## Load data
*You will have to change your path*

```r
# For disease incidence and taxa layers
Pamb_Pmac_combined_PCR_Positives_Summary_2021 <- read_excel("~2021-Pamb_Pmac_combined_PCR_Positives_Summary.xlsx")
x2022_OR_Raw_Data <- read_excel("~2022_Hemp_Disease_Survey_Field_Info.xlsx", sheet="OR Raw Data")
x2022_WA_Raw_Data <- read_excel("~2022_Hemp_Disease_Survey_Field_Info.xlsx", sheet="WA Raw Data")
x2021_datasheet <- read_excel("~2021_Hemp_PM_Survey_Data.xlsx")

# For sampling sites layer
x2022_Hemp_Disease_Survey_Field_Info_and_Data <- read_excel("~2022_Hemp_Disease_Survey_Field_Info.xlsx")
Hemp_fields_surveyed_2021_22 <- read_excel("~2021-22_Hemp_fields_surveyed.xlsx", sheet= "Sampling_Sites")

# For acreage layer
Hop_acreage <- read_csv("~2017-Slightly-Modified-SASS_Hop_Data.csv")
Hop_acreage_2017 <- read_excel("~2021-22_Hemp_fields_surveyed.xlsx", sheet= "Hemp_Hop_ROUGH_Acreage_Data")
OR_growers <- read_excel("~2022_OR_Growers.xlsx")
WA_growers <- read_excel("~2022_WA_growers.xlsx")
x2021_hemp_national_acreage <- read_excel("~2021-Hemp_National_Acreage_Data.xlsx")


# Slimming columns to save memory

x2021_datasheet_abb <- x2021_datasheet[,c(1:3,6:7,9:13)]


```
## Looking at hemp production by state
Download the [2021 hemp survey results](https://usda.library.cornell.edu/concern/publications/gf06h2430) and remove the spaces and special symbols ($ / , etc.) in the headers to make it more R friendly. You can do this in R, but I find ctrl+f and then replace to be super fast in excel. I replaced all spaces with underscores and wrote out symbols when present. 

```{r hemp usda}
hemp_open_production <- x2021_hemp_national_acreage %>%
  filter(Data_Item == "IN_THE_OPEN_UTILIZED_FLORAL_PRODUCTION_MEASURED_IN_DOLLARS")

# You might toy around with different breaks... I wasn't seeing enough contrast, so I increased the contrast with these breaks. 
my_breaks = c(0, 5000000, 10000000, 50000000, 200000000)

# Use USmap to quickly plot out the state values
plot_usmap(data = hemp_open_production,
           values = "Value",      # Value in this case is in dollars
           labels = TRUE,         # text labels
           label_color = "white", 
           face = "bold", 
           alpha = 0.8) +         # alpha = transparancy
  ggtitle("2021 Total Production Value of Open Grown Hemp Flower By State") +
  scale_fill_gradient2("Production Value \n (Dollars)",
                       low = "#440154",                 # purple
                       mid = "#279a86",                 # blue-green
                       high = "#fce724",                # yellow
                       na.value = "grey80",
                       midpoint = 100000000) +
  theme(
    legend.background = element_blank(),
    legend.position = c(0.9,0))

```

<img align="center" width="600" alt="Plot2" src="https://github.com/mswiseman/hemp_survey/blob/main/images/plot2.png?raw=true" class="padding"/>
## Plotting county data

```r county data
# set the state and county names of interest
state_names <- c("Oregon", "Washington")

# get COUNTY data for a given state
counties_spec <- us_counties(resolution = "high", states = state_names)

# get STATE data
OR_WA_2<-us_states(resolution = "high", states = state_names) %>%
  st_transform(crs = 4326)

# get range of lat/longs from counties for mapping and river function
mapRange1 <- c(range(st_coordinates(counties_spec)[,1]),range(st_coordinates(counties_spec)[,2]))

# check quickly
ggplot() + 
  geom_sf(data=OR_WA_2, color = "gray30", lwd=2, fill=NA) +
  geom_sf(data=counties_spec, fill = NA, show.legend = F, color="gray50", lwd=0.4) +
  theme_bw()
```
<img align="center" width="600" alt="Plot1" src="https://github.com/mswiseman/hemp_survey/blob/main/images/plot1.png?raw=true" class="padding"/>

## Using TidyUSDA
TidyUSDA is a pretty nifty library that enables quick downloading of USDA-SASS data. To see what things you can load in, you'll have to check out the USDA-NASS [quick stats website](https://quickstats.nass.usda.gov/). Unfortunately if you're like me and **running an M1 mac** there are mapping features that aren't yet supported in TidyUSDA, so keep that in mind. To get a USDA-NASS API key, fill out the quick form [here](https://quickstats.nass.usda.gov/api). 

More info on TidyUSDA [here](https://github.com/bradlindblad/tidyUSDA). 

```{r TidyUSDA data}
# uncomment below to get a quick tutorial of the library.
#vignette("using_tidyusda")  <- tutorial 

# enter your API key in the quotation marks below. 
key <- ''  

# to see all available categories
tidyUSDA::allCategory %>% head()

# lets look at the last available county data for hops (2017)
hop_county_harvest <- tidyUSDA::getQuickstat(
  sector= NULL,
  group= NULL,
  commodity= 'HOPS',
  category= NULL,
  domain='TOTAL',
  county= NULL,
  key = key,
  program = NULL,
  data_item = "HOPS - ACRES HARVESTED",
  geographic_level = 'COUNTY',
  year = "2017",
  state = c('WASHINGTON','OREGON'),
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = FALSE)

# drop any counties that have null values
hop_county_harvest <- hop_county_harvest %>%
  drop_na(Value)

# need to rename a column for later
q <- colnames(hop_county_harvest)
q[1] <- "fips"
colnames(hop_county_harvest) <- q
hop_county_harvest$fips <- as.integer(hop_county_harvest$fips)

  ```
  
 ## Process address data
 For the privacy of growers, I won't link the excel spreadsheet with addresses, though it is available to the public. 
 
 When using address data, you have to convert it to lat/long before mapping it with a lot of the R mapping packages. In the box below I'm feeding in address data and the Google API is returning lat, long data in a new column. For a google API key, sign up [here](https://developers.google.com/maps/documentation/javascript/get-api-key). 
 
 ```{r process hemp addresses}
 
# insert your google api key
register_google(key = '')     # you have to sign up for this. 

# Function to add geo data will append your original df
geocoded <- data.frame(stringsAsFactors = FALSE) 
for(i in 1:nrow(OR_growers))
{
  result <- geocode(OR_growers$`Address`[i], output = "latlona", source = "goog")
  OR_growers$lon[i] <- as.numeric(result[1])
  OR_growers$lat[i] <- as.numeric(result[2])
  OR_growers$geoAddress[i] <- as.character(result[3])
}

# Now add geo data for Washington growers

geocoded <- data.frame(stringsAsFactors = FALSE) 
for(i in 1:nrow(WA_growers))
{
  result <- geocode(WA_growers$`Address`[i], output = "latlona", source = "goog")
  WA_growers$lon[i] <- as.numeric(result[1])
  WA_growers$lat[i] <- as.numeric(result[2])
  WA_growers$geoAddress[i] <- as.character(result[3])
}

# Combine the relevent columns into new dataframe
OR_grow_abb <- OR_growers[,20:22]
WA_grow_abb  <- WA_growers[,38:40]

# bind by rows to make one df
PNW_growers <- rbind(OR_grow_abb, WA_grow_abb)

```
## Base map
Maps are built in layers with the base layer typically being land/water. 

```r, creating base map
#use the map packages to make a dataframe of the polygons in map_data("state")
states <- map_data("state")


#Now we want to filter out a polygon from our states dataframe for Oregon
oregon_wash <- states %>%
  filter(region %in% c("oregon","washington"))

gcounty <- map_data("county") %>%
  filter(region %in% c("oregon", "washington"))


fipstab <-
    transmute(maps::county.fips, fips, county = sub(":.*", "", polyname)) %>%
    unique() %>%
    separate(county, c("region", "subregion"), sep = ",")

gcounty <- left_join(gcounty, fipstab, c("region", "subregion"))

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))

#bounding box for the area
box <- make_bbox(long, lat, data = oregon_wash)

```

## Lets try some stamen maps

```r, exploring maps with for loop

# custom colors and shapes to be used in the maps and figures. 
col_year <- c("2021" = "#7d3ec1", "2022"= "#3EC17D")
col_month <- c("July"= "#bf642f", "August"= "#242c31","September"="#938a5d", "October"="#474c33") 
col_species<- c("G. ambrosiae" = "#E0614E", "P. macularis"="#000000")
shape_species <- c("G. ambrosiae" = 21, "P. macularis" = 24)

#since year is being used in a discrete way, it's easier to deal with as a character object
Hemp_fields_surveyed_2021_22$Year <- as.character(Hemp_fields_surveyed_2021_22$Year)

#lets see how all the map types look with a little for loop.

#################    for loop set up     #################

maptype <- c("terrain-labels", "terrain-lines", "toner", "toner-2011", "toner-background", "toner-hybrid", "toner-lines","toner-lite", "watercolor")
mylist <- vector("list", length(maptype))
num <- 0

#for whatever reason I needed to uninstall and reinstall ggmap here... if you get an 'Error in gzfile(file, "rb") : cannot open the connection' then you may need to do the same. Uncomment the next three lines if you need to do this. 

#remove.packages("ggmap")
#install.packages("ggmap")
#library(ggmap)

# Previewing different basemaps with our data. 

for(i in maptype ){
  num <- num+1
  map <- get_stamenmap(box, zoom = 7, maptype = i)
  p <- ggmap(map) +
   xlab("")+
   ylab("")+
   geom_jitter() +
    geom_point(data = Hemp_fields_surveyed_2021_22,
             aes(x=Long, y=Lat, color=Year)) +
    scale_color_manual(
    values = col_year,
    breaks = c("2021", "2022")) +
    ggtitle(i) + 
    theme(
      plot.title = element_text(color = "orange"), 
      plot.background = element_rect(color = "black"),
      panel.border = element_rect(color = "#7d3ec1", fill=NA, size=2),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.key = element_rect(fill="white"),
      legend.title = element_blank(),
    )
  mylist[[num]] <- p
}
 
n <- length(mylist)
nCol <- floor(sqrt(n))
grid <-do.call("grid.arrange", c(mylist, ncol=nCol))

ggsave("plot3.png", width = 8, height = 11, units = "in", dpi = 300)

#################    end for loop     #################

```
<img align="center" width="600" alt="Plot3" src="https://github.com/mswiseman/hemp_survey/blob/main/images/plot3.png?raw=true" class="padding"/>
The stamen maps are really nice, but for whatever reason you can't overlay geom_sf onto gmap objects that easily (they don't line up because they're different coordinate types). I found a hack online to fix this, hence the next set of code below.

```r playing with the best looking map
gmap <- get_stamenmap(box, zoom = 7, maptype = "toner-background")

counties_spec_3857 <- st_transform(counties_spec, 3857) 

ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}
counties_spec <- ggmap_bbox(gmap)


```
## Converting ojbects to geom_sf objects

```r geom sf conversion

# make into geom_sf objects
(Fields_surveyed <- st_as_sf(Hemp_fields_surveyed_2021_22, coords = c("Long", "Lat"), 
    crs = 4326, agr = "constant"))

# hemp grower locations
(PNW_grower_sites <- st_as_sf(PNW_growers, coords = c("lon", "lat"), 
    crs = 4326, agr = "constant"))
```

## Okay, lets do some mapping of data. 

```r, using gmap base and county polygons

#manually define colors for year
col_year <- c("2021" = "#7d3ec1", "2022"= "#3EC17D") #to match the poster


ggmap(counties_spec) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = counties_spec_3857, inherit.aes = FALSE, fill = NA, color = "gray90") +
  geom_sf(data = county_and_yield_data,   
          aes(fill = as.numeric(Value),
       geometry = geometry), inherit.aes = FALSE) +
  #scale_fill_viridis_c()+
  scale_fill_gradient("Acres of Hops Harvested", 
                      low = "#F3E6DA",
                      high = "#C17D3E",
                      space = "Lab",
                      breaks = my_breaks,
                      labels = my_breaks,
                      na.value = "grey90",
                      trans="log",
                      guide = "colourbar",
                      aesthetics = "fill")  +
  new_scale_fill() +
  geom_sf(data = PNW_grower_sites,                       # cannabis growers
          aes(geometry = geometry),
          color = "black",
          fill = "gray",
          size = 3,
          shape = 21,
          inherit.aes = FALSE) +
  geom_sf(data = Fields_surveyed,                          # sampling locations
          aes(geometry = geometry,
              color = Year), #fill = Year),
          fill = "white",
          #colour="gray30",
          size = 3,
          shape = 21, 
          alpha = 0.9,
          inherit.aes = FALSE) +
  scale_color_manual(values = col_year,
    breaks = c("2021", "2022")) +
  theme(legend.key=element_blank(),         # removes gray box behind symbols
        legend.position = "right",
        axis.title = element_blank(),       # removes "lat" and "long" labels
        axis.ticks = element_blank(),
        axis.text = element_blank())        # removes "lat" and "long" values


ggsave("plot4.png", width = 8, height = 11, units = "in", dpi = 300)
```

<img align="center" width="600" alt="Plot4" src="https://github.com/mswiseman/hemp_survey/blob/main/images/plot4.png?raw=true" class="padding"/>

```r converting df to geom_sf

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

# switch to title case
states$ID <- toTitleCase(states$ID)

or_wa <- states %>%
  filter(ID %in% c("Oregon","Washington"))
  
or_wa <- cbind(or_wa, st_coordinates(st_centroid(or_wa)))
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("washington|oregon", counties$ID))

```
## We can also make maps by just using geometric data

```r, using just shape data
ggplot() +
geom_polygon(data = oregon_wash,
             aes(x = long,
                 y = lat,
                 group = group),
             fill = "white",
             color = "darkgray") + 
  coord_quickmap() +
  geom_point(data = Hemp_fields_surveyed_2021_22,
             aes(x = Long,
                 y = Lat,
                 fill = Year),
             shape = 21,
             color = "black")+
  scale_fill_manual(
    values = col_year) +
    theme(
      axis.ticks = element_blank(),                 #remove tick marks
      axis.text = element_blank(),                  #remove lat/long axes labels
      axis.title = element_blank(),                 #remove axes title
      legend.key = element_rect(fill="white"),      #fill legend white 
      legend.title = element_blank(),               #remove legend title 
      panel.background = element_rect(fill="black", color="black"),
      panel.grid = element_blank()
    )
    
 ```
<img align="center" width="600" alt="Plot5" src="https://github.com/mswiseman/hemp_survey/blob/main/images/plot5.png?raw=true" class="padding"/>    

```r, density

# get density polygons
dens <- contourLines(
    kde2d(Species_comp_and_sampling_time_drop_June_jittered$Long, Species_comp_and_sampling_time_drop_June_jittered$Lat, 
          lims=c(expand_range(range(Species_comp_and_sampling_time_drop_June_jittered$Long), add=0.5),
                 expand_range(range(Species_comp_and_sampling_time_drop_June_jittered$Lat), add=0.5))))

# this will be the color aesthetic mapping
Species_comp_and_sampling_time_drop_June_jittered$Density <- 0

# density levels go from lowest to highest, so iterate over the
# list of polygons (which are in level order), figure out which
# points are in that polygon and assign the level to them

for (i in 1:length(dens)) {
  tmp <- point.in.polygon(Species_comp_and_sampling_time_drop_June_jittered$Long, Species_comp_and_sampling_time_drop_June_jittered$Lat, dens[[i]]$x, dens[[i]]$y)
  Species_comp_and_sampling_time_drop_June_jittered$Density[which(tmp==1)] <- dens[[i]]$level
}
```
## Looking at species ID results

```r
# subsampling necessary colnames to free memory (wider df = more memory needs)
Positive_samples_abb <- Pamb_Pmac_combined_PCR_Positives_Summary_2021[,c(3:4,10:27)]   
Species_comp_and_sampling_time <-  Positive_samples_abb[,(1:10)] %>%
  distinct()            # I want to make sure there are no duplicates

# just 2021 for now
Fields_surveyed_2021 <- Fields_surveyed %>%              
  filter(Year == "2021")

# make into geom_sf objects
(Species_comp_and_sampling_time_drop_June_jittered2 <- st_as_sf(Species_comp_and_sampling_time_drop_June_jittered, coords = c("Long", "Lat"), 
    crs = 4326, agr = "constant"))
    
 ```
 
 ## Incidence and severity
 
 ```r creating disease incidence and severity df
 
# subsamples to save on memory
x2021_datasheet_abb <- x2021_datasheet[,c(1:3,6:7,9:13)]

# make columns for month and year so I can facet by month (more relevant for hop growers)
x2021_datasheet_abb2 <- x2021_datasheet_abb %>%
  separate("Sampling_Date", sep="-", into = c("Year", "Month", "Day")) %>%
  select(-Day) %>%
  mutate(Perc_mildew = (PM_per_ten_leaves/10)*100) %>%
  drop_na(Perc_mildew)

Incidence_and_severity <-x2021_datasheet_abb2 %>%
  group_by(Year, Month, Lat, Long, Field_Name) %>%
  summarize(
    Average_severity = mean(Perc_mildew),
    N_all_plants = length(Perc_mildew == 0),
    N_mildew_plants = sum(Perc_mildew > 0),
    Disease_incidence = (N_mildew_plants/N_all_plants)*100)

# make into geom_sf objects
(Incidence_and_severity2 <- st_as_sf(Incidence_and_severity, coords = c("Long", "Lat"), crs = 4326, agr = "constant")) 
 ```
 
 
 ```r prepping another base map

PNW_growers2 <- PNW_growers[-1,]

df[-c(row_index_1, row_index_2),]
OR_growers2 <- OR_grow_abb
colnames(PNW_growers2) <- c("Long","Lat","Street")
colnames(OR_growers2) <- c("Long","Lat","Street")

# Oddly enough there was a nevada grower... so I had to filter it out
PNW_growers2 <- PNW_growers2 %>%
  filter(!Lat < 42)
  
```

## Visualizing progression of disease
### Current map

```{r visualizing progression of disease}
# reload jittered data
Species_comp_and_sampling_time_drop_June_jittered <- read_xlsx("Desktop/Hemp PM Work/Species_comp_and_sampling_time_drop_June.xlsx")

col_sp2<- c("G. ambrosiae" = "#7d3ec1", "P. macularis"= "#3EC17D") #to match the poster
col_V6 <-  c("." = "#7d3ec1",
             "Non-V6"= "#3EC17D",
             "V6" = "#fce724",
             "Both" = "#36668c")

jitter <- position_jitter(width = 0.05, height = 0.05)
jitter2 <- position_jitter(width = 0.1, height = 0.1)
shape_species2 <- c("G. ambrosiae" = 21, "P. macularis" = 24)
shape_V6 <- c("V6" = 21, "Non-V6" = 22, "Both" = 23, "." = 24)

# makes sure your months are in the right order
Species_comp_and_sampling_time_drop_June_jittered$Month_collected <- factor(Species_comp_and_sampling_time_drop_June_jittered$Month_collected, levels = c("July", "August", "September", "October"))

qmplot(
  Long,
  Lat,
  data = Species_comp_and_sampling_time_drop_June_jittered,
  geom = "blank",
  maptype = "toner", 
  legend = "bottomright",
  zoom = 7 ) +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon",
                  alpha = .3,
                  data = Species_comp_and_sampling_time_drop_June_jittered, contour_var = "density") +
  scale_fill_viridis_c() +
  facet_grid(~ Month_collected) +                                                # facet by month
  guides(fill = guide_legend(title = "Level")) +
  new_scale_fill() +                                                             # allows two fill scales
  geom_point(data = Species_comp_and_sampling_time_drop_June_jittered ,
             aes(
             x = Long,
             y = Lat,
             fill = Target,
             shape = Target),
             size = 2,
             alpha = 0.8,
             color = "black",
             position = jitter) +
  scale_viridis_d() +
  scale_shape_manual(values = shape_species2) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  theme(panel.spacing  = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1, fill = "black"), 
        legend.background = element_rect(fill=alpha('black')),
        legend.key=element_blank(),
        legend.box = "horizontal",
        legend.position = "bottom",
        legend.text.align = 1,
        legend.text = element_text(size=24, color = "white", face="bold"),
        legend.title = element_text(size=28, face = "bold", color = "white"),
        strip.text = element_text(face="bold", size=32, color = "white"),
        legend.margin = margin(c(2,2,2,2)))

#need to specify the scale so you don't cut the title off
ggsave("plot6.png",
        width = 11,
       height = 8,
       units = "in",
       dpi = 300)

```
## virulence factors map

```{r highlighting virulence factors}

qmplot(
  Long,
  Lat,
  data = PNW_growers2,
  geom = "blank",
  maptype = "toner", 
  legend = "bottomright",
  zoom = 7 ) +
  #new_scale_fill() +
  geom_point(data= Species_comp_and_sampling_time_drop_June_jittered,
             aes(x= Long, y= Lat, shape = Target, fill = V6), 
             color = "black",
             size = 2,
             alpha = 0.7,
             position = jitter2) +
  scale_fill_manual(values = col_V6) +
  scale_shape_manual(values = shape_species2) + 
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  theme(panel.spacing  = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1, fill = "black"), 
        legend.background = element_rect(fill=alpha('black')),
        legend.key=element_blank(),
        legend.box = "horizontal",
        legend.position = "bottom",
        legend.text.align = 1,
        legend.text = element_text(size=24, color = "white", face="bold"),
        legend.title = element_text(size=28, face = "bold", color = "white"),
        strip.text = element_text(face="bold", size=32, color = "white"),
        legend.margin = margin(c(2,2,2,2)))

#need to specify the scale so you don't cut the title off
ggsave("plot9.png",
        width = 11,
       height = 8,
       units = "in",
       dpi = 300)
```
