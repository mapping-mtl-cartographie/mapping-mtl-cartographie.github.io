# Air BnBs Per QC Provincial Riding Map

# Packages
library(geojsonio) # convert to/from geojson formats
library(here) # helps reference files easier
library(hrbrthemes) # typographic theme elements
library(htmlwidgets) # for saving the map as HTML
library(htmltools) # for saving the map as HTML
library(leaflet) # for building the map itself!
library(leaflet.extras)
library(leaflet.extras2)
library(rmapzen) # vector tiles for maps
library(sf) # spatial data and visualization
library(tidyverse) # Using ggplot, tidyr, purr, dplyr

# Setting working directory--set it to the folder where you'll have your script,
# files, data, etc
setwd('YOUR/WORKING/DIRECTORY')

# Reading in downloaded shapefile as sf file
# Data is from: https://www.dgeq.org/en/data.html
# I downloaded it so I could look through it, but downloading directly works.
# Using read_sf to "read-in" the file:
qc_election_map_2022_sf <- read_sf('FILE/PATH/shapefile.shp')

# Checking class to make sure it's good
# Should have "sf", "tbl_df", "tbl", "data.frame"
class(qc_election_map_2022_sf)

# Loading in Air BnB Data
listings_mtl_sept_2022 <- read.csv('FILE/PATH/file.csv')

# Check structure of csv file, with names of columns
str(listings_mtl_sept_2022)

# Identify x, y location columns to make it mappable!
# Columns in this case are "latitude" and "longitude" 
# Figure out utm data to convert to crs object
# Looked up on https://hub.arcgis.com/datasets/esri::world-utm-grid/explore and
# https://epsg.io/map
# Montréal is 18N, or 4326 EPSG code
sept_2022_listings_sf <- listings_mtl_sept_2022 %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Transform QC election map into EPSG 
# (reproject so it's the same as the listings)
qc_election_map_2022_epsg <- st_transform(qc_election_map_2022_sf, 4326)

# Check the transformation was successful (Should show EPSG)
st_crs(qc_election_map_2022_epsg)

# Spatially join the 2022 Election Dataset with the 2022 Election data. 
# A join keeps the data that matches 
# (So, will keep all ridings with 2022 Election listings)
# I will drop all NA values, to make sure I'm not counting NA values as listings
# NA values are those ridings without listings
sept_2022_qc_elect_2022_join <- sf::st_join(qc_election_map_2022_epsg, sept_2022_listings_sf) %>%
  filter(!is.na(id))

# Creates a freq table (if desired)
sept_2022_qc_elect_2022_freq_table <- table(june_2022_qc_elect_2018_join$NM_CEP)

# Creates new sf object with counts in each riding. 
# Counting the instances of each NMCEP after the join, which will tally all 
# listings for that riding. 
sept_2022_qc_elect_2022_count <- sept_2022_qc_elect_2022_join %>%
  count(NM_CEP, name = "Total_AirBnB_Listings", sort = TRUE)

# Backing things up, just in case
sept_2022_qc_elect_2022_count_backup <- sept_2022_qc_elect_2022_count
sept_2022_qc_elect_2022_count <- sept_2022_qc_elect_2022_count_backup

# Adding in MNAs--reading in a CSV sheet of names that I created
# Created a spreadsheet, using this as a source: 
# https://www.assnat.qc.ca/en/deputes/index.html
# Reading it in this way because sometimes readr is not able to
# handle accents on read-in
qc_mnas_2022<- read.csv('FILE/PATH/file.csv')
view(qc_mnas_2022)

# Merging the MNA csv with the count list
# I made sure that there was one similar column in both, so I only use one 'by'
# call (both have 'NM_CEP')
sept_2022_qc_elect_2022_count_mna <- merge(sept_2022_qc_elect_2022_count, 
                                           qc_mnas_2022, 
                                           by='NM_CEP') 

# Because it was merged, it isn't an sf object anymore, so now telling R that
# there is a geometry column
sept_2022_qc_elect_2022_count_mna <- sept_2022_qc_elect_2022_count_mna %>%
  st_as_sf(coords = 'geometry', crs = 4326)


# Creating a riding map with Sept 2022 Inside Air BnB Data:
# Bins, Pal
bins_airbnb_per_riding <- c(1, 400, 800, 1200, 1600, 2000, Inf)
#(1, 375, 750, 1125, 1500, Inf)
pal_airbnb_per_riding <- colorBin("YlGnBu",
                                  sept_2022_qc_elect_2022_count_mna$Total_AirBnB_Listings, 
                                  bins = bins_airbnb_per_riding)
# Creating labels that will have accents
labels <- sprintf(
  "<strong>%s</strong><br/>Unités/Units: %i<br/>DAN/MNA: %s<br/>Parti/Party: %s",
  sept_2022_qc_elect_2022_count_mna$NM_CEP, 
  sept_2022_qc_elect_2022_count_mna$Total_AirBnB_Listings,
  sept_2022_qc_elect_2022_count_mna$Name,
  sept_2022_qc_elect_2022_count_mna$Parti) %>% 
  lapply(htmltools::HTML)

# Building the map:  
sept_2022_qc_elect_2022_map <- leaflet(sept_2022_qc_elect_2022_count_mna) %>%
  addTiles(attribution = 'AirBnB Data via Inside Air BnB') %>%
  addTiles(attribution = 'Carte de circonscription provinciale via ?lections Qu?bec') %>%
  addTiles(attribution = 'Map created by Rine Vieth, funded by QPIRG McGill') %>%
  addTiles(attribution = 'Note: InsideAirBnB data is anonymized 0-150 m') %>%
  addPolygons(data = sept_2022_qc_elect_2022_count_mna,
              color = "#000000",
              weight = 0.2,
              fillColor = ~pal_airbnb_per_riding(Total_AirBnB_Listings),
              smoothFactor = 0.2,
              fillOpacity = 0.5,
              popup = labels) %>%
  addLegend(position = "bottomright",
            pal = pal_airbnb_per_riding, 
            values =  sept_2022_qc_elect_2022_count_mna$Total_AirBnB_Listings,
            title = "Unités Air BnB par<br>circonscription provinciale<br>Air BnB Units per<br>Provincial Election District<br>09/22",
            opacity = 1.0) %>%
  setView(lat = 45.5555598, lng = -73.5952134, zoom = 11)

# Showing (printing) the map
sept_2022_qc_elect_2022_map

# Save widget so can upload HTML
saveWidget(sept_2022_qc_elect_2022_map, 
           file.path('PATH/TO/FOLDER','airbnb_mtl_elect_map.html'))
