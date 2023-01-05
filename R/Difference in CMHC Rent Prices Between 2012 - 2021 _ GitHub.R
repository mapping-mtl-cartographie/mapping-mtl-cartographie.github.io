# Map of Difference in CMHC Rent Prices Between 2012 - 2021

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

# Reading in the shapefile of the CMHC zones from where it's saved on my computer
# Make sure that *ALL* of the unzipped files are in your working directory
# I got the file by emailing the CMHC; you may be able to find your region's
# data through your local government OR through the CMHC package:
# https://cran.r-project.org/web/packages/cmhc/index.html
cmhc_mtl_zone_sf <- st_read('PATH/TO/FILE.shp')
cmhc_mtl_zone_sf

# Transform so working with same projection
cmhc_mtl_zone_sf_epsg <- st_transform(cmhc_mtl_zone_sf, 4326)

# Check the transformation was successful
st_crs(cmhc_mtl_zone_sf_epsg)

## Look at the zone names. Do these match to the names with the shapefiles? 
# I will be matching with ZONENAME_F (but names are also available in English). 
# I renamed a column name in the spreadsheet (outside of R) during data cleaning 
# so that they would join easier, at a later stage, but you can also rename
# from within R.

# Next, read in the spreadsheet with cost by neighborhoods.
# I got this data through:
# https://www.cmhc-schl.gc.ca/en/professionals/housing-markets-data-and-research/housing-data/data-tables
# But, also available through the cmhc R package.
cmhc_mtl_rent_size_csv  <- read_csv('PATH/TO/FILE.csv')

# Checking to see how things look:
glimpse(cmhc_mtl_rent_size_csv)

# Now...join the just-imported data with the CMHC shapefiles, to get geometry 
# with the CMHC data data.
cmhc_mtl_rent_size_zone <- merge(cmhc_mtl_rent_size_csv, 
                                 cmhc_mtl_zone_sf_epsg, 
                                 by = 'ZONENAME_F')

# Again, checking to see how things look:
glimpse(cmhc_mtl_rent_size_zone)

# In looking at the joined data (cmhc_mtl_rent_size_zone), I noticed that 
# some were duplicated. (It's always good to check how things are looking!) 
# So, I'm going to use dyplr's distinct() function to remove rows 
# that are exactly the same.
# Because Object Ids are different, I am going to match name and year columns. 
# .keep_all = TRUE means we want to keep the rest of the variables
# T his is important, because we want to keep them!
cmhc_mtl_rent_size_zone <- distinct(cmhc_mtl_rent_size_zone, 
                                    ZONENAME_F, 
                                    Year, 
                                    .keep_all = TRUE)

# Checking to make sure things look good (Spoiler: they do!)
glimpse(cmhc_mtl_rent_size_zone)
head(cmhc_mtl_rent_size_zone)

# I am going to organize things by year, instead of area.
# This will make things easier during the Shiny App building.
cmhc_mtl_rent_size_zone <- cmhc_mtl_rent_size_zone %>% 
  arrange(Year)

# I've been treating this as a data frame, but it needs to be an sf object
# (This makes it so we can map it!)
# Changing to an sf object, 4326 is the code for Montréal
cmhc_mtl_rent_size_zone <- st_as_sf(cmhc_mtl_rent_size_zone, 
                                    crs = 4326)

# So...what if we want to create a Leaflet map showing the difference 
# between rent in 2012 and 2021?
# This will have:
#     1. Difference between 2012-2021
#     2. Different kinds of housing types (for ex, apartment sizes)

# First: Create 2012 and 2021 values 
# (Doing this step-by step, there are ways to make it faster!)
cmhc_mtl_rent_2012 <- cmhc_mtl_rent_size_zone %>%
  filter(Year == '2012')

cmhc_mtl_rent_2021 <- cmhc_mtl_rent_size_zone %>%
  filter(Year == '2021')

# Setting up a "difference" sf object 
cmhc_mtl_rent_difference <- cmhc_mtl_rent_size_zone %>%
  # Keeping the ZONENAME_FR and geometry columns
  subset(select = c(ZONENAME_F, geometry)) %>%
  unique()

# Subtracting, and adding columns to the difference sheet.
# I've checked to make sure the orders are the same--which they should be,
# because they have not been reordered since subsetting from the original 
# data! There are more efficient ways to do this, but I'm doing it step-by step.
cmhc_mtl_rent_difference$Studio <- cmhc_mtl_rent_2021$Studio - cmhc_mtl_rent_2012$Studio
cmhc_mtl_rent_difference$One_BR <- cmhc_mtl_rent_2021$One_BR - cmhc_mtl_rent_2012$One_BR
cmhc_mtl_rent_difference$Two_BR <- cmhc_mtl_rent_2021$Two_BR - cmhc_mtl_rent_2012$Two_BR
cmhc_mtl_rent_difference$Three_plus <- cmhc_mtl_rent_2021$Three_plus - cmhc_mtl_rent_2012$Three_plus
cmhc_mtl_rent_difference$Total <- cmhc_mtl_rent_2021$Total - cmhc_mtl_rent_2012$Total

# Now: checking to see how it all looks. There are some NA values, which happens
# with subtraction involving NA values
# (Subtracting an NA or subtracting from an NA leads to an NA here)
glimpse(cmhc_mtl_rent_difference)

# In case you want to export this for backup or any other reason:
st_write(cmhc_mtl_rent_difference, 
         'name.geojson')

# If for whatever reason you want to export individual layers
st_write(cmhc_mtl_rent_diff_Studio, 
         'cmhc_mtl_rent_diff_Studio.geojson')

st_write(cmhc_mtl_rent_diff_One_BR, 
         'cmhc_mtl_rent_diff_One_BR.geojson')

st_write(cmhc_mtl_rent_diff_Two_BR, 
         'cmhc_mtl_rent_diff_Two_BR.geojson')

st_write(cmhc_mtl_rent_diff_Three_plus, 
         'cmhc_mtl_rent_diff_Three_plus.geojson')

st_write(cmhc_mtl_rent_diff_Total, 
         'cmhc_mtl_rent_diff_Total.geojson')



# Turning it into a map!
# Bins (for legend)--creating the scale manually
bins_cmhc_mtl_rent <- c(0, 100, 200, 300, 400, 500, 600, Inf)

# Because I'm doing this in Leaflet, it's a little cumbersome.
# I'm creating particular palettes for particular layers, to make the colors
# different for each type of housing
Studio_pal <- colorBin(
  palette = "Reds",
  domain = cmhc_mtl_rent_diff_Studio$Studio,
  bins = bins_cmhc_mtl_rent)

One_BR_pal <- colorBin(
  palette = "YlOrBr",
  domain = cmhc_mtl_rent_diff_One_BR$One_BR,
  bins = bins_cmhc_mtl_rent)

Two_BR_pal <- colorBin(
  palette = "YlGn",
  domain = cmhc_mtl_rent_diff_Two_BR$Two_BR,
  bins = bins_cmhc_mtl_rent)

Three_plus_pal <- colorBin(
  palette = "Blues",
  domain = cmhc_mtl_rent_diff_Three_plus$Three_plus,
  bins = bins_cmhc_mtl_rent)

Total_pal <- colorBin(
  palette = "Purples",
  domain = cmhc_mtl_rent_diff_Total$Total,
  bins = bins_cmhc_mtl_rent)


# Making the map!
cmhc_mtl_rent <- leaflet()%>%
  addTiles()%>%
  addPolygons(data = cmhc_mtl_rent_diff_Studio,
              fillColor = ~Studio_pal(cmhc_mtl_rent_diff_Studio$Studio),
              color = "#000000", # outline color
              fillOpacity = 0.55,
              group="Studio",
              weight = 0.2,
              # I'm using polygons with popups here to make popup info blurbs
              popup = paste("Quartier: ", 
                            "<br>", 
                            "Neighborhood: <br>" ,
                            cmhc_mtl_rent_diff_Studio$ZONENAME_F,
                            "<br>",
                            "2012 - 2021 Différence de loyer SCHL : <br>",
                            "<br>",
                            "2012 - 2021 CMHC Difference in Rent: <br> $",
                            cmhc_mtl_rent_diff_Studio$Studio))%>%
  addPolygons(data = cmhc_mtl_rent_diff_One_BR,
              fillColor = ~One_BR_pal(cmhc_mtl_rent_diff_One_BR$One_BR),
              color = "#000000", 
              fillOpacity = 0.55,
              group="One Bedroom",
              weight = 0.2,
              popup = paste("Quartier: ", 
                            "<br>", 
                            "Neighborhood: <br>" ,
                            cmhc_mtl_rent_diff_One_BR$ZONENAME_F,
                            "<br>",
                            "2012 - 2021 Différence de loyer SCHL : <br>",
                            "<br>",
                            "2012 - 2021 CMHC Difference in Rent: <br> $",
                            cmhc_mtl_rent_diff_One_BR$One_BR))%>%
  addPolygons(data = cmhc_mtl_rent_diff_Two_BR,
              fillColor = ~Two_BR_pal(cmhc_mtl_rent_diff_Two_BR$Two_BR),
              color = "#000000", 
              fillOpacity = 0.55,
              group="Two Bedrooms",
              weight = 0.2,
              popup = paste("Quartier: ", 
                            "<br>", 
                            "Neighborhood: <br>" ,
                            cmhc_mtl_rent_diff_Two_BR$ZONENAME_F,
                            "<br>",
                            "2012 - 2021 Différence de loyer SCHL : <br>",
                            "<br>",
                            "2012 - 2021 CMHC Difference in Rent: <br> $",
                            cmhc_mtl_rent_diff_Two_BR$Two_BR))%>%
  addPolygons(data = cmhc_mtl_rent_diff_Three_plus,
              fillColor = ~Three_plus_pal(cmhc_mtl_rent_diff_Three_plus$Three_plus),
              color = "#000000", 
              fillOpacity = 0.55,
              group="Three Bedrooms Plus",
              weight = 0.2,
              popup = paste("Quartier: ", 
                            "<br>", 
                            "Neighborhood: <br>" ,
                            cmhc_mtl_rent_diff_Three_plus$ZONENAME_F,
                            "<br>",
                            "2012 - 2021 Différence de loyer SCHL : <br>",
                            "<br>",
                            "2012 - 2021 CMHC Difference in Rent: <br> $",
                            cmhc_mtl_rent_diff_Three_plus$Three_plus))%>%
  addPolygons(data = cmhc_mtl_rent_diff_Total,
              fillColor = ~Total_pal(cmhc_mtl_rent_diff_Total$Total),
              color = "#000000", 
              fillOpacity = 0.55,
              group="Total",
              weight = 0.2,
              popup = paste("Quartier: ", 
                            "<br>", 
                            "Neighborhood: <br>" ,
                            cmhc_mtl_rent_diff_Total$ZONENAME_F,
                            "<br>",
                            "2012 - 2021 Différence de loyer SCHL : <br>",
                            "<br>",
                            "2012 - 2021 CMHC Difference in Rent: <br> $",
                            cmhc_mtl_rent_diff_Total$Total))%>%
  addLayersControl(
    overlayGroups =c("Studio", "Une CH/One BR", "Deux CH/Two BR", "Trois CH+/Three BR+", "Total"),
    options = layersControlOptions(collapsed=FALSE)) %>%
  addLegend("bottomright", 
            pal = Total_pal, 
            values = cmhc_mtl_rent_diff_Total$Total,
            title = paste("2012-2022",
                          "<br>",
                          "Différence de loyer SCHL :",
                          "<br>",
                          "CMHC Difference in Rent:"),
            group = "total",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1
  )

# Displaying (printing) the map
cmhc_mtl_rent

# Saving it to a computer
saveWidget(cmhc_mtl_rent, file.path('PATH/TO/FILE','name_of_file.html'))

