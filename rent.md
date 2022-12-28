Cela montre les différences de loyer telles que calculées par la
[SCHL](https://www.cmhc-schl.gc.ca/). Comme toutes les données ne sont
pas collectées chaque année, il y a des années avec des valeurs “NA”.

This shows the differences in rent as calculated by the
[CMHC](https://www.cmhc-schl.gc.ca/). As not all data is collected each
year, there are years with “NA” values.

    # Packages
    library(geojsonio)

    ## Registered S3 method overwritten by 'geojsonsf':
    ##   method        from   
    ##   print.geojson geojson

    ## 
    ## Attaching package: 'geojsonio'

    ## The following object is masked from 'package:base':
    ## 
    ##     pretty

    library(here)

    ## here() starts at /Users/cevieth/Desktop/*MTL Rental Research/Rstats Coding/Coding

    library(hrbrthemes) # typographic theme elements

    ## NOTE: Either Arial Narrow or Roboto Condensed fonts are required to use these themes.

    ##       Please use hrbrthemes::import_roboto_condensed() to install Roboto Condensed and

    ##       if Arial Narrow is not on your system, please see https://bit.ly/arialnarrow

    library(leaflet)
    library(leaflet.extras)
    library(leaflet.extras2)
    library(sf) # spatial data and visualization

    ## Linking to GEOS 3.10.2, GDAL 3.4.2, PROJ 8.2.1; sf_use_s2() is TRUE

    library(tidyverse) # Using ggplot, tidyr, purr, dplyr

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

    library(tongfen)

    cmhc_mtl_rent_diff_Studio  <- st_read('cmhc_mtl_rent_diff_Studio.geojson') %>%
      st_as_sf()

    ## Reading layer `cmhc_mtl_rent_diff_Studio' from data source 
    ##   `/Users/cevieth/Desktop/*MTL Rental Research/Rstats Coding/Coding/CMHC Over Time/cmhc_mtl_rent_diff_Studio.geojson' 
    ##   using driver `GeoJSON'
    ## Simple feature collection with 35 features and 2 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -74.32797 ymin: 45.22901 xmax: -73.12856 ymax: 45.96849
    ## Geodetic CRS:  WGS 84

    cmhc_mtl_rent_diff_One_BR <- st_read('cmhc_mtl_rent_diff_One_BR.geojson') %>%
      st_as_sf()

    ## Reading layer `cmhc_mtl_rent_diff_One_BR' from data source 
    ##   `/Users/cevieth/Desktop/*MTL Rental Research/Rstats Coding/Coding/CMHC Over Time/cmhc_mtl_rent_diff_One_BR.geojson' 
    ##   using driver `GeoJSON'
    ## Simple feature collection with 35 features and 2 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -74.32797 ymin: 45.22901 xmax: -73.12856 ymax: 45.96849
    ## Geodetic CRS:  WGS 84

    cmhc_mtl_rent_diff_Two_BR <- st_read('cmhc_mtl_rent_diff_Two_BR.geojson') %>%
      st_as_sf()

    ## Reading layer `cmhc_mtl_rent_diff_Two_BR' from data source 
    ##   `/Users/cevieth/Desktop/*MTL Rental Research/Rstats Coding/Coding/CMHC Over Time/cmhc_mtl_rent_diff_Two_BR.geojson' 
    ##   using driver `GeoJSON'
    ## Simple feature collection with 35 features and 2 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -74.32797 ymin: 45.22901 xmax: -73.12856 ymax: 45.96849
    ## Geodetic CRS:  WGS 84

    cmhc_mtl_rent_diff_Three_plus <- st_read('cmhc_mtl_rent_diff_Three_plus.geojson') %>%
      st_as_sf()

    ## Reading layer `cmhc_mtl_rent_diff_Three_plus' from data source 
    ##   `/Users/cevieth/Desktop/*MTL Rental Research/Rstats Coding/Coding/CMHC Over Time/cmhc_mtl_rent_diff_Three_plus.geojson' 
    ##   using driver `GeoJSON'
    ## Simple feature collection with 35 features and 2 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -74.32797 ymin: 45.22901 xmax: -73.12856 ymax: 45.96849
    ## Geodetic CRS:  WGS 84

    cmhc_mtl_rent_diff_Total <- st_read('cmhc_mtl_rent_diff_Total.geojson') %>%
      st_as_sf()

    ## Reading layer `cmhc_mtl_rent_diff_Total' from data source 
    ##   `/Users/cevieth/Desktop/*MTL Rental Research/Rstats Coding/Coding/CMHC Over Time/cmhc_mtl_rent_diff_Total.geojson' 
    ##   using driver `GeoJSON'
    ## Simple feature collection with 35 features and 2 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -74.32797 ymin: 45.22901 xmax: -73.12856 ymax: 45.96849
    ## Geodetic CRS:  WGS 84

    # Bins (for legend)
    bins_cmhc_mtl_rent <- c(0, 100, 200, 300, 400, 500, 600, Inf)

    # Using colorNumeric so that there will be a gradient (continuous colors)
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

    mhc_mtl_rent <- leaflet()%>%
      addTiles()%>%
      addPolygons(data = cmhc_mtl_rent_diff_Studio,
                  fillColor = ~Studio_pal(cmhc_mtl_rent_diff_Studio$Studio),
                  color = "#000000", #this is an outline color
                  fillOpacity = 0.55,
                  group="Studio",
                  weight = 0.2,
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
                  color = "#000000", #this is an outline color
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
                  color = "#000000", #this is an outline color
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
                  color = "#000000", #this is an outline color
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
                  color = "#000000", #this is an outline color
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

    # Print the map
    mhc_mtl_rent

![](CMHC-Over-Time-web_files/figure-markdown_strict/making%20the%20map-1.png)
