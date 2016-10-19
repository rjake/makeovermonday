#This script will convert a GIS polygon to Tableau polygon

#First establish your workspace & libraries
  setwd("C:/Users/jriley215/Desktop/Fortify Polygons")

  library(tidyverse)  #dplyr piping
  library(maptools)   #fortify requires rgeos which is here
  library(rgdal)      #readOGR and spTransform


#Read in map (change "layer = ..." to match you file) then assign CRS (second line)
  shape <- readOGR(dsn=".", layer="Census Tracts Philly", stringsAsFactors=FALSE) %>%
           spTransform(., CRS("+init=epsg:4326")) #this is WGS84

#Use fortify() to turn the shapefile into a dataframe
#you will need to change "region = ..." to identify the field that makes each polygon piece unique
#in this example, GEOID10 is the FIPS code and unifies each polygon
  ggshape <- fortify(shape, region="GEOID10")

#write to csv
  write.csv(ggshape, "shapefile.csv")