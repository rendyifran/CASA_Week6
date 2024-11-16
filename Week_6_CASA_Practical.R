library(spatstat)
library(here)
library(sp)
library(tmap)
library(sf)
library(tmaptools)
library(stringr)
library(tidyverse)

Hide##First, get the London Borough Boundaries
LondonBoroughs <- st_read(here::here("ESRI", "London_Borough_Excluding_MHW.shp"))

BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

##Now get the location of all Blue Plaques in the City
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")
BluePlaques<-st_transform(BluePlaques,27700)

tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

BluePlaques<-distinct(BluePlaques)

BluePlaquesSub <- BluePlaques[BoroughMap,]
#check to see that they've been removed
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

# add sparse=false to get the complete matrix.
intersect_indices <-st_intersects(BoroughMap, BluePlaques)

Londonborough <- st_read(here::here("ESRI", 
                                    "London_Borough_Excluding_MHW.shp"))%>%
  st_transform(., 27700)

OSM <- st_read(here::here("greater-london-latest-free.shp", 
                              "gis_osm_pois_a_free_1.shp")) %>%
  st_transform(., 27700) %>%
  #select hotels only
  filter(fclass == 'hotel')

join_example <- st_join(OSM, Londonborough)

# read in the .csv
# and make it into spatial data

Airbnb <- read_csv("listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  #select entire places that are available all year
  filter(room_type == 'Entire home/apt' & availability_365 =='365')


# make a function for the join
# functions are covered in practical 7
# but see if you can work out what is going on
# hint all you have to do is replace data1 and data2
# with the data you want to use

Joinfun <- function(data1, data2){
  
  output<- data1%>%
    st_join(Londonborough,.) %>%
    add_count(GSS_CODE, name="hotels_in_borough") 
  
  return(output)
}

# use the function for hotels
Hotels <- Joinfun(OSM, Londonborough)

# then for airbnb
Airbnb <- Joinfun(Airbnb, Londonborough)

Hotels <- Hotels %>%
  #at the moment each hotel is a row for the borough
  #we just one one row that has number of airbnbs
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))

Airbnb <- Airbnb %>%
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))

all_accomodation <- st_join(Hotels, Airbnb, join = st_equals)

head(all_accomodation)

#extract the borough

# select by attribute
Harrow <- BoroughMap %>%
  filter(., NAME=="Harrow")

#Check to see that the correct borough has been pulled out
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5)

#clip the data to our single borough
BluePlaquesSub <- BluePlaques[Harrow,]
#check that it's worked
tmap_mode("plot")

tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

window <- as.owin(Harrow)
plot(window)
