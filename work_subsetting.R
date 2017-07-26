library(sf)
library(rgdal)
#list.files("../handouts/data")

#bring in shapefile of US counties
shp="../handouts/data/cb_2016_us_county_5m"
counties=st_read(shp, stringsAsFactors = FALSE)

#crop the shapefile to just IL counties, and then down to the three counties of interest
#plot to check
library(dplyr)
counties_il=filter(counties, STATEFP=='17')
head(counties)
counties_il_three=filter(counties_il, NAME %in% c('Cook','DuPage', 'Lake'))
plot(counties_il_three$geometry) 

#install.packages('leaflet')
#library(leaflet)
#leaflet(counties_il_three)%>%
#  addProviderTiles('Esri.WorldImagery')%>% 
#  addPolygons()%>%
#  addMarkers(data=georef, popup=~common)

#bring in data file that contains gps coordinates of organisms
#set the coordinate reference system and check
#we had to put all coordinates on the WGS84 system because 
#we couldn't get the georef file into NAD83
georef=st_as_sf(data_file, coords=c("Longitude","Latitude")) %>%
  st_set_crs(4326)
st_crs(georef)

#bring in huc12s within the three counties and 
#set the crs to WGS84
huc2<-'HUCs_clipped'
huc<-st_read(huc2, stringsAsFactors = FALSE)
huc_transform=st_transform(huc, crs=4326)

#combine the shapefile with three separate counties(county3)
state_il<-st_union(county3)
huc_state_il<-st_intersection(huc_transform,state_il)
county3=st_transform(counties_il_three, crs=4326)
plot(county3$geometry, col='blanchedalmond')
plot(georef, add=TRUE)
georef_3<-st_intersection(georef,county3)
plot(georef_3$geometry, add=TRUE, pch=1, col='red')
huc2<-'HUCs_clipped.shp'
huc<-st_read(huc2, stringsAsFactors = FALSE)
huc_transform=st_transform(huc, crs=4326)
state_il<-st_union(county3)
georef_3<-st_intersection(georef,state_il)
plot(georef_3$geometry, border='black', pch=20, col='red')
huc_test<-st_intersection(huc_transform,georef)
plot(huc_transform$geometry)
huc_state_il<-st_intersection(huc_transform,state_il)
plot(huc_state_il$geometry, col=NA, border='blue')
huc_il_georef<-st_intersection(georef,huc_state_il)
plot(huc_il_georef$geometry, col=NA)



#plot(state_il)
#plot(georef_3, add=TRUE, col='coral1', pch=20)









#Code from 7/20/17...no longer important
#georef<-st_transform(georef, crs=st_crs(counties_il_three))
#st_contains(georef,counties_il_three)
#Coords=cbind(data_file$Longitude,data_file$Latitude)
#sp=SpatialPoints(coords)
#plot(sp)
#spdf=SpatialPointsDataFrame(coords, as.data.frame(data_file), 
#                            proj4string =CRS('+init=epsg:26916'))
#plot(spdf)
#st_crs(spdf)
#CRS(spdf)


#georef<-st_as_sf(spdf)
#class(georef)
#plot(georef$geometry)
#st_crs(georef)
#st_crs(counties_il_three)
#georef<-st_transform(georef, crs=st_crs(counties_il_three)$proj4string)
#plot(counties_il_three, col='aquamarine4', add=TRUE, lwd=2)
#st_contains(georef, counties_il_three)
#st_crs(georef)