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
data_file <- read_csv("~/InvasivePets/data_file_redo.csv")
georef=st_as_sf(data_file, coords=c("Longitude","Latitude")) %>%
  st_set_crs(4326)
st_crs(georef)

#bring in huc12s within the three counties and 
#set the crs to WGS84
huc2<-'HUCs_clipped'
huc<-st_read(huc2, stringsAsFactors = FALSE)
huc_transform=st_transform(huc, crs=4326)

#after transforming crs, combine the shapefile with three separate counties(county3)
county3=st_transform(counties_il_three, crs=4326)
state_il<-st_union(county3)
plot(state_il, col='blanchedalmond')
plot(georef, add=TRUE)

#see how the hucs are located in the three counties
huc_state_il<-st_intersection(huc_transform,state_il)
plot(huc_state_il$geometry, col=NA, border='blue')
View(huc_state_il)

#see how gps coordinates are found within the three counties
#this step seems unnecessary, as you could just intersect georef with huc test
georef_3<-st_intersection(georef,state_il)
View(georef_3)

#see how the gps points lay within the hucs of three counties
plot(huc_state_il$geometry, col=NA, border='blue')
plot(georef_3$geometry, pch=20, col='red', add=TRUE)


#but we still need a single data frame for counties, hucs, and gps poiints
#I didn't know we already had this done in the MasterDataSheet...
huc_gps<-st_intersection(huc_transform,georef_3)
View(huc_gps)
huc_gps_table<-huc_gps[,c(22,46,47,48,49,50,51,52,53)] #removes unwanted columns
head(huc_gps_table)
write.csv(huc_gps_table, file="huc_gps_table.csv")

#bring in centroid files
centroid_shp="Census"
centroid=st_read(centroid_shp, stringsAsFactors = FALSE) %>%st_set_crs(4326)
st_crs(centroid)
View(centroid)

#determine in which huc_12 each centroid is in
centroid_huc<-st_intersection(centroid, huc_transform)
View(centroid_huc)
centroid_huc_crop<-centroid_huc[,c(1,2,3,8,32)]
head(centroid_huc_crop)
View(centroid_huc_crop)
write.csv(centroid_huc, file="centroid_huc.csv")

#determine population size of each huc
huc_population<-centroid_huc_crop %>%
  group_by(HUC12_1) %>%
  summarise(Population = sum(DP0010001))
View(huc_population)
write.csv(huc_population, file="huc_populations.csv")

dt <- read.csv("~/InvasivePets/MasterDataSheet_temperature")

output$cntymap_cn <-renderLeaflet({
  leaflet(cnty)%>%
    addProviderTiles('Esri.WorldImagery', group='Esri')%>% 
    addProviderTiles('Stamen.TonerLite', group='Stamen')%>% 
    addLayersControl(baseGroups=c('Esri','Stamen'))%>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>% 
    addPolygons()%>%
    addCircles(
      data = filteredData_cn(), 
      radius = ~count, 
      lat = ~Latitude, 
      lng = ~Longitude,
      col='red'
    )
})


