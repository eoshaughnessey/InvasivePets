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

#merge huc population sizes with other data
huc_pop <- read.csv("E:/postdoc/R/InvasivePets/huc_populations.csv", 
                    stringsAsFactors = FALSE)
hp <- as.data.table(huc_pop)
hp[,Population := as.numeric(Population)] 


hg <- read.csv("E:/postdoc/R/InvasivePets/huc_gps_table.csv",
               stringsAsFactors = FALSE)
hg <- as.data.table(hg)

#plot number of unique species by population
hg_count <- setDT(hg)[ , .(
  count=.N) ,
  by = .(HUC12_1,common)]
View(hg_count)
count_pop <- merge(hp,hg_count,by="HUC12_1", all=TRUE)
View(count_pop)
#library(dplyr)
#elimainte count column
count_pop[,count:=NULL]
head(count_pop)
DT<-data.table(count_pop)
DT<-DT[,.(species=length(unique(common[!is.na(common)]))), by=Population]
DT<-as.data.table(DT)
head(DT)
reg<-lm(Population~species, data=DT)
summary(reg)


plot(DT$Population, DT$species,
     xlab="population", 
     ylab="number of species", 
     pch=19
     )



#abline(lm(count~Population, data=y), col="red")

#population by group/taxa
hg_group <- setDT(hg)[ , .(
  count=.N) ,
  by = .(HUC12_1,group_)]
View(hg_group)
group_pop <- merge(hp,hg_group,by="HUC12_1", all=TRUE)
View(group_pop)
group_pop[,count:=NULL]
head(group_pop)
GP<-data.table(group_pop)
GP<-GP[,.(taxa=length(unique(group_[!is.na(group_)]))), by=Population]
GP<-as.data.table(GP)
head(GP)
reg<-lm(Population~taxa, data=GP)
summary(reg)

huc_sp_grp<-data.frame(huc_gps_table[,c("HUC12_1","group_","common")])
huc_grp_sp <- merge(hp,huc_sp_grp,by="HUC12_1", all=TRUE)
View(huc_grp_sp)
hucgrpsp<-with(huc_grp_sp, tapply(common, list(Population,group_),
                              FUN=function(x)length(unique(x))))
head(hucgrpsp)
View(hucgrpsp)
hucgrpsp<-subset(hucgrpsp, 
                 select=-c(Coelenterates, Crustaceans))
head(hucgrpsp)
hucgrpsp<-hucgrpsp[,-4]
hucgrpsp[is.na(hucgrpsp)]<-0
hucgrpsp<-as.data.frame(hucgrpsp)
View(hucgrpsp)
str(hucgrpsp)
hucgrpsp<-setNames(cbind(rownames(hucgrpsp), hucgrpsp, row.names=NULL),
         c("HUC", "Fishes", "Mollusks", "Plants"))
hucgrpsp[,1] <- as.numeric(as.character(hucgrpsp[,1]))  

#multiple regression
test<-lm(HUC~Fishes+Mollusks+Plants, data=hucgrpsp)
summary(test)

#regression tree
test2<-rpart(HUC~Fishes+Mollusks+Plants, data=hucgrpsp, method="class")
summary(test2)
printcp(test2)
plotcp(test2)
plot(test2, uniform=TRUE,
     main="classification tree for population size x number of species present within each taxon")
text(test2, use.n=TRUE, all=TRUE, cex=.8)
post(test2, file="e:/postdoc/R/cart.invasive.ps")
pfit<-prune(test2, cp=0.0100000)
plot(pfit, uniform=TRUE)
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

hist(hucgrpsp$HUC, 
     col="blue")


