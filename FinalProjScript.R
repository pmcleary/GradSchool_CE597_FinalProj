library("raster")
library("sf")
library("tidyr")
library("tidyverse")
library("plyr")
library("tmap")
library("tmaptools")
library("RColorBrewer")
library("lwgeom")
library("Hmisc")
library("exactextractr")
library("GISTools")
library("rgeos")
library("spdep")
library("mapview")
library("lubridate")
library("geosphere")
library("maps")
library("classInt")
library("ggplot2")
library("gganimate")
library("rnaturalearth")
library("rnaturalearthdata")
#--------------------Data Preperation-------------------------
# Set the working directory
setwd('~/Fall 19/Data Analytics/Final Project')
wd <- '~/Fall 19/Data Analytics/Final Project'
flights <- read.csv("asdi_2014_07_01.csv")
# Convert time 
us.population <- read.csv("UScountyPoP.csv")
US <- st_read("cb_2017_us_county_500k/cb_2017_us_county_500k.shp")
airports1 <- read.csv("airports.csv")
airports <- st_as_sf(airports1, coords = c("LONGITUDE","LATITUDE"), crs = 4269)
# Repoject flights to Lambert Conformal Conic to preserve shape
# Popular projection for aeronautical charts
airports <- st_transform(airports, 102009)
# filter population by midwestern states
mw.pop <- filter(us.population, STATE %in% c('17', '18','19', '26','27','29','39','55'))
#remove na values in the df
mw.pop <- filter(mw.pop, COUNTY != 0)
mw.pop2018 <- mw.pop$POPESTIMATE2018
# Remove rows with NA values
flights <- na.omit(flights)
# Create an sf object from the flight data using the spherical mercator projection
flights <- st_as_sf(flights, coords = c("LONGITUDE", "LATITUDE"), crs = 4269)
midwest <- filter(US, STATEFP %in% c('17', '18','19', '26','27','29','39','55'))
midwest <- arrange(midwest,STATEFP, COUNTYFP)
midwest['pop2018'] <- mw.pop2018
midwest <- st_transform(midwest, crs(flights))
mwflights <- st_crop(flights, c(xmin=-97.25, xmax=-80, ymin=36.9, ymax=49))
mwflights <- st_transform(mwflights, 102009)
# UPDATE_TIME is UTC, only want flights on one day(2014-07-01)
mwflights['UPDATE_TIME'] <- ymd_hms(mwflights$UPDATE_TIME)
mwflights['DATE'] <- date(mwflights$UPDATE_TIME)
mwflights <- filter(mwflights, DATE == "2014-07-01")
mwf_4000ft <- filter(mwflights, ALTITUDE <=4000)
# Check population by county
midwest <- arrange(midwest,desc(pop2018))
tmap_mode("view")
#tm_shape(mwf_4000ft) + tm_symbols(col = "red", size = 0.2)
#tm_shape(midwest) + tm_polygons(col = "pop2018", style = "jenks")
mapview(mwf_4000ft)
# Determine the number of flights assciated with each airport
# Create a buffer around each airport
# first have to align the coordinate sytems and use one with meters as the distance
OHare.buffer <- st_buffer(airports[1,], 11500)
Midway.buffer <- st_buffer(airports[2,], 11500)
detroit.buffer <- st_buffer(airports[3,], 15000)
columbus.buffer <- st_buffer(airports[4,], 15000)
Minneapolis.buffer <- st_buffer(airports[5,], 15000)
cleveland.buffer <- st_buffer(airports[6,], 15000)
StLouis.buffer <- st_buffer(airports[7,], 15000)
Indianapolis.buffer <- st_buffer(airports[8,], 15000)
Milwaukee.buffer <- st_buffer(airports[9,], 15000)
Cincinnati.buffer <- st_buffer(airports[10,], 15000)
KansasCity.buffer <- st_buffer(airports[11,], 15000)
GrandRapids.buffer <- st_buffer(airports[12,], 15000)
# Use the intersection of the buffer and flights under 4000ft
ohare.flights <- st_intersection(mwf_4000ft, OHare.buffer)
Midway.flights <- st_intersection(mwf_4000ft, Midway.buffer)
detroit.flights <- st_intersection(mwf_4000ft, detroit.buffer)
columbus.flights <- st_intersection(mwf_4000ft, columbus.buffer)
Minneapolis.flights <- st_intersection(mwf_4000ft, Minneapolis.buffer)
cleveland.flights <- st_intersection(mwf_4000ft, cleveland.buffer)
StLouis.flights <- st_intersection(mwf_4000ft, StLouis.buffer)
Indianapolis.flights <- st_intersection(mwf_4000ft, Indianapolis.buffer)
Milwaukee.flights <- st_intersection(mwf_4000ft, Milwaukee.buffer)
Cincinnati.flights <- st_intersection(mwf_4000ft, Cincinnati.buffer)
KansasCity.flights <- st_intersection(mwf_4000ft, KansasCity.buffer)
GrandRapids.flights <- st_intersection(mwf_4000ft, GrandRapids.buffer)

#---------Experimenting to find how to connect two cities----------------
# Below code was placed into the function 'connections'
# Finding flights that connect St Louis & OHare
#ohare.StLouis.df <- inner_join(ohare.flights %>% as.data.frame(),StLouis.flights  %>% as.data.frame(), by = "OBJECT_ID")
# Create linestring from points, create multipoint first
# Extract unique values b/w locations
#ohare.StLouis.unique <- unique(ohare.StLouis.df["OBJECT_ID"])
#ohare.StLouis.unique <- as.matrix(ohare.StLouis.unique)
# pull the full length flights from mwflights
#test <- filter(mwflights,OBJECT_ID %in% ohare.StLouis.unique)
# need to create an sf object of each unique OBJECT_ID
#my_list <- list()
#for(i in unique(test$OBJECT_ID)) {
#  nam <- paste("df", i, sep = ".")
#  my_list[[i]] <- assign(nam, test[test$OBJECT_ID==i,])
#}
#function to convert the geometries to linestring
tolinestring <- function(x){
  temp <- st_combine(x)
  st_cast(temp,"LINESTRING")
}
#Convert all ohare 2 StLouis flights to linestring
#my_list <- lapply(my_list,tolinestring)
#---Use experiment form above to create sf objects of all flights two places have in common--
# Function to create a list sf objects with all flights that connect two cities 
connections <- function(x,y){
  a <- inner_join(x %>% as.data.frame(), y  %>% as.data.frame(), by = "OBJECT_ID")
  c <- unique(a["OBJECT_ID"])
  c <- as.matrix(c)
  e <- filter(mwflights,OBJECT_ID %in% c)
  function_list <- list()
  for(i in unique(e$OBJECT_ID)) {
    nam <- paste("df", i, sep = ".")
    function_list[[i]] <- assign(nam, e[e$OBJECT_ID==i,])
  }
  function_list <- lapply(function_list, tolinestring)
}
# test the function
ohare.Minneapolis <- connections(ohare.flights,Minneapolis.flights)
#------Create List of City Flight SF DFs-----------------------------------------
# Create a list of sf dfs with each city as the the base
#then use the connections functions to detrmine all the common connections
ohare.list <- list(ohare.flights,Midway.flights,detroit.flights,columbus.flights,
                          Minneapolis.flights,cleveland.flights,StLouis.flights,Indianapolis.flights,
                          Milwaukee.flights,Cincinnati.flights,KansasCity.flights,GrandRapids.flights)
# Create an empty list of all ohare connections
ohare.connections <- list()
for (i in 2:length(ohare.list)){
  x = as.data.frame(ohare.list[[1]])
  y = as.data.frame(ohare.list[[i]])
  ohare.connections[[i]] = connections(x,y)
}
# Create a list of all midway connections
midway.list <- list(Midway.flights,ohare.flights,detroit.flights,columbus.flights,
                    Minneapolis.flights,cleveland.flights,StLouis.flights,Indianapolis.flights,
                    Milwaukee.flights,Cincinnati.flights,KansasCity.flights,GrandRapids.flights)
midway.connections <- list()
for (i in 2:length(midway.list)){
  x = as.data.frame(midway.list[[1]])
  y = as.data.frame(midway.list[[i]])
  midway.connections[[i]] = connections(x,y)
}
# Create a list of all detroit connections
detroit.list <- list(detroit.flights,ohare.flights,Midway.flights,columbus.flights,
                     Minneapolis.flights,cleveland.flights,StLouis.flights,Indianapolis.flights,
                     Milwaukee.flights,Cincinnati.flights,KansasCity.flights,GrandRapids.flights)
detroit.connections <- list()
for (i in 2:length(detroit.list)){
  x = as.data.frame(detroit.list[[1]])
  y = as.data.frame(detroit.list[[i]])
  detroit.connections[[i]] = connections(x,y)
}
# Create a list of all columbus connections
columbus.list <- list(columbus.flights,ohare.flights,Midway.flights,detroit.flights,Minneapolis.flights,cleveland.flights,StLouis.flights,Indianapolis.flights,
                      Milwaukee.flights,Cincinnati.flights,KansasCity.flights,GrandRapids.flights)
columbus.connections <- list()
for (i in 2:length(columbus.list)){
  x = as.data.frame(columbus.list[[1]])
  y = as.data.frame(columbus.list[[i]])
  columbus.connections[[i]] = connections(x,y)
}
# Create a list of all minneapolis connections
minneapolis.list <- list(Minneapolis.flights,ohare.flights,Midway.flights,detroit.flights,columbus.flights,cleveland.flights,StLouis.flights,Indianapolis.flights,
                         Milwaukee.flights,Cincinnati.flights,KansasCity.flights,GrandRapids.flights)
minneapolis.connections <- list()
for (i in 2:length(minneapolis.list)){
  x = as.data.frame(minneapolis.list[[1]])
  y = as.data.frame(minneapolis.list[[i]])
  minneapolis.connections[[i]] = connections(x,y)
}

# Create a list of all cleveland connections
cleveland.list <- list(cleveland.flights,ohare.flights,Midway.flights,detroit.flights,columbus.flights,
                       Minneapolis.flights,StLouis.flights,Indianapolis.flights,
                       Milwaukee.flights,Cincinnati.flights,KansasCity.flights,GrandRapids.flights)
cleveland.connections <- list()
for (i in 2:length(cleveland.list)){
  x = as.data.frame(cleveland.list[[1]])
  y = as.data.frame(cleveland.list[[i]])
  cleveland.connections[[i]] = connections(x,y)
}

# Create a list of all stlouis connections
stlouis.list <- list(StLouis.flights,ohare.flights,Midway.flights,detroit.flights,columbus.flights,
                     Minneapolis.flights,cleveland.flights,Indianapolis.flights,
                     Milwaukee.flights,Cincinnati.flights,KansasCity.flights,GrandRapids.flights)
stlouis.connections <- list()
for (i in 2:length(stlouis.list)){
  x = as.data.frame(stlouis.list[[1]])
  y = as.data.frame(stlouis.list[[i]])
  stlouis.connections[[i]] = connections(x,y)
}

# Create a list of all Indy connections
indianapolis.list <- list(Indianapolis.flights,ohare.flights,Midway.flights,detroit.flights,columbus.flights,
                          Minneapolis.flights,cleveland.flights,StLouis.flights,
                          Milwaukee.flights,Cincinnati.flights,KansasCity.flights,GrandRapids.flights)
indianapolis.connections <- list()
for (i in 2:length(indianapolis.list)){
  x = as.data.frame(indianapolis.list[[1]])
  y = as.data.frame(indianapolis.list[[i]])
  indianapolis.connections[[i]] = connections(x,y)
}

# Create a list of all milwaukee connections
milwaukee.list <- list(Milwaukee.flights,ohare.flights,Midway.flights,detroit.flights,columbus.flights,
                       Minneapolis.flights,cleveland.flights,StLouis.flights,Indianapolis.flights
                       ,Cincinnati.flights,KansasCity.flights,GrandRapids.flights) 
milwaukee.connections <- list()
for (i in 2:length(milwaukee.list)){
  x = as.data.frame(milwaukee.list[[1]])
  y = as.data.frame(milwaukee.list[[i]])
  milwaukee.connections[[i]] = connections(x,y)
}

# Create a list of all cincinnati connections
cincinnati.list <- list(Cincinnati.flights,ohare.flights,Midway.flights,detroit.flights,columbus.flights,
                        Minneapolis.flights,cleveland.flights,StLouis.flights,Indianapolis.flights,
                        Milwaukee.flights,KansasCity.flights,GrandRapids.flights)
cincinnati.connections <- list()
for (i in 2:length(cincinnati.list)){
  x = as.data.frame(cincinnati.list[[1]])
  y = as.data.frame(cincinnati.list[[i]])
  cincinnati.connections[[i]] = connections(x,y)
}

# Create a list of all kc connections
kansascity.list <- list(KansasCity.flights,ohare.flights,Midway.flights,detroit.flights,columbus.flights,
                        Minneapolis.flights,cleveland.flights,StLouis.flights,Indianapolis.flights,
                        Milwaukee.flights,Cincinnati.flights,GrandRapids.flights)
kansascity.connections <- list()
for (i in 2:length(kansascity.list)){
  x = as.data.frame(kansascity.list[[1]])
  y = as.data.frame(kansascity.list[[i]])
  kansascity.connections[[i]] = connections(x,y)
}

# Create a list of all grandrapids connections
grandrapids.list <- list(GrandRapids.flights,ohare.flights,Midway.flights,detroit.flights,columbus.flights,
                         Minneapolis.flights,cleveland.flights,StLouis.flights,Indianapolis.flights,
                         Milwaukee.flights,Cincinnati.flights,KansasCity.flights) 
grandrapids.connections <- list()
for (i in 2:length(grandrapids.list)){
  x = as.data.frame(grandrapids.list[[1]])
  y = as.data.frame(grandrapids.list[[i]])
  grandrapids.connections[[i]] = connections(x,y)
}

#Map ohare connections
#mapview(ohare.connections[[2]]) + mapview(ohare.connections[[3]]) + mapview(ohare.connections[[4]]) + mapview(ohare.connections[[5]]) + mapview(ohare.connections[[6]]) + mapview(ohare.connections[[7]]) + mapview(ohare.connections[[8]]) + mapview(ohare.connections[[9]]) + mapview(ohare.connections[[10]]) + mapview(ohare.connections[[11]]) + mapview(ohare.connections[[12]])
#Map midway connections
#mapview(midway.connections[[2]]) + mapview(midway.connections[[3]]) + mapview(midway.connections[[4]]) + mapview(midway.connections[[5]]) + mapview(midway.connections[[6]]) + mapview(midway.connections[[7]]) + mapview(midway.connections[[8]]) + mapview(midway.connections[[9]]) + mapview(midway.connections[[11]]) + mapview(midway.connections[[12]])
#Map detroit connections
#mapview(detroit.connections[[2]]) + mapview(detroit.connections[[3]]) + mapview(detroit.connections[[4]]) + mapview(detroit.connections[[5]]) + mapview(detroit.connections[[6]]) + mapview(detroit.connections[[7]]) + mapview(detroit.connections[[8]]) + mapview(detroit.connections[[9]]) + mapview(detroit.connections[[10]]) + mapview(detroit.connections[[11]]) + mapview(detroit.connections[[12]])
#Map columbus connections
#mapview(columbus.connections[[2]]) + mapview(columbus.connections[[3]]) + mapview(columbus.connections[[4]]) + mapview(columbus.connections[[5]]) + mapview(columbus.connections[[6]]) + mapview(columbus.connections[[7]]) + mapview(columbus.connections[[8]]) + mapview(columbus.connections[[11]])
#Map minneapolis connections
#mapview(minneapolis.connections[[2]]) + mapview(minneapolis.connections[[3]]) + mapview(minneapolis.connections[[4]]) + mapview(minneapolis.connections[[5]]) + mapview(minneapolis.connections[[6]]) + mapview(minneapolis.connections[[7]]) + mapview(minneapolis.connections[[8]]) + mapview(minneapolis.connections[[9]]) + mapview(minneapolis.connections[[10]])  + mapview(minneapolis.connections[[11]])  + mapview(minneapolis.connections[[12]])
#Map cleveland connections
#mapview(cleveland.connections[[2]]) + mapview(cleveland.connections[[3]]) + mapview(cleveland.connections[[4]]) + mapview(cleveland.connections[[5]]) + mapview(cleveland.connections[[6]]) + mapview(cleveland.connections[[7]]) + mapview(cleveland.connections[[8]]) + mapview(cleveland.connections[[9]]) + mapview(cleveland.connections[[12]])
#Map StL connections
#mapview(stlouis.connections[[2]]) + mapview(stlouis.connections[[3]]) + mapview(stlouis.connections[[4]]) + mapview(stlouis.connections[[5]]) + mapview(stlouis.connections[[6]]) + mapview(stlouis.connections[[7]]) + mapview(stlouis.connections[[8]]) + mapview(stlouis.connections[[9]]) + mapview(stlouis.connections[[10]]) + mapview(stlouis.connections[[11]])
#Map Indy connections
#mapview(indianapolis.connections[[2]]) + mapview(indianapolis.connections[[3]]) + mapview(indianapolis.connections[[4]]) + mapview(indianapolis.connections[[5]]) + mapview(indianapolis.connections[[6]]) + mapview(indianapolis.connections[[7]]) + mapview(indianapolis.connections[[8]]) + mapview(indianapolis.connections[[9]]) + mapview(indianapolis.connections[[10]]) + mapview(indianapolis.connections[[11]]) + mapview(indianapolis.connections[[12]])
#Map milwaukee connections
#mapview(milwaukee.connections[[2]]) + mapview(milwaukee.connections[[3]]) + mapview(milwaukee.connections[[4]]) + mapview(milwaukee.connections[[6]]) + mapview(milwaukee.connections[[7]]) + mapview(milwaukee.connections[[8]]) + mapview(milwaukee.connections[[9]]) + mapview(milwaukee.connections[[11]])
#Map cincinnati connections
#mapview(cincinnati.connections[[2]]) + mapview(cincinnati.connections[[4]]) + mapview(cincinnati.connections[[6]]) + mapview(cincinnati.connections[[8]]) + mapview(cincinnati.connections[[9]]) + mapview(cincinnati.connections[[11]]) + mapview(cincinnati.connections[[12]])
#Map KC connections
#mapview(kansascity.connections[[2]]) + mapview(kansascity.connections[[3]]) + mapview(kansascity.connections[[4]]) + mapview(kansascity.connections[[5]]) + mapview(kansascity.connections[[6]]) + mapview(kansascity.connections[[8]]) + mapview(kansascity.connections[[9]]) + mapview(kansascity.connections[[10]]) + mapview(kansascity.connections[[11]])
#Map Grand Rapids connections
mapview(grandrapids.connections[[2]]) + mapview(grandrapids.connections[[3]]) + mapview(grandrapids.connections[[4]]) + mapview(grandrapids.connections[[6]]) + mapview(grandrapids.connections[[7]]) + mapview(grandrapids.connections[[9]]) + mapview(grandrapids.connections[[11]])

#----------------------Plot Great Circle-------------------------------
# Locations
airport.connects <- read.csv("airport_connections.csv")
Beijing <- c(116,40)
Munich <- c(11,48)
San.Pedro.Sula <- c(-87,15)
mwports <- data.frame(x = airports1$LONGITUDE, y = airports1$LATITUDE)
ohr <- c(airports1[1,4],airports1[1,5])
mid <- c(airports1[2,4],airports1[2,5])
det <- c(airports1[3,4],airports1[3,5])
col <- c(airports1[4,4],airports1[4,5])
min <- c(airports1[5,4],airports1[5,5])
cle <- c(airports1[6,4],airports1[6,5])
stl <- c(airports1[7,4],airports1[7,5])
ind <- c(airports1[8,4],airports1[8,5])
mil <- c(airports1[9,4],airports1[9,5])
cin <- c(airports1[10,4],airports1[10,5])
kc <- c(airports1[11,4],airports1[11,5])
gr <- c(airports1[12,4],airports1[12,5])

# Calculate Great Circle routes with gcIntermediate
c2b <- gcIntermediate(ohr,Beijing, n=60, addStartEnd=TRUE, breakAtDateLine=T)
c2b1 <- c2b[1]
c2b2 <- c2b[2]
c2m <- gcIntermediate(ohr,Munich, n=60, addStartEnd=TRUE, breakAtDateLine=F)
c2s <- gcIntermediate(ohr,San.Pedro.Sula, n=60, addStartEnd=TRUE, breakAtDateLine=F)
c2det <- gcIntermediate(ohr,det, n=50, addStartEnd=TRUE, breakAtDateLine=F)


# function to convert the Great Circle routes to sf objects
tosf.object <- function(x){
  temp <- as.data.frame(x)
  colnames(temp) <- c("long","lat")
  temp2 <- st_as_sf(temp,coords = c("long","lat"), crs = 4326)
  temp3 <- tolinestring(temp2)
}
#-------------Visualize most connected routes---------------------
#Determine breaks of ohare flights to other locations
print(classIntervals(airport.connects[2:12,2], n=4, style="kmeans"))
ohr2all <- gcIntermediate(ohr,mwports, n=50, addStartEnd=TRUE)
# Convert GCs to sf
ohr2gr <- tosf.object(ohr2all[12])
ohr2kc <- tosf.object(ohr2all[11])
ohr2cin <- tosf.object(ohr2all[10])
ohr2mil <- tosf.object(ohr2all[9])
ohr2ind <- tosf.object(ohr2all[8])
ohr2stl <- tosf.object(ohr2all[7])
ohr2cle <- tosf.object(ohr2all[6])
ohr2minn <- tosf.object(ohr2all[5])
ohr2col <- tosf.object(ohr2all[4])
ohr2det <- tosf.object(ohr2all[3])
ohr2all.kmeans <- tm_shape(airports) + tm_dots(col='black') + tm_shape(ohr2det) +tm_lines(lwd=5, col='blue') + tm_shape(ohr2col) + tm_lines(lwd=5,col='blue') + tm_shape(ohr2stl) + tm_lines(lwd=5,col='blue') + tm_shape(ohr2mil) + tm_lines(lwd=5,col='blue') + tm_shape(ohr2kc) + tm_lines(lwd=5,col='blue') + tm_shape(ohr2cin) + tm_lines(lwd=1,col='blue') + tm_shape(ohr2ind) + tm_lines(lwd=10,col='blue') + tm_shape(ohr2cle) + tm_lines(lwd=10,col='blue') + tm_shape(ohr2minn) + tm_lines(lwd=15, col='blue') + tm_shape(ohr2gr) + tm_lines(lwd=1, col='blue')
print(ohr2all.kmeans)
#Determine breaks of Midway flights to other locations
midlist <- airport.connects[1:12,3]
#remove midway
midlist <- midlist[-2]
print(classIntervals(midlist, n=4, style="kmeans"))
mid2all <- gcIntermediate(mid,mwports, n=50, addStartEnd=TRUE)
mid2gr <- tosf.object(mid2all[12])
mid2kc <- tosf.object(mid2all[11])
mid2cin <- tosf.object(mid2all[10])
mid2mil <- tosf.object(mid2all[9])
mid2ind <- tosf.object(mid2all[8])
mid2stl <- tosf.object(mid2all[7])
mid2cle <- tosf.object(mid2all[6])
mid2min <- tosf.object(mid2all[5])
mid2col <- tosf.object(mid2all[4])
mid2det <- tosf.object(mid2all[3])
mid2ohr <- tosf.object(mid2all[1])
mid2all.kmeans <- tm_shape(airports) + tm_dots(col='black') + tm_shape(mid2det) +tm_lines(lwd=5, col='blue') + tm_shape(mid2col) + tm_lines(lwd=10,col='blue') + tm_shape(mid2stl) + tm_lines(lwd=15,col='blue') + tm_shape(mid2mil) + tm_lines(lwd=1,col='blue') + tm_shape(mid2kc) + tm_lines(lwd=15,col='blue') + tm_shape(mid2ind) + tm_lines(lwd=1,col='blue') + tm_shape(mid2cle) + tm_lines(lwd=10,col='blue') + tm_shape(mid2ohr) + tm_lines(lwd=10, col='blue') + tm_shape(mid2gr) + tm_lines(lwd=1, col='blue') + tm_shape(mid2min) + tm_lines(lwd=15, col='blue')
print(mid2all.kmeans)

#Determine breaks of Minn flights to other locations
minlist <- airport.connects[1:12,6]
#remove minn
minlist <- minlist[-5]
print(classIntervals(minlist, n=4, style="kmeans"))
min2all <- gcIntermediate(min,mwports, n=50, addStartEnd=TRUE)
min2gr <- tosf.object(min2all[12])
min2kc <- tosf.object(min2all[11])
min2cin <- tosf.object(min2all[10])
min2mil <- tosf.object(min2all[9])
min2ind <- tosf.object(min2all[8])
min2stl <- tosf.object(min2all[7])
min2cle <- tosf.object(min2all[6])
min2col <- tosf.object(min2all[4])
min2det <- tosf.object(min2all[3])
min2mid <- tosf.object(min2all[2])
min2ohr <- tosf.object(min2all[1])
min2all.kmeans <- tm_shape(airports) + tm_dots(col='black') + tm_shape(min2det) +tm_lines(lwd=5, col='blue') + tm_shape(min2col) + tm_lines(lwd=5,col='blue') + tm_shape(min2stl) + tm_lines(lwd=10,col='blue') + tm_shape(min2mil) + tm_lines(lwd=10,col='blue') + tm_shape(min2kc) + tm_lines(lwd=1,col='blue') + tm_shape(min2ind) + tm_lines(lwd=5,col='blue') + tm_shape(min2cle) + tm_lines(lwd=1,col='blue') + tm_shape(min2ohr) + tm_lines(lwd=15, col='blue') + tm_shape(min2gr) + tm_lines(lwd=1, col='blue')
print(min2all.kmeans)



c2b1.line <- tosf.object(c2b1)
c2b2.line <- tosf.object(c2b2)
c2m.line <- tosf.object(c2m)
c2s.line <- tosf.object(c2s) 
c2det.line <- tosf.object(c2det)
# Print GC map
mapview(c2b1.line) + mapview(c2m.line) + mapview(c2s.line) + mapview(c2b2.line)
mapview(c2det.line)
#-----Analysis------------------------------------------------------------------------------
plot(x = airport.connects$Pop.Rank, xlab = 'City Population Rank',ylab ='Connection Rank', main = 'Population vs Airport Connections', y = airport.connects$Connect.Rank)
text(x = airport.connects$Pop.Rank, y = airport.connects$Connect.Rank,labels = names, pos = 3, cex = .8, offset = .3)                      
barplot(airport.connects$Total.Flights, main = 'Total Flights Connecting the Midwest',xlab = "Airport", ylab = "# of Flights", names.arg = c('Ohr', 'Mid', 'Det', 'Col', 'Min','Cle','StL','Ind','Mil','Cin','KC','GR'),col = 'blue',horiz=F) 

mwflights.cnt <- count(mwflights$OBJECT_ID)
topflights <- filter(mwflights, OBJECT_ID %in% c('N4717C','MWT185', 'N2061A','N425TX'))
mapview(topflights)

#----------------Create Animation---------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(14269))

pa <- animate(p, nframes = 200, fps = 10, width = 1000, height = 500)

plot(p)

save_animation(pa, file = "flight.gif")