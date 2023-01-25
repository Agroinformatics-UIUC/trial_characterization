library("rgdal")
library("sp")
library("raster")
library("tmap")
library("maptools")
library("dplyr")
library("Hmisc")
library("sf")

# Define information to be inserted into file names below 
farm<-"sommer"
field<-"uofiaddington1"

# Name what files will be read # Identify whether the shapefile for this 
#variable is available for this field
preplant<-"no"
asapplied<-"yes"



setwd("C:/Users/gmandrini/Downloads/sommer")

# Read shapefiles and remove extra variables from dataframes #
x<-c(farm,field,"ec",2017)
file<-paste(x, collapse="_")
ec <- readOGR(".", file)
#identify column of ec
ec.dt<-data.table::as.data.table(ec)
ec<-ec[,c(5,7)]
names(ec@data)[1] <- "ecs"
names(ec@data)[2] <- "ecd"

x<-c(farm,field,"trialyield",2017)
file<-paste(x, collapse="_")
yield <- readOGR(".", file)
#identify column of dry yield
yield.dt<-data.table::as.data.table(yield)
yield<-yield[,4]
names(yield@data)[1] <- "yield"


x<-c(farm,field,"asplanted",2017)
file<-paste(x, collapse="_")
asplanted <- readOGR(".", file)
#idetify columns of seed and elevation
asplanted.dt<-data.table::as.data.table(asplanted)
View(asplanted@data)
asplanted <-asplanted[,c(4)]
#names(asplanted@data)[1] <- "elevation"
names(asplanted@data)[1] <- "seed"

x<-c(farm,field,"elevationec",2017)
file<-paste(x, collapse="_")
elevation <- readOGR(".", file)
#idetify columns of seed and elevation
names(elevation@data)
elevation <-elevation[,c(4)]
names(elevation@data)[1] <- "elevation"



if (preplant=="yes") {
  x<-c(farm,field,"preplant",2017)
  file<-paste(x, collapse="_")
  preplant <- readOGR(".", file)
}

x<-c(farm,field,"asapplied",2017)
file<-paste(x, collapse="_")
asapplied <- readOGR(".", file)
#identify column for nitrogen applied
names(asapplied@data)
asapplied<-asapplied[,4]
names(asapplied@data)[1] <- "nitrogen"

# topography
x<-c(farm,field,"slopeandaspect")
file<-paste(x, collapse="_")
topography <- readOGR(".", file)

x<-c(farm,field,"trialdesign",2017)
file<-paste(x, collapse="_")
appmap <-  readOGR(".",file)
# Define the UTM zone #
sp::proj4string(appmap) <- sp::CRS('+proj=longlat +datum=WGS84')

#Calculate the utm zone (formula from wikipedia)
utmzone <- floor(((mean(bbox(appmap)[1,]) + 180) / 6) %% 60) + 1

# Convert to UTM #
appmap <- sp::spTransform(appmap,sp::CRS(paste0('+proj=utm +zone=',utmzone,' ellps=WGS84' )))

# Remove Partial Blocks and Blocks without Treatment #
appmap$area <- area(appmap)
appmap$treat_type<-ifelse(appmap$area<=2000,17,appmap$treat_type)
appmap <- appmap[appmap$treat_type !=  17, ]
appmap$CLIENT <- NULL
appmap$FARM <- NULL
appmap$FIELD <- NULL
appmap$Tgt_Nrate <- appmap$NRATE_Gal3
appmap$Tgt_seedrate <- appmap$SEEDRATE
appmap$NRATE_Gal3 <- NULL
appmap$SEEDRATE <- NULL

# Create new plotid's #
appmap$new_plotid <- seq.int(nrow(appmap))
#appmap <- appmap[c("new_plotid","plotid","cent_x","cent_y","treat_type", "Tgt_Nrate","Tgt_seedrate","POLYGONTYP","geometry")]

# Looping through to design new plots #
# Find coordinates of the plots #
appmap1 <- subset(appmap, new_plotid == 1)
coords1 <- appmap1@polygons[[1]]@Polygons[[1]]@coords #Grabs the long and lat in a 5x2 matrix.

# Find the angle of the 
ror1 <- (coords1[4,2]-coords1[1,2])/(coords1[4,1]-coords1[1,1])
angle1 <- atan(ror1)
sin1 <- sin(angle1)
cos1 <- cos(angle1)
# 30 ft= 9.144 meters #
move <- c(9.144*cos1,9.144*sin1)

gsw1 <- coords1[1,]+move
gnw1 <- coords1[2,]+move
gne1 <- coords1[3,]-move
gse1 <- coords1[4,]-move

coords1_reduced <- rbind(t(gsw1),t(gnw1),t(gne1),t(gse1),t(gsw1))
coords1.dt <- data.table::as.data.table(coords1)
coords1_reduced.dt <- data.table::as.data.table(coords1_reduced)
###################Divide the reduced plot into four equally-sized partial plots.##########

#Find length of reduced plot, in meters:
plotlength1 <- rbind(gnw1,gne1)
subplotlength1 <- as.numeric(dist(plotlength1))/4

go <- c(subplotlength1*cos1, subplotlength1*sin1)

#Take first two vertices of the subplot, then create two more vertices that are used to make the next subplot.
#Subplots have to be 25% as long as the reduced plot.
coordssubplot1_1 <- rbind(coords1_reduced[1:2,],(coords1_reduced[2:1,]+rbind(go,go)),coords1_reduced[1,])
coordssubplot1_1.dt <- data.table::as.data.table(coordssubplot1_1)

coordssubplot1_2 <- rbind(coordssubplot1_1[4:3,],(coordssubplot1_1[3:4,]+rbind(go,go)),coordssubplot1_1[4,])
coordssubplot1_2.dt <- data.table::as.data.table(coordssubplot1_2)

coordssubplot1_3 <- rbind(coordssubplot1_2[4:3,],(coordssubplot1_2[3:4,]+rbind(go,go)),coordssubplot1_2[4,])
coordssubplot1_3.dt <- data.table::as.data.table(coordssubplot1_3)

coordssubplot1_4 <- rbind(coordssubplot1_3[4:3,],(coordssubplot1_3[3:4,]+rbind(go,go)),coordssubplot1_3[4,])
coordssubplot1_4.dt <- data.table::as.data.table(coordssubplot1_4)

# Create polygons #
poly1_1<-Polygon(coordssubplot1_1)
poly1_2<-Polygon(coordssubplot1_2)
poly1_3<-Polygon(coordssubplot1_3)
poly1_4<-Polygon(coordssubplot1_4)

# Put polygons in a list #
list1<-Polygons(list(poly1_1,poly1_2,poly1_3,poly1_4),"1")
plots<-SpatialPolygons(list(list1))
polyproj <- CRS(paste0('+proj=utm +zone=',utmzone,' ellps=WGS84'))
proj4string(plots) <- polyproj

plotid <- unique(appmap@data$new_plotid)
# Loop through all plots taking off ends, splitting into 4 plots, and rbinding to the first polygon
for (i in plotid[-1]) {
  # Find coordinates of the plots #
  appmap1 <- subset(appmap, new_plotid == i)
  coords1 <- appmap1@polygons[[1]]@Polygons[[1]]@coords #Grabs the long and lat in a 5x2 matrix.
  
  # Find the angle of the 
  ror1 <- (coords1[4,2]-coords1[1,2])/(coords1[4,1]-coords1[1,1])
  ror1
  angle1 <- atan(ror1)
  angle1
  
  sin1 <- sin(angle1)
  cos1 <- cos(angle1)
  # 30 ft= 9.144 meters #
  move <- c( 9.144*cos1, 9.144*sin1   )
  
  gsw1 <- coords1[1,]+move
  gnw1 <- coords1[2,]+move
  gne1 <- coords1[3,]-move
  gse1 <- coords1[4,]-move
  
  coords1_reduced <- rbind(t(gsw1),t(gnw1),t(gne1),t(gse1),t(gsw1))
  coords1_reduced
  
  coords1.dt <- data.table::as.data.table(coords1)
  coords1_reduced.dt <- data.table::as.data.table(coords1_reduced)
  
  ###################Divide the reduced plot into four equally-sized partial plots.##########
  
  #Find length of reduced plot, in meters:
  plotlength1 <- rbind(gnw1,gne1)
  subplotlength1 <- as.numeric(dist(plotlength1))/4
  
  go <- c( subplotlength1*cos1, subplotlength1*sin1   )
  
  #Take first two vertices of the subplot, then create two more vertices that are used to make the next subplot.
  #Subplots have to be 25% as long as the reduced plot.
  coordssubplot1_1 <- rbind(coords1_reduced[1:2,],(coords1_reduced[2:1,]+rbind(go,go)),coords1_reduced[1,])
  coordssubplot1_1.dt <- data.table::as.data.table(coordssubplot1_1)
  
  coordssubplot1_2 <- rbind(coordssubplot1_1[4:3,],(coordssubplot1_1[3:4,]+rbind(go,go)),coordssubplot1_1[4,])
  coordssubplot1_2.dt <- data.table::as.data.table(coordssubplot1_2)
  
  coordssubplot1_3 <- rbind(coordssubplot1_2[4:3,],(coordssubplot1_2[3:4,]+rbind(go,go)),coordssubplot1_2[4,])
  coordssubplot1_3.dt <- data.table::as.data.table(coordssubplot1_3)
  
  coordssubplot1_4 <- rbind(coordssubplot1_3[4:3,],(coordssubplot1_3[3:4,]+rbind(go,go)),coordssubplot1_3[4,])
  coordssubplot1_4.dt <- data.table::as.data.table(coordssubplot1_4)
  
  # Create polygons #
  poly1_1<-Polygon(coordssubplot1_1)
  poly1_2<-Polygon(coordssubplot1_2)
  poly1_3<-Polygon(coordssubplot1_3)
  poly1_4<-Polygon(coordssubplot1_4)
  
  # Put polygons in a list #
  list1<-Polygons(list(poly1_1,poly1_2,poly1_3,poly1_4),"1")
  plot1<-SpatialPolygons(list(list1))
  polyproj <- CRS(paste0('+proj=utm +zone=',utmzone,' ellps=WGS84'))
  proj4string(plot1) <- polyproj
  plots <- rbind(plots,plot1,makeUniqueIDs=TRUE)
}
plot(plots)

# Transform into WGS84 #
subplots <- spTransform(plots, CRS("+proj=longlat +datum=WGS84"))
plot(subplots)


#Let us aggregate variables inside each plot
#Yield
proj4string(yield)<- CRS("+proj=longlat +datum=WGS84")
merge1<-sp::over(subplots,yield,fn=median)
subplots<-SpatialPolygonsDataFrame(subplots, merge1, match.ID = FALSE)


#ec
proj4string(ec)<- CRS("+proj=longlat +datum=WGS84")
merge2<-sp::over(subplots,ec,fn=median)
subplots@data<-cbind(merge2,subplots@data)

#asplanted and elevation
proj4string(asplanted)<- CRS("+proj=longlat +datum=WGS84")
merge3<-sp::over(subplots,asplanted,fn=median)
subplots@data<-cbind(merge3,subplots@data)

#preplant
if (preplant=="yes") {
proj4string(preplant)<- CRS("+proj=longlat +datum=WGS84")
merge4<-sp::over(subplots,preplant,fn=median)
subplots@data<-cbind(merge4,subplots@data)
}

#asapplied
proj4string(asapplied)<- CRS("+proj=longlat +datum=WGS84")
merge5<-sp::over(subplots,asapplied,fn=mean)
subplots@data<-cbind(merge5,subplots@data)

#topography
topo <- spTransform(topography, CRS("+proj=longlat +datum=WGS84"))
slopemerge <- sp::over(subplots,topo,fn=median)
subplots@data <-cbind(slopemerge,subplots@data)

#save the new shapefile
x<-c(farm,field,"cleanandmerged","2017.shp")
file<-paste(x, collapse="_")
shapefile(subplots, file,overwrite=TRUE)

