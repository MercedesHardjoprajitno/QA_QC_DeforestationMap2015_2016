####################################################################################################
####################################################################################################
## Clip time series to desired boxes: Landsat time series, Sentinel, NDVI trend 
## Contact remi.dannunzio@fao.org
## 2016/12/19 -- Suriname CollectEarth exercise
####################################################################################################
####################################################################################################

# Options -----------------------------------------------------------------

# HELLO THIS IS REMI MESSING UP WITH YOUR SCRIPT
options(stringsAsFactors=FALSE)
library(Hmisc)
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(foreign)
library(rgeos)

##########################################################################################################################################################################
################# Directory and FILE : ADAPT TO YOUR CONFIGURATION
##########################################################################################################################################################################

# Parameters --------------------------------------------------------------

setwd("A:/WORK/WORKFOLDER/Deforestation_Monitoring/Deforestationmap2015_2016/QaQc")

#### Name of the directory where your Landsat data is
lsat_dir <- "A:/WORK/WORKFOLDER/Deforestation_Monitoring/Deforestationmap2015_2016/QaQc/Landsat/"

#### Name of the directory where your Sentinel data is
stnl_dir <- "A:/WORK/WORKFOLDER/Deforestation_Monitoring/Deforestationmap2015_2016/QaQc/Sentinel"


pts <- read.csv("A:/WORK/WORKFOLDER/Deforestation_Monitoring/Deforestationmap2015_2016/QaQc/aa_design_output/pts_CE_2017-05-17.csv")  #####  CHANGE TO MY VALUE HERE

head(pts)
names(pts)

map_code <- "map_class"
point_id <- "id"
xcoord   <- "XCoordinate"
ycoord   <- "YCoordinate"


#### Name of the directory where your data will be stored in output
dest_dir <-"A:/WORK/WORKFOLDER/Deforestation_Monitoring/Deforestationmap2015_2016/QaQc/Clip_Time_Series/"

#### Read shapefile of plots
## shp <- readOGR("A:/WORK/WORKFOLDER/Deforestation_Monitoring/Deforestationmap2015_2016/QaQc/shpfile_CE_2017-05-17.shp")###

shp <- readOGR(dsn="A:/WORK/WORKFOLDER/Deforestation_Monitoring/Deforestationmap2015_2016/QaQc", layer="shpfile_CE_2017-05-17")

names(shp)

# #### Path to your file point and set the parameters
map_code <- "Class"
point_id <- "ID"
xcoord   <- "xcoord"
ycoord   <- "ycoord"

# Preparation of files ----------------------------------------------------


##########################################################################################################################################################################
################## SCRIPT AUTOMATICALLY RUNS FROM HERE
##########################################################################################################################################################################

################## Read points from Design_App
pts <- gCentroid(shp,byid=TRUE)

################# Create spatial point file 
pt_df <- SpatialPointsDataFrame(
  coords = pts@coords,
  data   = data.frame(shp@data),
  proj4string=CRS(proj4string(shp))
)

pt_df_geo <- spTransform(pt_df,CRS("+init=epsg:4326"))

pts <- cbind(pt_df_geo@data,pt_df_geo@coords)

names(pts)

names(pts)[3] <- "xcoord"
 
names(pts)[4] <- "ycoord"

################ Create the index of the Landsat tiles
list_2000 <- list.files(lsat_dir,pattern="clip2010")
lp<-list()

for(file in list_2000){
  raster <- raster(paste(lsat_dir,file,sep=""))
  
  e<-extent(raster)
  
  poly <- Polygons(list(Polygon(cbind(
    c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
    c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
  )),file)
  lp <- append(lp,list(poly))
}

## Transform the list into a SPDF
lsat_idx <-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:length(lp)), 
  data.frame(list_2000), 
  match.ID = F
)

names(lsat_idx@data) <- "bb"
lsat_idx@data$bb <- substr(lsat_idx@data$bb,21,(nchar(lsat_idx@data$bb)-4))
lsat_idx@data

################ Create the index of the Sentinel tiles
list_s2 <- list.files(stnl_dir,pattern="s2_bb")
lp<-list()

for(file in list_s2){
  raster <- raster(paste(stnl_dir,file,sep=""))
  
  e<-extent(raster)
  
  poly <- Polygons(list(Polygon(cbind(
    c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
    c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
  )),file)
  lp <- append(lp,list(poly))
}

## Transform the list into a SPDF
stnl_idx <-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:length(lp)), 
  data.frame(list_s2), 
  match.ID = F
)

names(stnl_idx@data) <- "bb"
stnl_idx@data$bb <- substr(stnl_idx@data$bb,20,(nchar(stnl_idx@data$bb)-4))
stnl_idx@data

################# Project both into Lat-Lon EPSG:4326
proj4string(pt_df_geo) <- proj4string(lsat_idx) <- proj4string(stnl_idx) <- proj4string(spot_idx) <- CRS("+init=epsg:4326")


################# Intersect points with index of imagery and append ID's of imagery to data.frame
pts_lsat <- over(pt_df_geo,lsat_idx)
pts_stnl <- over(pt_df_geo,stnl_idx)
pts_spot <- over(pt_df_geo,spot_idx,returnList = T)

for(i in 1:nrow(pts)){
  print(i)
  df <- data.frame(pts_spot[i])
  names(df) <- names(spot_idx@data)
  
  df <- df[df$clou_num == min(df$clou_num),]
  df <- df[df$RESOLUTION == min (df$RESOLUTION),]
  
  if(i==1){
    spot_bb <- df$JOB[1]
    spot_rs <- df$RESOLUTION[1]
    spot_yr <- df$year[1]
    tmp_tp  <- unlist(strsplit(df$TYPE[1],split=" "))
    spot_tp <- tmp_tp[length(tmp_tp)]
    
  }else{
    spot_bb <- c(spot_bb,df$JOB[1])
    spot_rs <- c(spot_rs,df$RESOLUTION[1])
    spot_yr <- c(spot_yr,df$year[1])
    tmp_tp  <- unlist(strsplit(df$TYPE[1],split=" "))
    spot_tp <- c(spot_tp, tmp_tp[length(tmp_tp)])
    }
}

pts<-cbind(pts,pts_lsat$bb)
pts<-cbind(pts,pts_stnl$bb)
pts<-cbind(pts,spot_bb)
pts<-cbind(pts,spot_yr)
pts<-cbind(pts,spot_rs)
pts<-cbind(pts,spot_tp)


################# Create the outside boundaries box (1km // twice 500m from center of box)
lp<-list()
ysize <- 1000/111321

## Loop through all points
for(i in 1:nrow(pts)){
  ymin <- pts[i,ycoord]-ysize
  ymax <- pts[i,ycoord]+ysize
  xmin <- pts[i,xcoord]-ysize*cos(pts[1,ycoord]*pi/180)
  xmax <- pts[i,xcoord]+ysize*cos(pts[1,ycoord]*pi/180)
  
  p  <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
  ps <- Polygons(list(p), pts[i,point_id])
  lp <- append(lp,list(ps))
}

## Transform the list into a SPDF
outbox<-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:nrow(pts)), 
  pts[,c(map_code,point_id,xcoord,ycoord)], 
  match.ID = F
)


## Reproject the shapefile of the interpretation plots
inbox <- spTransform(shp,CRS("+init=epsg:4326"))

proj4string(inbox) <- proj4string(outbox) <- CRS("+init=epsg:4326")
#head(pts[!is.na(pts[,"pts_stnl$bb"]),])

head(pts)
#####################################################################################
#####################################################################################
#####################################################################################

# Loop through all IDs ----------------------------------------------------


################ Create the list of ID's to process
list_ids <- pts[,point_id]

listdone <- list() 
listdone <- read.table(text=list.files("clip_time_series_new/"),as.is=T,fill=T,sep="_")[,2]

listodo  <- list_ids[!(list_ids %in% listdone)]


######################################################################################################
################# Loop through the IDs
##     the_id = "3"
dev.off()
for(the_id in listodo){
  
  ####################################################################
  ################# Open the image output file
  #out_name <- paste(dest_dir,"pt_",the_id,"_",pts[pts[,point_id]==the_id,map_code],".png",sep="")
  out_name <- paste(dest_dir,"pt_",the_id,"_","blind",".png",sep="")
  png(file=out_name,width=2000,height=1600)
  
  ## Check which point is being processed
  (the_pt <- pts[pts[,point_id]==the_id,])
  
  ####################################################################
  ##### Delimitations of the plot in geographic coordinates
  one_poly <- outbox[outbox@data[,point_id]==the_id,]
  in_poly  <-   inbox[inbox@data[,point_id]==the_id,]
  
  margins <- extent(
    one_poly@bbox["x","min"]-100/111321,
    one_poly@bbox["x","max"]+100/111321,
    one_poly@bbox["y","min"]-100/111321,
    one_poly@bbox["y","max"]+100/111321)

  ####################################################################
  ##### Delimitations of the plot in local coordinates
  
  one_poly_utm <- spTransform(one_poly,CRS("+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  in_poly_utm  <-  spTransform(in_poly,CRS("+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  margins_utm <- extent(
    one_poly_utm@bbox["x","min"]-100,
    one_poly_utm@bbox["x","max"]+100,
    one_poly_utm@bbox["y","min"]-100,
    one_poly_utm@bbox["y","max"]+100)
  
  ####################################################################
  ################# Find the corresponding indexes
  lsat_bbox <- the_pt[,"pts_lsat$bb"]
  stnl_bbox <- the_pt[,"pts_stnl$bb"]
  spot_bbox <- the_pt[,"spot_bb"]
  spot_year <- the_pt[,"spot_yr"]
  spot_type <- the_pt[,"spot_tp"]
  
  ################# Set the layout
  #dev.off()
  ## The export image will be in a 4 (height) x 5 (width) grid box
  par(mfrow = c(4,5))
  par(mar=c(1,0,1,0))

  ndvi_trend <- data.frame(matrix(nrow=0,ncol=2))
  names(ndvi_trend) <- c("year","mean")
  i <- 1
  
  ####################################################################
  ################# Clip the landsat time series
  for(year in c(2000:2015)){
    print(year)
    plot(margins,axes=F,xlab="",ylab="")
    tryCatch({
      lsat <- brick(paste(lsat_dir,"median_hul_clip",year,"_",lsat_bbox,".tif",sep=""))
      lsat_clip<-crop(lsat,one_poly)
      
      swir <- raster(lsat_clip,4)
      nir  <- raster(lsat_clip,3)
      red  <- raster(lsat_clip,2)
      #green<- raster(lsat_clip,1)
      ndvi <- (nir-red)/(nir+red)
      #nbr  <- (nir-swir)/(nir+swir)
      
      ndvi_trend[i,]$year <- year 
      ndvi_trend[i,]$mean <- cellStats(crop(ndvi,in_poly),stat='mean')
      i <- i + 1
      
      #Plot natural colours composite (SWIR-NIR-RED == 4-5-3 in L7 nomenclature)
      stack <- stack(swir,nir,red)
      plotRGB(stack,stretch="hist",add=T)
    },error=function(e){cat("Configuration impossible \n")})
    
    lines(in_poly,col="red",lwd=2)
    title(main=paste("landsat_",year,sep=""),font.main=1200)
    
  }
  
  
  

  
  ####################################################################
  ################# Clip the sentinel tile 
  plot(margins,axes=F,xlab="",ylab="")
  the_pt
  tryCatch({
    stnl <- brick(paste(stnl_dir,"median_hul_clip_s2_",stnl_bbox,".tif",sep=""))
    stnl_clip<-crop(stnl,one_poly)
    
    blu <- raster(stnl_clip,1)
    grn <- raster(stnl_clip,2)
    red <- raster(stnl_clip,3)
    #nir <- raster(stnl_clip,4)
    
    #ndvi <- (nir-red)/(nir+red)
    
    R <- red
    G <- grn
    B <- blu
    
    stackNat <- stack(R,G,B)
    #stackVeg <- stack(nir,ndvi,grn)
    #stackNIR <- stack(nir,red,grn)
    
    plotRGB(stackNat,stretch="hist",add=T)
    
    
  },error=function(e){cat("Configuration impossible \n")})
  lines(in_poly,col="red",lwd=2)
  #plot(in_poly,add=T,col="red")
  
  title(main="sentinel_2016",font.main=200)
  
  ####################################################################
  ################# function to all pixel stack 
  
  par(mar=c(2,2,2,2))
  plot(ndvi_trend,
       # yaxt='n',
       # xaxt='n',
       xlab="year",
       ylab="",
       ylim=c(0,1)
  )
  
  title(main="mean ndvi",font.main=200)
  
  ####################################################################
  ### Close the image file
  dev.off()
  
  
  ####################################################################
  ### End the points loop
}


