#EDA with Snow Depth

library(raster)
library(rgdal)
library(tidyverse)
#WGS_1984_UTM_Zone_11N
#WKID: 32611 Authority: EPSG

snow_test <- raster("../Large Files/Snow Data/ASO 2014/MB20140406_SUPERsnow_depth_EXPORT.tif") # A 3m snow depth layer

pct_canopy <- raster("../Large Files/YNP_Illilouette2011/jts/Raster/Cover_aligned/e4101.tif")

dem <- raster("../Large Files/GIS/Topo/DEM.tif")

snow_crop <- raster::crop(snow_test,round(extent(pct_canopy)*0.9)) #0.9 adjusts for tilt in reprojection.
canopy_crop <- raster::crop(pct_canopy,round(extent(pct_canopy)*0.9))
dem_crop <- raster::crop(dem,round(extent(pct_canopy)*0.9))

plot(snow_crop)
plot(canopy_crop)
plot(dem_crop)

####1. Adjust for unreliable snow depths####
#Determine breakpoint for snow depth values, above which they are unreliable
graphics::hist(getValues(snow_crop),breaks=40)

v=getValues(snow_crop)
vc <- cut(v,
          breaks=seq(from=0,to=max(v),by=0.1),
          labels=seq(from=0.1,to=max(v),by=0.1)-0.05 
          )
vc <- as.numeric(as.character(vc))
vct <- table(vc)
for(e in 1:length(vct)){
  #print(names(vct[e]))
  if(e>2){
    print(as.numeric(vct[e]/vct[e-1]))
    if(as.numeric(vct[e]/vct[e-1]) > as.numeric(vct[e-1]/vct[e-2])){
      #When the decay rate stops accelerating, i.e. you aren't losing as many pixels in the next snow depth category as you would expect in a poisson-type distribution, that's the point at which we stop trusting the data.
      #Could eventually test this against a true poisson distribution.
      print(paste0("cut point is", names(vct[e])))
    }
  }
}
#Cutting at 1.4 m

new.v <- v
new.v[which(new.v>1.4)]=NA
snow_crop2 <- setValues(snow_crop,new.v)
plot(snow_crop2)

####2. Sample 3m landscape####
#START HERE; control for elevation in these figures and subsequent analyses.
#2a: Depth ~ CC
samples <- sample(x=c(1:length(new.v)), size= 1000, replace=F)
df <- data.frame(sample.ID=samples)
df$depth <- snow_crop2[samples]
df$CC <- canopy_crop[samples]

ggplot(df,aes(x=CC,y=depth))+
  geom_point()+
  geom_smooth()+
  labs(title="04-06-2014")

#2b: Depth ~ Distance to forest
binary.v <- getValues(canopy_crop)
binary.v[binary.v<0.25] <- NA
binary.v[!is.na(binary.v)] <- 1
canopy_crop_binary <- setValues(canopy_crop,binary.v)
canopy_crop_distance <- distance(canopy_crop_binary)
plot(canopy_crop_distance)

#samples.open <- sample(x=df[df$CC<0.2,"sample.ID"]) #Deprecate
which(!is.na(getValues(canopy_crop_binary)))
samples.open <- sample(x=which(is.na(getValues(canopy_crop_binary))), 
                  size= 1000, replace=F)
df <- data.frame(sample.ID=samples.open)
df$depth <- snow_crop2[samples.open]
df$CC <- canopy_crop[samples.open]
df$distance_to_edge <- canopy_crop_distance[samples.open]

ggplot(df,aes(x=distance_to_edge,y=depth))+
  geom_point()+
  geom_smooth()+
  #xlim(c(0,20))+
  labs(title="04-06-2014; Open areas only")