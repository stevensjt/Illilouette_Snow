#EDA with Snow Depth

library(raster)
library(rgdal)
#WGS_1984_UTM_Zone_11N
#WKID: 32611 Authority: EPSG

snow_test <- raster("../Large Files/Snow Data/ASO 2014/MB20140406_SUPERsnow_depth_EXPORT.tif") # A 3m snow depth layer

pct_canopy <- raster("../Large Files/YNP_Illilouette2011/jts/Raster/Cover_aligned/e4101.tif")

hist(snow_test)


snow_crop <- raster::crop(snow_test,round(extent(pct_canopy)*0.9)) #0.9 adjusts for tilt in reprojection.
canopy_crop <- raster::crop(pct_canopy,round(extent(pct_canopy)*0.9))

plot(snow_crop)
plot(canopy_crop)

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

