library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(fasterize)
setwd("~/git/WDPA/Script/protected_area")
wdpa<-st_read("../../RAW/WDPA_Jun2020-shapefile/WDPA_Jun2020-shapefile-polygons.shp")
wdpa$v<-1

wdpa_df<-as_tibble(wdpa)

wdpa_df%>%group_by(DESIG_TYPE)%>%count()

#wdpa_merge<-st_combine(wdpa)

mask<-raster("../../Raster/mask.tif")
res(mask)<-c(1e+4, 1e+4)

wdpa_moll<-st_transform(wdpa, crs=crs(mask))
st_write(wdpa_moll, "../../Shape/WDPA/WDPA_moll.shp")

for (type in unique(wdpa_moll$DESIG_TYPE)){
  print(type)
  sub_wdpa<-wdpa_moll[wdpa_moll$DESIG_TYPE==type,]
  r<-fasterize(sub_wdpa, mask)
  writeRaster(r, sprintf("../../Raster/WDPA/%s.tif", type), overwrite=T)
}


realms<-st_read("../../Shape/Biogeographic_realms/Biogeographic_realms_clip_moll.shp")
r<-fasterize(realms, mask, field="REALM_ID")
writeRaster(r, "../../Raster/Realms/realms.tif", overwrite=T)
plot(r)
