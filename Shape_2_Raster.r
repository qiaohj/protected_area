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


china_pa<-st_read("../../RAW/China_PAs/China_pas.shp")
china_pa_moll<-st_transform(china_pa, crs=crs(mask))
colnames(china_pa_moll)[c(13,14,16:23)]<-paste("c", c(13,14,16:23), sep="_")
st_write(china_pa_moll, "../../Shape/WDPA/China_PAs_moll.shp",delete_layer = TRUE)

unique(china_pa_moll$c_14)
r<-fasterize(china_pa_moll[which(!is.na(china_pa_moll$c_14)),], mask)
writeRaster(r, "../../Raster/China_PAs/China_PAs.tif", overwrite = TRUE)
plot(r)
