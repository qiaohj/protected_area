library(raster)
library(rgdal)
library(rgeos)
library(sf)
library(fasterize)
library(gdalUtils)
library(dplyr)

setwd("~/git/WDPA/Script/protected_area")

i=1
args = commandArgs(trailingOnly=TRUE)
i<-as.numeric(args[1])

groups<-c("Amphibians", "Birds", "Mammals", "Odonata", "Reptiles")


dsn<-c("../../Shape/iucn_species_Ranges/AMPHIBIANS",
       "../../Shape/iucn_species_Ranges/Birds",
       "../../Shape/iucn_species_Ranges/MAMMALS1",
       "../../Shape/iucn_species_Ranges/Odonata",
       "../../Shape/iucn_species_Ranges/Reptiles")
#layer<-c("AMPHIBIANS_eck4", "BIRD_eck4", "MAMMALS_eck4", "Odonata_eck4", "Reptiles_eck4")
layer<-c("AMPHIBIANS", "All_Species", "MAMMALS", "data_0", "modeled_reptiles")

field_name<-c("binomial", "SCINAME", "binomial", "BINOMIAL", "Binomial")
fs<-list.files(sprintf("../../Shape/IUCN_SPECIES/%s", groups[i]), pattern = "\\.shp$")
f<-fs[1]
mask_bak<-raster("../../Raster/mask.tif")
result<-NULL
for (f in fs){
  sci_name<-gsub("\\.shp", "", f)
  
  target<-sprintf("../../Raster/IUCN_Range_By_Species/%s/%s", groups[i], gsub("\\.shp", "\\.tif", f))
  if (file.exists(target)){
    next()
  }
  dir.create(sprintf("../../Raster/IUCN_Range_By_Species/%s", groups[i]), showWarnings = F)
  mask<-mask_bak
  print(f)
  sp <- st_read(sprintf("../../Shape/IUCN_SPECIES/%s/%s", groups[i], f), quiet=T)
  sp_moll<-st_transform(sp, proj4string(mask_bak))
  item<-data.frame(sci_name=sci_name, area=as.numeric(st_area(sp_moll)))
  
  extent(mask)<-extent(sp_moll)
  res(mask)<-res(mask_bak)
  rp <- fasterize(sp_moll, mask)
  writeRaster(rp, target, overwrite=T)
  if (is.null(result)){
    result<-item
  }else{
    result<-bind_rows(result, item)
  }
}
saveRDS(result, sprintf("../../Objects/Species_Area/%s.rda", groups[i]))

if (F){
  result<-readRDS(sprintf("../../Objects/Species_Area/%s.rda", groups[4]))
  df<-result %>% group_by(sci_name) %>% summarise(sum=sum(area))
  library(ggplot2)
  ggplot(df, aes(x=sum)) + geom_histogram(aes(y = ..density..), bins=100) + 
    scale_x_log10()
}