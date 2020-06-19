library(raster)
library(rgdal)
library(rgeos)
library(sf)
library(fasterize)
library(gdalUtils)
library(dplyr)

setwd("~/git/WDPA/Script/protected_area")
i=3
args = commandArgs(trailingOnly=TRUE)
i<-as.numeric(args[1])

groups<-c("Amphibians", "Birds", "Mammals", "Reptiles")

fs<-list.files(sprintf("../../Raster/IUCN_Range_By_Species/%s", groups[i]), pattern = "\\.tif$")
f<-fs[1]
realms<-raster("../../Raster/Realms/realms.tif")
china_pa<-raster("../../Raster/China_PAs/China_PAs.tif")
types<-c("International", "National", "Not Applicable", "Regional")
wdpa<-list()
for (type in types){
  wdpa[[type]]<-raster(sprintf("../../Raster/WDPA/%s.tif", type))  
}
sci_name="Ctenomys_pearsoni"
result<-NULL
for (f in fs){
  
  sci_name<-gsub("\\.tif", "", f)
  print(paste(sci_name, groups[i]))
  raster_f<-sprintf("../../Raster/IUCN_Range_By_Species/%s/%s.tif", groups[i], sci_name)
  rda_f<-sprintf("../../Object/Details/%s/%s.rda", groups[i], f)
  if (file.exists(rda_f)){
    r<-readRDS(rda_f)
    if (nrow(r)==0){
      next()
    }
  }else{
    r<-data.frame(rasterToPoints(raster(raster_f)))
    if (nrow(r)==0){
      saveRDS(r, rda_f)
      next()
    }
    for (type in types){
      r[, type]<-extract(wdpa[[type]], r[, c("x", "y")])
    }
    r$realm<-extract(realms, r[, c("x", "y")])
    r<-r[which(!is.na(r$realm)), ]
    if (nrow(r)==0){
      saveRDS(r, rda_f)
      next()
    }
    r[is.na(r)]<-0
    
    r$protected<-r$International+r$National+r$Regional
    
    dir.create(sprintf("../../Object/%s", groups[i]), showWarnings = F)
    saveRDS(r, rda_f)
  }
  if (!("china_pa" %in% colnames(r))){
    r$china_pa<-extract(china_pa, r[, c("x", "y")])
    r[is.na(r)]<-0
    r$protected<-r$International+r$National+r$Regional+r$china_pa
    saveRDS(r, rda_f)
  }
  
  protected<-nrow(r[which(r[, "protected"]>0), ])
  unprotected<-nrow(r)-protected
  item<-data.frame(sci_name=sci_name, protected=protected, unprotected=unprotected)
  item$china_pa<-nrow(r[which(r[, "china_pa"]>0), ])
  
  for (type in types){
    item[, type]<-nrow(r[which(r[, type]>0), ])
  }
  if (is.null(result)){
    result<-item
  }else{
    result<-bind_rows(result, item)
  }
}
saveRDS(result, sprintf("../../Object/Protected/%s.rda", groups[i]))