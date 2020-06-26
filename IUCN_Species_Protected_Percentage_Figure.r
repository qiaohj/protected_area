library(ggplot2)
library(viridis)
library(dplyr)
library(rayshader)
library(magick)
setwd("~/git/WDPA/Script/protected_area")
i=1
groups<-c("Amphibians", "Birds", "Mammals",  "Reptiles")

result<-NULL
table(df$china_pa)
for (i in c(1:length(groups))){
  df<-readRDS(sprintf("../../Object/Protected/%s.rda", groups[i]))
  df$group<-groups[i]
  if (is.null(result)){
    result<-df
  }else{
    result<-bind_rows(result, df)
  }
}

result$all_area<-result$protected+result$unprotected
result[which(result$all_area==0),]
result$percentile<-result$protected/result$all_area
write.csv(result, "../../Tables/PA_coverage.csv", row.names = F)


result<-read.csv("../../Tables/PA_coverage.csv", head=T, sep = ",")
ggplot(result, aes(percentile)) + geom_density()

ggdiamonds = ggplot(result, aes(percentile, all_area)) +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon", n = 100, bins = 100,contour = TRUE) +
  facet_wrap(group~.) +
  scale_fill_viridis_c(option = "A")
# \donttest{
plot_gg(ggdiamonds,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
        zoom = 0.55, phi = 30)
render_snapshot()


ggdiamonds = ggplot(diamonds, aes(x, depth)) +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon", n = 100, bins = 10,contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")
# \donttest{
plot_gg(ggdiamonds,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
        zoom = 0.55, phi = 30)
render_snapshot()


no_protected<-result%>%dplyr::filter(protected<=1)

no_protected%>%group_by(group)%>%count()
