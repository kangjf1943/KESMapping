# Statement ----
# GIS analysis, including: visualize ES interpolation results from EBK; compare ES of different land use. 

# Package ----
library(sf)
library(terra)
library(tmap)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
showtext_auto()

# Read data ----
# land use
kLandUse <- c("ResLow", "ResHigh", "ResOther", "Ind", "ComNbr", "Com")

# ES *.tif data from EBK
kNameEs <- c("CS", "CSE", "NO2", "O3", "PM2.5", "SO2", "ROF")

ebk <- vector("list", 7)
names(ebk) <- kNameEs
for (i in kNameEs) {
  ebk[[i]] <- rast(paste0("GProcData/EBK/", i, ".tif"))
}

# land use *.shp data 
land.use <- st_read("GRawData/京都市用途地域_JGD2000.shp") %>% 
  rename("land_use" = "Land_class") %>% 
  mutate(land_use = case_when(
    land_use == "Com" ~ "Com", 
    land_use == "Com neigh" ~ "ComNbr", 
    land_use == "Ind" ~ "Ind", 
    land_use == "R high" ~ "ResHigh", 
    land_use == "R low" ~ "ResLow", 
    land_use == "R resi" ~ "ResOther", 
  ))
tm_shape(land.use) + tm_fill(col = "land_use")

# Analysis ----
## ES plots from EBK ----
# storage plots into list
ebk.plots <- vector("list", 7)
names(ebk.plots) <- kNameEs
for (i in kNameEs) {
  ebk.plots[[i]] <- 
    tm_shape(ebk[[i]]) + 
    tm_raster() + 
    tm_layout(legend.title.size = 1, legend.position = c("left", "bottom")) + 
    tm_compass(type = "8star", size = 1, position = c("left", "top")) + 
    tm_scale_bar(position = c("right", "bottom"))
}
# patchwork
# bug: 和QGIS图对比校核了图中颜色深浅代表的数值范围，但是和文稿中
tmap_arrange(ebk.plots)

## Total ES of land use ----
# Calculate the ES of each land use then compared with that of i-Tree output
# function: get total ES of each land use 
# note: extract ES interpolation results based on land use *.shp data, then calculate the sum of ES of the extracted data 
# variable: 
# x: name of land use type 
# es: name of ES
GetSumLanduse <- function(x, es) {
  outline <- subset(land.use, land_use == x) %>% 
    st_transform(crs(ebk[[es]]))
  mask(ebk[[es]], outline) %>% 
    global(fun = "sum", na.rm = TRUE) %>% 
    .[1, 1] %>% 
    return()
}

# calculate the total ES of each land use 
sum.landuse <- data.frame(land_use = kLandUse)
for (j in kNameEs) {
  es.sum <- c()
  for (i in kLandUse) {
    es.sum <- c(es.sum, GetSumLanduse(i, j))
  }
  sum.landuse[[j]] <- es.sum
}

# compare ES of land use types 
# bug: 和文稿中谢于松做的条形图有差别，比如谢图中ResLow最高，而我的图中ResOther最高
sum.landuse %>% 
  pivot_longer(cols = kNameEs, names_to = "es", values_to = "es_value") %>% 
  mutate(land_use = factor(land_use, levels = kLandUse)) %>% 
  ggplot() + 
  geom_bar(aes(land_use, es_value), stat = "identity") + 
  facet_wrap(.~ es, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90))

