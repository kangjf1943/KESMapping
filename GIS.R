# 概述 ----
# 用R语言进行GIS分析和制图，包括：做EBK各项ES插值结果栅格图并拼图；对各类ES，对比不同土地利用类型的ES差异

# 加载包 ----
library(sf)
library(terra)
library(tmap)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
showtext_auto()

# 数据 ----
# 土地利用类别
kLandUse <- c("ResLow", "ResHigh", "ResOther", "Ind", "ComNbr", "Com")

# 读取EBK插值的各项ES的结果栅格图
kNameEs <- c("CS", "CSE", "NO2", "O3", "PM2.5", "SO2", "ROF")
ebk <- vector("list", 7)
names(ebk) <- kNameEs
for (i in kNameEs) {
  ebk[[i]] <- rast(paste0("GProcData/EBK/", i, ".tif"))
}

# 读取土地利用矢量图
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

# 分析 ----
# EBK各ES插值结果作图并拼图
# 作图并且暂存到列表中
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
# 栅格地图拼图
# 漏洞：和QGIS图对比校核了图中颜色深浅代表的数值范围，但是和文稿中
tmap_arrange(ebk.plots)

# 计算EBK插值结果各项ES的和，并和i-Tree的推断结果比较
# 思路：基于各土地利用的矢量图剪切插值栅格结果，然后计算剪切后的栅格之和
# 函数：基于土地利用名称提取对应土地利用范围内插值栅格图的栅格数值之和
# 参数：
# x：土地利用名称
# es：生态系统服务名称
GetSumLanduse <- function(x, es) {
  outline <- subset(land.use, land_use == x) %>% 
    st_transform(crs(ebk[[es]]))
  mask(ebk[[es]], outline) %>% 
    global(fun = "sum", na.rm = TRUE) %>% 
    .[1, 1] %>% 
    return()
}

# 计算各类土地利用范围内的ES总值
sum.landuse <- data.frame(land_use = kLandUse)
for (j in kNameEs) {
  es.sum <- c()
  for (i in kLandUse) {
    es.sum <- c(es.sum, GetSumLanduse(i, j))
  }
  sum.landuse[[j]] <- es.sum
}

# 对比不同土地利用类型各类ES的差异
# 漏洞：和文稿中谢于松做的条形图有差别，比如谢图中ResLow最高，而我的图中ResOther最高
sum.landuse %>% 
  pivot_longer(cols = kNameEs, names_to = "es", values_to = "es_value") %>% 
  mutate(land_use = factor(land_use, levels = kLandUse)) %>% 
  ggplot() + 
  geom_bar(aes(land_use, es_value), stat = "identity") + 
  facet_wrap(.~ es, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90))

