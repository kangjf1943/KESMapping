# Statement ----
# The analysis process of this research includes 3 parts: preparation of basic point GIS data of ecosystem service (ES) for interpolation; GIS interpolation to mapping ES; analysis of the results and discussion in urban planning context. 

# First part: 
# The ES-related investigation data of each quadrat was stored in tables. I firstly joined this data to the sample quadrat GIS point layer, so that GIS can access the data. The result layers will be sent to Xie so that he can apply multiple interpolation methods to the results to mapping the ES spatial pattern of Kyoto City. 

# Second part: 
# GIS interpolation was applied by Xie using ArcGIS. Then the interpolation GIS results were sent to me for model validation by an array of errors. With the validation, we can pick up the best interpolation method which has the lowest errors. 

# Third part: 
# Analysis of the results, including: calculating tree attributes of each land use; calculating ES values of each land use; the relationship between ES spatial pattern and the other urban planning layers, e.g., parks, infrastructures. 

# Package ----
library(openxlsx)
library(units)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shapefiles)
library(sf)
library(terra)
library(tmap)
library(vegan)
library(dunn.test)

# Setting ----
install_unit("yen")
install_unit("dollar", "109 yen")
install_unit("quadrat", "400 m2")

# Function ----
# function: get total certain ESV of all land use types
# note: extract ES interpolation results based on land use *.shp data, then calculate the sum of ES of the extracted data 
# arguement: 
# x: a sf containing ESV data
# es: name of target ES
SumESVLanduse <- function(x, es) {
  result <- numeric(length = length(kLanduse))
  names(result) <- kLanduse
  for (i in kLanduse) {
    result[i] <- x %>% 
      mask(., subset(land.use, land_use == i)) %>% 
      global(fun = "sum", na.rm = TRUE) %>% 
      .[1, 1]
  }
  return(result)
}

# function: read *.dbf file based on file name
# note: Since the interpolation results are stored in the directories in this manner: InterpolationMethodName > EcosystemServiceName, I will use the name of the interpolation method and the name of ES as the variable, though they should be the directory name and file name. This function reads the *.dbf table, then add colnumns of the directory (interpolation method) and file name (ES). 
# variable: 
# intrpl: interpolation method name, which is actually directory of folder containing target *.dbf
# es: name of the target ES, which is actually the *.dbf file name
ReadDbf <- function(es, intrpl) {
  read.dbf(paste0("RawData/Validation/", intrpl, "/", es, ".dbf")) %>% 
    .$dbf %>% 
    as_tibble() %>%  
    rename_with(tolower) %>%
    select(measured, predicted) %>% 
    mutate(intrpl = intrpl, es = es, .before = measured)
}

# Data ----
## Constant ----
# rate of Japanese Yen and US dollar: average rate of 2019
kUsdJpy <- set_units(109, yen/dollar)

# ecosystem service items 
kES <- data.frame(
  # ES names in this code
  es = c("carbon_stor", "carbon_seq", 
         "no2_rem", "o3_rem", "pm25_rem", "so2_rem", "ro_rd"), 
  # ES names of EBK interpolation result layers from Xie
  abbr = c("CS", "CSE", 
             "NO2", "O3", "PM2.5", "SO2", "RR")
) %>% 
  mutate(esv = paste0(es, "_val"))

# land use type
kLanduse <- 
  c("ResLow", "ResHigh", "ResOther", "Ind", "Com", "ComNbr")

## Quadrat information ----
qua.info <- 
  read.xlsx("RawData/KUP_Plot_info.xlsx", sheet = "样方信息") %>% 
  rename_with(tolower) %>% 
  rename(kes_qua_id = kes_plot_id, ward = ward_en) %>% 
  select(qua_id, kes_qua_id, landuse_class, ward) %>% 
  rename(landuse = landuse_class) %>% 
  tibble() %>% 
  # change names of land use column
  mutate(landuse = case_when(
    landuse == "Com" ~ "Com", 
    landuse == "Com-neigh" ~ "ComNbr", 
    landuse == "Ind" ~ "Ind", 
    landuse == "R-high" ~ "ResHigh", 
    landuse == "R-low" ~ "ResLow", 
    landuse == "R-other" ~ "ResOther", 
  ))

## GIS data ----
# quadrat point layer 
quadrat <- 
  st_read(dsn = "RawData/SampleQuadrat", layer = "Sample_plot") %>% 
  # bug: can not rename it directly? 
  # rename_with(qua_id = Qua_id) %>% 
  select(Qua_id, geometry) %>% 
  st_transform(6668) %>% 
  rename(qua_id = Qua_id)

# ecosystem services interpolation by EBK
ebk <- vector("list", 7)
names(ebk) <- kES$abbr
for (i in kES$abbr) {
  ebk[[i]] <- rast(paste0("RawData/ESEBK/", i, ".tif")) %>% 
    project("EPSG:6668")
}
# add units: here "quadrat" is area of each quadrat
names(ebk) <- 
  paste(names(ebk), c(rep("kg/quadrat", 2), rep("g/quadrat", 4), "m3/quadrat"))

# ecosystem services values interpolation by EBK
# 漏洞：这个数据应该是用服务量乘以单价算出来的？
esv.ebk <- vector("list", 7)
names(esv.ebk) <- kES$abbr
for (i in kES$abbr) {
  esv.ebk[[i]] <- rast(paste0("RawData/ESVEBK/", i, ".tif")) %>% 
    project("EPSG:6668")
}

# land use *.shp data 
land.use <- 
  st_read("RawData/LandUse/京都市用途地域_JGD2000.shp") %>% 
  rename("land_use" = "Land_class") %>% 
  mutate(land_use = case_when(
    land_use == "Com" ~ "Com", 
    land_use == "Com neigh" ~ "ComNbr", 
    land_use == "Ind" ~ "Ind", 
    land_use == "R high" ~ "ResHigh", 
    land_use == "R low" ~ "ResLow", 
    land_use == "R resi" ~ "ResOther", 
  )) %>% 
  st_transform(6668)

## Species information ----
species.info <- read.csv("RawData/Plant_info.csv") %>% 
  tibble() %>% 
  rename_with(tolower) %>% 
  rename(species = species_lt)

## Individual tree attr and ES ----
# including ES, investigation data, and quadrat information 
indv.data <- 
  # ES part: raw data from Access database from Hirabayashi 
  read.xlsx("RawData/Output_i_tree.xlsx", sheet = "Trees") %>% 
  tibble() %>% 
  rename(tree_id = "TreeID", 
         dbh = "DBH.(CM)", 
         height = "HEIGHT.(M)", 
         lai = "LEAF.AREA.INDEX", 
         carbon_stor = "CARBON.STORAGE.(KG)", 
         carbon_seq = "GROSS.CARBON.SEQ.(KG/YR)", 
         no2_rem = "NO2.Removal.(g)", 
         o3_rem = "O3.Removal.(g)", 
         pm25_rem = "PM25.Removal.(g)", 
         so2_rem = "SO2.Removal.(g)", 
         no2_rem_val = "NO2.Value.($)", 
         o3_rem_val = "O3.Value.($)", 
         pm25_rem_val = "PM25.Value.($)",          
         so2_rem_val = "SO2.Value.($)", 
         ro_rd = "Avoided.Runoff.(m3)") %>% 
  # add units
  mutate(
    dbh = set_units(dbh, cm), 
    height = set_units(height, m), 
    lai = set_units(lai, 1), 
    carbon_stor = set_units(carbon_stor, kg), 
    carbon_seq = set_units(carbon_seq, kg), 
    no2_rem = set_units(no2_rem, g), 
    o3_rem = set_units(o3_rem, g), 
    pm25_rem = set_units(pm25_rem, g), 
    so2_rem = set_units(so2_rem, g), 
    no2_rem_val = set_units(no2_rem_val, dollar), 
    o3_rem_val = set_units(o3_rem_val, dollar), 
    pm25_rem_val = set_units(pm25_rem_val, dollar),          
    so2_rem_val = set_units(so2_rem_val, dollar), 
    ro_rd = set_units(ro_rd, m3)
  ) %>% 
  # order by res_tree_id so that it can be bind with investigation data 
  arrange(tree_id) %>% 
  mutate(
    # turn monetary value from Japanese Yen to US dollar
    # calculate carbon monetary value and turn to US dollar: the carbon price in Japan is 10600 JPY/ton carbon = 10.6 JPY/kg carbon
    carbon_stor_val = 10.6 * carbon_stor / kUsdJpy,
    carbon_seq_val = 10.6 * carbon_seq / kUsdJpy, 
    # calculate runoff reduction value and turn to US dollars: runoff reduction value in Japan is 719 JPY/cubic meter rainwater
    ro_rd_val = 719 * ro_rd / kUsdJpy
  ) %>% 
  # investigation data part
  # bug: assume the i-Tree input data, output data, and original investigation data are of the same order
  cbind(
    read.csv("RawData/Plant_data.csv") %>% 
      tibble() %>% 
      rename_with(tolower) %>% 
      rename(species = species_lt) %>% 
      subset(tree_shrub == "tree") %>% 
      arrange(plot_id, plant_id)
  ) %>% 
  tibble() %>% 
  # match qua_id (Quadrat id of previous Kyoto plant diversity project) and kes_qua_id (quadrat ID of previous Kyoto urban ecosystem services project)
  left_join(
    read.csv("RawData/Input_i_tree.csv") %>% 
      rename(tree_id = ID, 
             kes_qua_id = PlotId) %>% 
      select(kes_qua_id, tree_id), 
    by = "tree_id"
  ) %>% 
  # add quadrat information part
  left_join(qua.info, by = "kes_qua_id") %>% 
  # add species information 
  left_join(species.info, by = "species") %>% 
  # get target variables 
  select(
    # basic information 
    qua_id, tree_id, ward, landuse, species, genus, family, 
    # tree attribute 
    dbh, height, lai, 
    # ES and ES value
    all_of(kES$es), all_of(kES$esv)
  )

## Quadrat tree attr and ES ----
qua.data <- indv.data %>% 
  group_by(qua_id, landuse) %>% 
  summarise(
    richness = length(unique(species)), 
    abundance = n(), 
    # mean and median values of tree attributes
    dbh_mean = mean(dbh), 
    dbg_mid = median(dbh), 
    height_mean = mean(height), 
    height_mid = median(height), 
    lai_mean = mean(lai), 
    lai_mid = median(lai), 
    # sum of ES and revise unit
    across(all_of(kES$es), sum), 
    across(all_of(kES$es), drop_units)
  ) %>% 
  ungroup() %>% 
  mutate(
    carbon_stor = set_units(carbon_stor, kg/quadrat), 
    carbon_seq = set_units(carbon_seq, kg/quadrat), 
    no2_rem = set_units(no2_rem, g/quadrat), 
    o3_rem = set_units(o3_rem, g/quadrat), 
    pm25_rem = set_units(pm25_rem, g/quadrat), 
    so2_rem = set_units(so2_rem, g/quadrat), 
    ro_rd = set_units(ro_rd, m3/quadrat)
  ) %>% 
  # join quadrat ecosystem service results to quadrat layer
  left_join(quadrat, by = "qua_id") %>% 
  st_as_sf() %>% 
  # remove Z value in coord
  st_zm()

## Land use tree attr and ES ----
lu.data <- left_join(
  # tree attribute of each land use, generally mean or median of all trees
  indv.data %>% 
    group_by(landuse) %>% 
    summarise(
      qua_num = n_distinct(qua_id), 
      # 漏洞：照理来说应该用物种累计曲线推算richness
      richness = length(unique(species)), 
      # 由于样方数量差异，abundance应为植株密度
      density = n() / qua_num, 
      dbh_mean = mean(dbh), 
      dbh_mid = median(dbh), 
      height_mean = mean(height), 
      height_mid = median(height), 
      lai_mean = mean(lai), 
      lai_mid = median(lai)
    ), 
  # total ESV of each land use generated by EBK
  lapply(esv.ebk, SumESVLanduse) %>% 
    do.call(rbind, .) %>% 
    t() %>% 
    data.frame() %>% 
    rename(
      carbon_stor_val = CS, 
      carbon_seq_val = CSE, 
      no2_rem_val = NO2, 
      o3_rem_val = O3, 
      pm25_rem_val = PM2.5, 
      so2_rem_val = SO2, 
      ro_rd_val = RR
    ) %>% 
    mutate(landuse = rownames(.), .before = ), 
  by = "landuse"
) %>% 
  # add units to ESV
  mutate(
    carbon_stor_val = set_units(carbon_stor_val, dollar), 
    carbon_seq_val = set_units(carbon_seq_val, dollar), 
    no2_rem_val = set_units(no2_rem_val, dollar), 
    o3_rem_val = set_units(o3_rem_val, dollar), 
    pm25_rem_val = set_units(pm25_rem_val, dollar),          
    so2_rem_val = set_units(so2_rem_val, dollar), 
    ro_rd_val = set_units(ro_rd_val, dollar)
  ) %>% 
  # reorder land use
  mutate(landuse = factor(landuse, levels = kLanduse))

# Analysis ----
## First part ----
# preparation for interpolation
# visualization 
tm_shape(land.use) + 
  tm_polygons(col = "land_use") +
  tmap_options(check.and.fix = TRUE) + 
  tm_shape(qua.data) + 
  tm_dots()
# export the *.shp file and send it to Xie
st_write(qua.data, "ProcData/Qua_es.shp")
# bug: warning messege, "abbreviate_shapefile_names(obj) で: Field names abbreviated for ESRI Shapefile driver". It doesn't matter though ... really should stop using shapefile format. 

## Second part ----
### GIS interpolation ----
# Xie do that manually in GIS
# bug: need to indicate what data should be obtained from Xie's operation 

### Interpolation validation ----
val.base <- 
  # get all *.dbf and bind them into data.frame
  do.call(
    rbind, 
    list(
      lapply(kES$abbr, ReadDbf, intrpl = "EBK") %>% 
        do.call(rbind, .), 
      lapply(kES$abbr, ReadDbf, intrpl = "IDW") %>% 
        do.call(rbind, .), 
      lapply(kES$abbr, ReadDbf, intrpl = "OK") %>% 
        do.call(rbind, .), 
      lapply(kES$abbr, ReadDbf, intrpl = "RBF") %>% 
        do.call(rbind, .)
    )
  ) %>% 
  # get difference between measured and predicted values
  mutate(
    abs_diff = abs(predicted - measured), 
    abs_diff_rate = abs_diff / measured, 
    abs_diff_sqr = abs_diff ^ 2, 
    abs_diff_rate_sqr = abs_diff_rate ^ 2
  )
val.res <- 
  val.base %>% 
  # calculate validation errors
  group_by(intrpl, es) %>% 
  summarise(
    mae = sum(abs_diff) / n(), 
    mre = sum(abs_diff_rate) / n(), 
    rmse = sqrt(sum(abs_diff_sqr) / n()), 
    rmare = sqrt(sum(abs_diff_rate_sqr) / n())
  ) %>% 
  ungroup()

# print the results by validation error type
lapply(
  c("mae", "mre", "rmse", "rmare"), 
  function(x) {
    val.res %>% 
      select(all_of(c("intrpl", "es", x))) %>% 
      pivot_wider(id_cols = intrpl, 
                  names_from = es, 
                  values_from = all_of(x)) %>% 
      mutate(error = x, .before = "intrpl")
  }
)

## Third part ----
# following analysis are based on Xie's data and other layers 
# focus on: ES spatial pattern; ES of each land use; tree attributes of each land use; relationship between ES spatial pattern and other layers 

### ES mapping from EBK ----
lapply(ebk, function(x) {
  tm_shape(x) + 
    tm_raster() + 
    tm_layout(legend.title.size = 1, legend.position = c("left", "bottom")) + 
    tm_compass(type = "8star", size = 1, position = c("left", "top")) + 
    tm_scale_bar(position = c("right", "bottom"))
}) %>% 
  tmap_arrange()

### Total ES of land use ----
# table for total ESV of each land use
lu.data %>% 
  select(landuse, all_of(kES$esv)) %>% 
  pivot_longer(cols = all_of(kES$esv), names_to = "es", values_to = "val") %>% 
  group_by(landuse) %>% 
  summarise(val = sum(val)) %>% 
  ungroup()
# plot for total ESV of each land use
lu.data %>% 
  select(landuse, all_of(kES$esv)) %>% 
  pivot_longer(cols = all_of(kES$esv), names_to = "esv", values_to = "val") %>% 
  left_join(kES, by = "esv") %>% 
  mutate(abbr = factor(abbr, levels = kES$abbr), 
         val = drop_units(val)) %>% 
  ggplot() + 
  geom_bar(aes(landuse, val / 10^6, fill = abbr), 
           stat = "identity", position = "stack") + 
  labs(x = "Land use", y = "ESV (million dollar)") + 
  scale_fill_discrete(name = NULL) + 
  theme_bw()

### Tree attributes of land use ----
# number of species and families 
cat("\n", "total species:", length(unique(indv.data$species)), "\n", 
    "total genera:", length(unique(indv.data$genus)), "\n", 
    "total families:", length(unique(indv.data$family)), "\n", "\n")

# top abundant families 
indv.data %>% 
  group_by(family) %>% 
  summarise(num_tree = n(), prop = n()/nrow(indv.data)) %>% 
  arrange(desc(prop)) %>% 
  head(10) %>% 
  mutate(family = factor(family, levels = .$family)) %>% 
  ggplot() + 
  geom_col(aes(reorder(family, prop), prop)) + 
  coord_flip()

# tree attribute of each land use
lu.data %>% 
  drop_units() %>% 
  select(landuse, richness, density, dbh_mean, dbh_mid, 
         height_mean, height_mid, lai_mean, lai_mid) %>% 
  pivot_longer(cols = -landuse, names_to = "attr", values_to = "val") %>% 
  ggplot() + 
  geom_bar(aes(landuse, val), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(.~ attr, scales = "free")

### Relationship between layers ----
# bug: waiting for Xie's data. 
