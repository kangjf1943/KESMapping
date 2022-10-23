# Statement ----
# calculate data for GIS analysis: ecosystem services for each quadrat
# calculate biodiversity indexes of each quadrat

# Package ----
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(vegan)
library(dunn.test)

# Function ----
# function: get community data based on individual data
# note: output is wide data of community - rows as species and columns as number of trees
# variable: 
# x: investigation data of each tree
# col.group: column name group based on, without quotation marks
GetComm <- function(x, col.group) {
  x %>% 
    mutate(stem = 1) %>%  # 每棵树的丰度即为1
    select({{col.group}}, stem, species) %>%
    pivot_wider(names_from = species, values_from = stem,
                values_fn = sum, values_fill = 0) %>% 
    return()
}

# function: get biodiversity indexes based on individual data 
# variable: 
# x: investigation data of each tree
# x_comm: community data, you can get it with GetComm()
# col.group: column name group based on, without quotation marks
GetDiv <- function(x, x_comm, col.group) {
  # inside function: summary each attribute 
  funin_attrcalc <- function(coltar, tarvalue) {
    x_sub <- x
    x_sub["tarornot"] <- x_sub[coltar] == tarvalue
    x_sub <- x_sub %>% group_by({{col.group}}) %>% 
      summarise(
        perc = sum(1 * tarornot) / sum(1)) %>%
      ungroup() %>% 
      select({{col.group}}, perc)
    names(x_sub)[2] <- paste0("perc_", tarvalue)
    return(x_sub)
  }
  
  output <- x_comm %>%
    mutate(abundance = rowSums(.[3:ncol(.)]),
           richness = apply(.[2:ncol(.)]>0, 1, sum),
           shannon = diversity(.[2:ncol(.)], index = "shannon"),
           simpson = diversity(.[2:ncol(.)], index = "simpson"),
           evenness = shannon / log(richness)) %>%
    select({{col.group}}, 
           abundance, richness, shannon, simpson, evenness)
  
  return(output)
}

# Data ----
## Constant ----
# rate of Japanese Yen and US dollar: average rate of 2019
kUsdJpy <- 109

# ecosystem service items 
kES <- 
  c("carbon_storage", "carbon_seq", 
    "no2_removal", "o3_removal", "pm25_removal", "so2_removal", "avo_runoff")
kESV <- 
  c("carbon_storage_value", "carbon_seq_value", 
    "no2_value", "o3_value", "pm25_value", "so2_value", "avo_runoff_value")

# land use type
kLanduse <- 
  c("R-low", "R-high", "R-other", "Ind", "Com-neigh", "Com")

# biodiversity indexes
kIndex <- 
  c("abundance", "richness", "shannon", "simpson", "evenness")

## Quadrat info ----
qua.info <- read.xlsx("RRawData/KUP_Plot_info.xlsx", sheet = "样方信息") %>% 
  rename_with(tolower) %>% 
  rename(kes_qua_id = kes_plot_id, ward = ward_en) %>% 
  select(qua_id, kes_qua_id, landuse_class, ward) %>% 
  rename(landuse = landuse_class) %>% 
  tibble() %>% 
  # change names of land use column
  mutate(land_use = case_when(
    land_use == "Com" ~ "Com", 
    land_use == "Com neigh" ~ "ComNbr", 
    land_use == "Ind" ~ "Ind", 
    land_use == "R high" ~ "ResHigh", 
    land_use == "R low" ~ "ResLow", 
    land_use == "R resi" ~ "ResOther", 
  ))

# investigation data 
tree.data <- read.csv("RRawData/Plant_data.csv") %>% 
  tibble() %>% rename_with(tolower) %>% 
  rename(species = species_lt) %>% 
  subset(tree_shrub == "tree") %>% 
  select(plot_id, species) %>% 
  # join land use data 
  left_join(qua.info, by = c("plot_id" = "qua_id"))

## i-Tree input data ----
# the raw data comes from Hirabayashi
itree.input <- read.csv("RRawData/i_tree_input.csv") %>% 
  rename(res_tree_id = ID, 
         kes_qua_id = PlotId) %>% 
  select(kes_qua_id, res_tree_id) %>% 
  as_tibble()

## Species list ----
spe.ls <- read.csv("RRawData/I_tree_species_list.csv") %>% 
  as_tibble() %>% 
  rename_with(tolower) %>% 
  rename(spe_code = "sppcode", 
         species_name = "species.name") %>% 
  mutate(species = paste0(genus, " ", species_name)) %>% 
  # eliminate the duplicated 
  unique() %>% 
  # join taxon family data 
  left_join(read.csv("RRawData/Plant_info.csv") %>% 
              rename_with(tolower) %>% 
              select(genus, family) %>% 
              unique(), 
            by = "genus") %>% 
  select(spe_code, species, genus, family)

# top species of each land use by abundance 
tree.data.top <- 
  tree.data %>% 
  group_by(landuse, species) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(landuse, n) %>% 
  mutate(landuse = factor(landuse, levels = kLanduse))
# tree number of each land use
tree.data.treenum <- 
  tree.data %>% 
  group_by(landuse) %>% 
  summarise(treenum = n()) %>% 
  ungroup()
# join total tree number and calculate the proportion
tree.data.top <- 
  tree.data.top %>% 
  left_join(tree.data.treenum, by = "landuse") %>% 
  mutate(prop = n / treenum)

## Individual tree data ----
# raw data from Access database from Hirabayashi 
indv.data <- read.xlsx("RRawData/Trees.xlsx", sheet = "Trees") %>% 
  as_tibble() %>% 
  rename(res_tree_id = "TreeID", 
         spe_code = "SpCode", 
         dbh = "DBH.(CM)", 
         lai = "LEAF.AREA.INDEX", 
         carbon_storage = "CARBON.STORAGE.(KG)", 
         carbon_seq = "GROSS.CARBON.SEQ.(KG/YR)", 
         biomass = "BIOMASS.ADJUSTMENT", 
         co_removal = "CO.Removal.(g)", 
         no2_removal = "NO2.Removal.(g)", 
         o3_removal = "O3.Removal.(g)", 
         pm25_removal = "PM25.Removal.(g)", 
         so2_removal = "SO2.Removal.(g)", 
         compensatory_value = "TREE.VALUE.(Yen)",       
         no2_value = "NO2.Value.($)", 
         o3_value = "O3.Value.($)", 
         pm25_value = "PM25.Value.($)",          
         so2_value = "SO2.Value.($)", 
         avo_runoff = "Avoided.Runoff.(m3)") %>% 
  left_join(itree.input, by = "res_tree_id") %>% 
  mutate(
    # turn monetary value from Japanese Yen to US dollar
    compensatory_value = compensatory_value / kUsdJpy,  
    # calculate carbon monetary value and turn to US dollar: the carbon price in Japan is 10600 JPY/ton carbon = 10.6 JPY/kg carbon
    carbon_storage_value = 10.6 * carbon_storage / kUsdJpy,
    carbon_seq_value = 10.6 * carbon_seq / kUsdJpy, 
    # calculate runoff reduction value and turn to US dollars: runoff reduction value in Japan is 719 JPY/cubic meter rainwater
    avo_runoff_value = 719 * avo_runoff / kUsdJpy
  ) %>% 
  left_join(spe.ls, by = "spe_code") %>% 
  left_join(qua.info, by = "kes_qua_id") %>% 
  select(res_tree_id, qua_id, species, genus, family, 
         lai, dbh, biomass, 
         carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff, 
         compensatory_value, 
         carbon_storage_value, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value,  
         avo_runoff_value)

## Community data and biodiversity ----
qua.div <- GetComm(indv.data, qua_id) %>% 
  GetDiv(x = indv.data, x_comm = ., col.group = qua_id) %>% 
  # left join land use data 
  left_join(qua.info, by = "qua_id")

## Quadrat ecosystem services ----
# aggregate individual data to quadrat data 
qua.es <- indv.data %>% 
  select(qua_id, 
         carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff, 
         compensatory_value, 
         carbon_storage_value, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value, 
         avo_runoff_value) %>% 
  group_by(qua_id) %>% 
  summarise(across(!starts_with("qua_id"), sum), 
            treenum = n()) %>% 
  ungroup()

## Ecosystem services unit price ----
# calculate quadrat ecosystem services unit price based on quadrat data
price <- vector("numeric", length = length(kES))

# 各项单价为所有样方对应服务货币量和物理量之商的平均值
for (i in 1:length(kES)) {
  price[i] <- (qua.es[[kESV[i]]] / qua.es[[kES[i]]]) %>% 
    mean()
}

# turn to data.frame
price <- data.frame(
  service = kES, 
  price = price, 
  # 各服务单价对应单位
  unit = c("dollar/kg carbon", "dolar/kg carbon", 
           "dollar/g", "dollar/g", "dollar/g", "dollar/g", 
           "dollar/cubic meter water")
)

# Analysis ----
## Description ----
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
  geom_bar(aes(family, prop), stat = "identity")

#. Top species ----
# top species of the whole city
tree.data %>% group_by(species) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(n) %>% 
  tail(10) %>% 
  mutate(species = factor(.$species, levels = .$species)) %>% 
  ggplot() + 
  geom_bar(aes(species, x = n), stat = "identity")

# top species by land use
temp.plots <- vector("list", 6)
names(temp.plots) <- kLanduse
for (i in kLanduse) {
  temp.plots[[i]] <- tree.data.top %>% 
    subset(landuse == i) %>% 
    tail(10) %>% 
    mutate(species = factor(.$species, levels = .$species)) %>% 
    ggplot() + 
    geom_bar(aes(species, x = prop), stat = "identity") +
    labs(x = "Relative abundance", y = "Species") + 
    xlim(0, 0.6) 
}
for (i in 1:6) {
  temp.plots[[i]] <- temp.plots[[i]] + 
    ggtitle(paste0("(", letters[i], ")"))
}
Reduce("|", temp.plots[1:3]) / 
  Reduce("|", temp.plots[4:6])

## Quadrat biodiversity ~ land use ----
png("RProcData/Quadrat_biodiversity_indexes_of_land_use_types.png", 
    width = 1200, height = 2000, res = 300)
qua.div %>% 
  pivot_longer(cols = c("richness", "shannon", "simpson", "evenness"), 
               names_to = "index", values_to = "index_value") %>% 
  mutate(landuse = factor(landuse, levels = kLanduse), 
         index = factor(index, levels = kIndex)) %>% 
  ggplot() + 
  geom_boxplot(aes(landuse, index_value)) + 
  labs(x = "Land use type", y = "Quadrat biodiversity index") + 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  theme_bw() + 
  facet_wrap(.~ index, ncol = 1, scales = "free_y", 
             labeller = labeller(
               index = c(richness = "Richness", 
                         shannon = "Shannon", 
                         simpson = "Simpson", 
                         evenness = "Evenness"))) + 
  geom_text(data = GetP(qua.div), aes(x =Inf, y = Inf, label = label), 
            size=3.5, hjust = 1.05, vjust = 1.5)
dev.off()

# 统计分析
for (i in kIndex) {
  cat("\n")
  print(i)
  dunn.test(x = qua.div[[i]], g = qua.div$landuse)
}

# correlation between quadrat biodiversity and ecosystem services 
qua.div.es <- qua.div %>% 
  left_join(qua.es, by = "qua_id")

qua.div.es.cor <- 
  psych::corr.test(
    select(qua.div.es, abundance, richness, shannon, all_of(kES))
  )
png("RProcData/Cor_between_quadrat_biodiversity_and_ecosystem_services.png", 
    width = 2000, height = 2000, res = 300)
(corrplot::corrplot(
  corr = qua.div.es.cor$r, method = "number", p.mat = qua.div.es.cor$p
))
dev.off()

# Export ----
write.xlsx(qua.es, "GRawData/R_Qua_es.xlsx")
write.xlsx(price, "RProcData/Unit_price_of_ecosystem_services.xlsx")
write.xlsx(qua.div, "RProcData/Biodiversity_of_each_quadrat.xlsx")
