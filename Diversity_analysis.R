# 概述 ----
# 输出GIS分析所需数据：调查样方及对应各类生态系统服务的数值
# 计算各样地多样性指数

# 包 ----
library(openxlsx)
library(dplyr)
library(tidyr)
library(vegan)

# 函数 ----
# 函数：基于每木数据获得群落宽数据
# 输出：群落款数据，列为物种名，每行为每棵树木
# 参数：
# x：调查所得每木数据
# nq_colgroup：汇总所根据的分组
GetComm <- function(x, nq_colgroup) {
  x %>% 
    mutate(stem = 1) %>%  # 每棵树的丰度即为1
    select({{nq_colgroup}}, stem, species) %>%
    pivot_wider(names_from = species, values_from = stem,
                values_fn = sum, values_fill = 0) %>% 
    return()
}

# 函数：基于个体数据计算样地多样性
# 参数：
# x：调查所得个体数据
# x_comm：群落数据
# nq_colgroup：汇总所根据的分组
GetDiv <- function(x, x_comm, nq_colgroup) {
  # 内置函数：分组汇总统计各项属性
  funin_attrcalc <- function(coltar, tarvalue) {
    x_sub <- x
    x_sub["tarornot"] <- x_sub[coltar] == tarvalue
    x_sub <- x_sub %>% group_by({{nq_colgroup}}) %>% 
      summarise(
        perc = sum(1 * tarornot) / sum(1)) %>%
      ungroup() %>% 
      select({{nq_colgroup}}, perc)
    names(x_sub)[2] <- paste0("perc_", tarvalue)
    return(x_sub)
  }
  
  output <- x_comm %>%
    mutate(abundance = rowSums(.[3:ncol(.)]),
           richness = apply(.[2:ncol(.)]>0, 1, sum),
           shannon = diversity(.[2:ncol(.)], index = "shannon"),
           simpson = diversity(.[2:ncol(.)], index = "simpson"),
           evenness = shannon / log(richness)) %>%
    select({{nq_colgroup}}, 
           abundance, richness, shannon, simpson, evenness)
  
  return(output)
}

# 数据 ----
kUsdJpy <- 109
kES <- 
  c("carbon_storage", "carbon_seq", 
    "no2_removal", "o3_removal", "pm25_removal", "so2_removal", "avo_runoff")
kESV <- 
  c("carbon_storage_value", "carbon_seq_value", 
    "no2_value", "o3_value", "pm25_value", "so2_value", "avo_runoff_value")

#. 样地信息 ----
qua.info <- read.xlsx("RRawData/KUP_Plot_info.xlsx", sheet = "样方信息") %>% 
  rename_with(tolower) %>% 
  rename(kes_qua_id = kes_plot_id, ward = ward_en) %>% 
  select(qua_id, kes_qua_id, landuse_class, ward) %>% 
  rename(landuse = landuse_class) %>% 
  tibble()

#. i-Tree输入数据 ----
# 原始数据来自平林
itree.input <- read.csv("RRawData/i_tree_input.csv") %>% 
  rename(res_tree_id = ID, 
         kes_qua_id = PlotId) %>% 
  select(kes_qua_id, res_tree_id) %>% 
  as_tibble()

#. 物种对照名单 ----
spe.ls <- read.csv("RRawData/I_tree_species_list.csv") %>% 
  as_tibble() %>% 
  rename_with(tolower) %>% 
  rename(spe_code = "sppcode", 
         species_name = "species.name") %>% 
  mutate(species = paste0(genus, " ", species_name)) %>% 
  select(spe_code, species)

#. 每木数据 ----
# 原始数据来自平林的Access数据库
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
    # 转化为美元
    compensatory_value = compensatory_value / kUsdJpy,  
    # 计算碳储存货币价值并转化为美元：
    # 日本碳服务价值为10600日元/吨碳，即10.6日元/千克碳
    carbon_storage_value = 10.6 * carbon_storage / kUsdJpy,
    carbon_seq_value = 10.6 * carbon_seq / kUsdJpy, 
    # 计算雨水截留货币价值并转化为美元：日本雨水截留价值为719日元/立方米雨水
    avo_runoff_value = 719 * avo_runoff / kUsdJpy
  ) %>% 
  left_join(spe.ls, by = "spe_code") %>% 
  left_join(qua.info, by = "kes_qua_id") %>% 
  select(res_tree_id, qua_id, species, 
         lai, dbh, biomass, 
         carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff, 
         compensatory_value, 
         carbon_storage_value, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value,  
         avo_runoff_value)

##. 群落和多样性 ----
qua.comm <- GetComm(indv.data, qua_id)
qua.div <- GetDiv(indv.data, qua.comm, qua_id) %>% 
  # 加入土地利用数据
  left_join(qua.info, by = "qua_id")

##. 样方水平服务量 ----
# 将个体水平数据汇总为样方水平数据
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

#. 各项服务单价 ----
# 基于样方生态系统服务结果计算各项服务单价
price <- vector("numeric", length = length(kES))

# 各项单价为所有样方对应服务货币量和物理量之商的平均值
for (i in 1:length(kES)) {
  price[i] <- (qua.es[[kESV[i]]] / qua.es[[kES[i]]]) %>% 
    mean()
}

# 转换成数据框并输出
price <- data.frame(
  service = kES, 
  price = price, 
  # 各服务单价对应单位
  unit = c("美元/千克碳", "美元/千克碳", 
           "美元/克", "美元/克", "美元/克", "美元/克", 
           "美元/立方米雨水")
)

# 分析 ----
# 各样地多样性和ES关系
qua.div.es <- qua.div %>% 
  left_join(qua.es, by = "qua_id")

# 查看多样性和各项ES之间的关系
qua.div.es.cor <- 
  psych::corr.test(
    select(qua.div.es, abundance, richness, shannon, all_of(kES))
  )
png("RProcData/样地水平多样性和ES之间的相关性.png", 
    width = 2000, height = 2000, res = 300)
corrplot::corrplot(
  corr = qua.div.es.cor$r, method = "number", p.mat = qua.div.es.cor$p
)
dev.off()

# 导出 ----
write.xlsx(qua.es, "GRawData/R_Qua_es.xlsx")
write.xlsx(price, "RProcData/各项服务单价.xlsx")
write.xlsx(qua.div, "RProcData/各样地乔木多样性.xlsx")
