# 输出GIS分析所需数据：调查样方及对应各类生态系统服务的数值

# 包 ----
library(openxlsx)
library(dplyr)

# 数据 ----
kUsdJpy <- 109

# 读取i-Tree输入数据
# 原始数据来自平林
itree.input <- read.csv("RRawData/i_tree_input.csv") %>% 
  rename(res_tree_id = ID, 
         qua_id = PlotId) %>% 
  select(qua_id, res_tree_id) %>% 
  as_tibble()

# 读取每棵树的生态系统服务计算结果
# 原始数据来自平林的Access数据库
indv.es <- read.xlsx("RRawData/Trees.xlsx", sheet = "Trees") %>% 
  as_tibble() %>% 
  rename(res_tree_id = "TreeID", 
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
  mutate(es_annual_value = sum(carbon_seq_value, 
                               no2_value, o3_value, pm25_value, so2_value,  
                               avo_runoff_value)) %>% 
  select(res_tree_id, qua_id, 
         carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff, 
         compensatory_value, 
         carbon_storage_value, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value,  
         avo_runoff_value)

# 将个体水平数据汇总为样方水平数据
qua.es <- indv.es %>% 
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

# 导出数据
write.xlsx(qua.es, "GRawData/R_Qua_es.xlsx")
