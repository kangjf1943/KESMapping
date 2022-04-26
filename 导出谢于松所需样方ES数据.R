# 输出GIS分析所需数据：调查样方及对应各类生态系统服务的数值

# 导入所需的包
library(openxlsx)
library(dplyr)

# 读取KES项目输出的各样方生态系统服务数据
qua_es <- read.csv("RRawData/KES_Quadata.csv") %>% 
  tibble() %>% 
  rename(kes_plot_id = qua_id) %>% 
  left_join(
    # 将KES项目的样方编号替换成KUP项目的样方编号
    read.xlsx("RRawData/KUP_Plot_info.xlsx", sheet = "样方信息") %>% 
      tibble() %>% 
      rename_with(tolower) %>% 
      select(qua_id, kes_plot_id), 
    by = "kes_plot_id"
  ) %>% 
  # 去除不需要的列
  select(- kes_plot_id, - co_removal, - compensatory_value) %>% 
  # 重命名名称超过10个字符的列
  rename_with(~ gsub("removal", "rem", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("value", "v", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("carbon", "c", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("storage", "sto", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("runoff", "run", .x, fixed = TRUE)) %>% 
  # 重新排序
  select(qua_id, treenum, lai:avo_run_v)

# 导出数据
write.xlsx(qua_es, "GRawData/R_Qua_es.xlsx")

# 在GIS中，将该数据和样方GIS数据对应起来，筛选掉未包含在内的样方GIS点数据

