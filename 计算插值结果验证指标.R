# 概述 ----
# 基于谢于松导出的各类插值方法各服务结果，计算推测值的验证指标，包括平均绝对误差MAE、平均绝对百分误差MARE、平均相对误差 MRE、均方根误差RMSE、均方根相对误差RMSRE（还是RMARE？）和一致性指标A

# 包 ----
library(shapefiles)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggh4x)

# 函数 ----
# 函数：提取给定
# 参数：
# dir.name：目标文件夹路径
ReadAllDbf <- function(dir.name) {
  # 提取指定文件夹中所有*.dbf文件
  dbf.names <- list.files(dir.name) %>% 
    grep(pattern = ".dbf", x = ., value = TRUE)
  
  # 读取这些*.dbf文件并存储在列表中
  # 建立空列表以存储读取结果
  out.ls <- vector("list", length = length(dbf.names))
  names(out.ls) <- dbf.names
  # 读取并存储*.dbf文件
  for (i in dbf.names) {
    out.ls[[i]] <- read.dbf(paste0(dir.name, "/", i)) %>% 
      .$dbf %>% as_tibble() %>%  # 提取目标表格并转化为tibble格式
      rename_with(tolower) %>%  # 将列名都改成小写
      select(measured, predicted)
  }
  # 将结果列表各元素名称中的“.dbf”去除
  names(out.ls) <- gsub(".dbf", "", names(out.ls))
  
  # 返回结果
  return(out.ls)
}

# 函数：基于某个插值方法得到的各服务的结果，计算预测值和实测值的差值等
# 参数：
# x：包含某个插值方法得到的各服务结果的列表
GetInterpDiff <- function(x) {
  # 构建空列表用于存储结果
  out.ls <- vector("list", length = length(x))
  names(out.ls) <- names(x)
  
  # 计算各项验证指标
  for (i in names(x)) {
    out.ls[[i]] <- x[[i]] %>% 
      mutate(
        abs_diff = abs(predicted - measured), 
        abs_diff_rate = abs_diff / measured, 
        abs_diff_sqr = abs_diff ^ 2, 
        abs_diff_rate_sqr = abs_diff_rate ^ 2
      ) 
  }
  
  return(out.ls)
}

# 函数：去除预测值和实测值差异太大的点
# 参数：
# x：包含某个插值方法得到的各服务结果预测值和实测值差值的列表
# pct.min：预测值和实测值差异最小值
# pct.min：预测值和实测值差异最大值
DelOutlier <- function(x, pct.min = 0.1, pct.max = 10) {
  # 构建空列表用于存储结果
  out.ls <- vector("list", length = length(x))
  names(out.ls) <- names(x)
  
  for (i in names(x)) {
    out.ls[[i]] <- x[[i]] %>% 
      subset(abs_diff_rate > pct.min & abs_diff_rate < pct.max)
  }
  
  return(out.ls)
}

# 函数：基于某个插值方法得到的各服务的结果，计算对应的各项验证指标
# 参数：
# x：包含某个插值方法得到的各服务结果预测值和实测值差值的列表
GetInterpError <- function(x) {
  # 构建空列表用于存储结果
  out.ls <- vector("list", length = length(x))
  names(out.ls) <- names(x)
  
  # 计算各项验证指标
  for (i in names(x)) {
    out.ls[[i]] <- x[[i]] %>% 
      summarise(
        mae = sum(abs_diff) / n(), 
        mre = sum(abs_diff_rate) / n(), 
        rmse = sqrt(sum(abs_diff_sqr) / n()), 
        rmare = sqrt(sum(abs_diff_rate_sqr) / n())
      ) %>% 
      .[1, ] %>% 
      mutate(es = i)
  }
  
  # 将结果转化为数据框
  out.df <- Reduce(rbind, out.ls) %>% 
    select(es, !es)
  
  return(out.df)
}

# 数据 ----
# 插值方法
KInterpMeth <- c("EBK", "IDW", "OK", "RBF")

# 读取各类插值法各种服务生成结果的属性表
# 空列表以存储各个*.shp文件的属性表
interp.res <- vector("list", length = length(KInterpMeth))
names(interp.res) <- KInterpMeth
# 读取各插值方法对应结果文件夹中的所有*.dbf文件属性表
for (i in KInterpMeth) {
  interp.res[[i]] <- 
    ReadAllDbf(dir.name = paste0("GProcData/InterpRes/", i))
}

# 计算预测值和实测值的差等
interp.diff <- lapply(interp.res, GetInterpDiff)

# 去除预测值和实测值差异小于0.01或者大于100的点
interp.diff.sub <- 
  lapply(interp.diff, DelOutlier, pct.min = 0.00, pct.max = 100000)

# 计算衡量准确度的验证指标
interp.error <- lapply(interp.diff.sub, GetInterpError)
interp.error.lng <- interp.error
for (i in names(interp.error.lng)) {
  interp.error.lng[[i]] <- interp.error.lng[[i]] %>% 
    mutate(interp_meth = i) %>% 
    pivot_longer(cols = c("mae", "mre", "rmse", "rmare"), 
                 names_to = "error", values_to = "error_value")
}
# 计算验证指标并将其合并为一个数据框
interp.error.lng <- Reduce(rbind, interp.error.lng) %>% 
  # 修正部分生态系统服务名称
  mutate(es = tolower(es)) %>% 
  mutate(es = case_when(
    es == "co_se" ~ "c_seq", 
    es == "pm2" ~ "pm25", 
    TRUE ~ es
  ))

# 可视化 ----
png("RProcData/各插值各服务各验证指标对比.png", 
    width = 2000, height = 1600, res = 300)
ggplot(interp.error.lng) + geom_col(aes(interp_meth, error_value)) + 
  facet_grid2(error ~ es, scales = "free", independent = "y", 
              switch = "y") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
dev.off()

# 结果导出 ----
interp.error.lng %>% 
  pivot_wider(id_cols = c("es", "interp_meth"), 
              names_from = "error", 
              values_from = "error_value") %>% 
  # 按照生态系统服务排序
  arrange(es, interp_meth) %>% 
  write.xlsx("RProcData/各插值各服务各验证指标结果.xlsx")
