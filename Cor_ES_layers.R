# 概述 ----
# 分析其他图层，如公共基础设施的密度，与ES的相关性

# 包 ----
library(shapefiles)
library(ggplot2)

# 数据 ----
esv <- read.dbf("GRawData/ESV.dbf")$dbf
infra <- read.dbf("GRawData/公共服务设施.dbf")$dbf
park <- read.dbf("GRawData/城市公园.dbf")$dbf

# 分析 ----
# 公共服务设施和ESV的关系
# 可视化
# 漏洞：公共服务设施密度的单位是什么？英文用词是什么？
ggplot() + 
  geom_point(aes(infra$grid_code, esv$grid_code), alpha = 0.3) + 
  geom_smooth(aes(infra$grid_code, esv$grid_code), 
              method = "lm", color = "red") + 
  labs(x = "Public service facility density", y = "ESV (dollar/ha)")
# 统计结果
cor.test(infra$grid_code, esv$grid_code)

# 公共服务设施和ESV的关系
# 可视化
# 漏洞：公园密度的单位是什么？英文用词是什么？
ggplot() + 
  geom_point(aes(park$grid_code, esv$grid_code), alpha = 0.3) + 
  geom_smooth(aes(park$grid_code, esv$grid_code), 
              method = "lm", color = "red") + 
  labs(x = "Park density", y = "ESV (dollar/ha)")
cor.test(park$grid_code, esv$grid_code)

