# 谢于松各ESV表格中的算法：用各种ES的平均值（sheet1）乘以各类土地利用的面积，得到了sheet2中的总量数据，再乘以我给的单价，得到各类ES的总价
# 各类土地利用的面积为：
land.use %>% 
  group_by(land_use) %>% 
  summarise(area = sum(Area)) %>% 
  ungroup()
# 比如ResLow：
3519 * 7697.95997866543
# 再比如Com：
1009 * 6525.90489592793

# 问题在于，我计算出来的平均值和谢的平均值不同
# 为了可视化过程，先写个提取特定土地利用的特定CS图层
GetTifLanduse <- function(x, es) {
  outline <- subset(land.use, land_use == x) %>% 
    st_transform(crs(ebk[[es]]))
  mask(ebk[[es]], outline)
}
# 再写个计算特定土地利用特定ES平均值的函数
GetMeanLanduse <- function(x, es) {
  outline <- subset(land.use, land_use == x) %>% 
    st_transform(crs(ebk[[es]]))
  mask(ebk[[es]], outline) %>% 
    global(fun = "mean", na.rm = TRUE) %>% 
    .[1, 1] %>% 
    return()
}

# 比如碳储存
# 提取ResLow的CS图层
GetTifLanduse("ResLow", "CS") %>% 
  plot()
# 计算出来的单位应该是千克/样方，因此将其转化为千克碳/公顷：
GetMeanLanduse("ResLow", "CS") / 400 * 10000
# 而谢于松为7698
# 再比如Com的NO2
GetMeanLanduse("Com", "NO2") / 400 * 10000
# 而谢于松为729
# 谢如何将单位从ES原单位/样方转化到ES单位/公顷呢？
# 谢计算方法的另一个问题：面积不同，面积是基于多边形计算的，而ES平均值则来自栅格数据
# 但是为什么平均值会差这么多？
