## Introduction 

The code is used for the KESMapping (Kyoto Ecosystem Services Mapping) project. 

## Raw Data Codebook 

### GProcData Folder 

### ESEBK Folder

EBK插值结果，相比于下面的InterRes（新），这个文件夹中多了很多除tif之外的文件，不知道是用来做什么的？

### ESVEBK Folder

ESV estimation results based on ES results of EBK and per unite price of each ES? From Xie. 

### InterRes Folder - 旧

旧的谢于松的插值结果，包括了普通克里金法（OK）、经验贝叶斯克里金法（EBK）、反距离加权法（IDW）、径向基函数法（RBF）对各种生态系统服务的插值结果。每个文件夹内包含对文件夹名称同名插值法插值的结果，这些结果均为*.shp文件，其属性表中各列名称代表：“Measured”为该点生态系统服务实测值；“Predicted”是该点插值后的生态系统服务预测值；“Error”是预测值减去实测值之差；“Source_id”某个计算中间结果的编号；“Included”表明计算该点推测值的时候是否考虑了该点的实测值。

### InterRes Folder 

谢于松插值结果的*.tif文件。

### Validation

谢于松的插值结果，包括了普通克里金法（OK）、经验贝叶斯克里金法（EBK）、反距离加权法（IDW）、径向基函数法（RBF）对各种生态系统服务的插值结果。每个文件夹内包含对文件夹名称同名插值法插值的结果，这些结果均为*.shp文件，其属性表中各列名称代表：“Measured”为该点生态系统服务实测值；“Predicted”是该点插值后的生态系统服务预测值；“Error”是预测值减去实测值之差；“Source_id”某个计算中间结果的编号；“Included”表明计算该点推测值的时候是否考虑了该点的实测值。

### RRawData Folder

**i_tree_input.csv**
输入i-Tree eco模型的原始数据，输入人为康杰锋。

**Trees.xlsx**
i-Tree eco的单株树木生态系统服务计算结果，来自平林。

