## Introduction 

The code is used for the KESMapping (Kyoto Ecosystem Services Mapping) project.

## Raw Data Codebook 

All the raw data are stored in RawData Folder. 

**ESEBK Folder**
Interpolation results of each ES by EBK interpolation method created by Xie. 

**ESVEBK Folder**
ESV estimation results based on ES results of EBK and per unite price of each ES, created by Xie using GIS. 

**InterRes Folder **
Interpolation results of each ES by each interpolation method in *.tif format, created by Xie using GIS. 

**LandUse Folder**
GIS layer of land use of Kyoto City built-up area, download from Ministry of Land, Infrastructure, Transport and Tourism, Japan. 

**SampleQuadrat Folder**
GIS layer of sample quadrat for plant investigation. 

**Validation Folder**
Interpolation results of the investigated quadrat in GIS point format, showing the ES estimation based on investigation data and that of interpolation. This data is used for model validation, to pick out the best interpolation method with lowest errors. The fields of the *.shp files are: 

`Measured`: the ES estimation based on investigation data and i-Tree Eco. 
`Predicted`: the ES prediction from GIS interpolation. 
`Error`: the difference between ES predicted value (the values of `Predicted` field) and measured value (the values of field `Measured`). 
`Source_id`: ID of middle result generated by GIS. 
`Included`: whether the interpolation method considered the measured ES value of this point during the interpolation. 

**Input_i_tree.csv**
Input data to i-Tree Eco, created by Kang. 

**Plant_data.csv**
Plant investigation data, investigated and created by Kang. 

**Plant_info.csv**
A list of species, the information of which including species name, origin, genus, and family. 

**KUP_Plot_info.xlsx**
Information of the quadrats. 

**Output_i_tree.xlsx** 
Output results of i-Tree Eco, provided by Hirabayashi. 

