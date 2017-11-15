library(rgdal) # readOGR function to read in shapefile
library(ggplot2) # fortify function to convert 
library(ggmap) 
library(readxl)

data.urls <- c("https://data.humdata.org/dataset/6b2656e2-b915-4671-bfed-468d5edcd80a/resource/49379525-bd51-4d27-a9fb-a04151561898/download/yem_admin_20171007.gdb.zip",
               "https://data.humdata.org/dataset/6b2656e2-b915-4671-bfed-468d5edcd80a/resource/f942fe94-c28a-4e4d-9bed-d241bdce345b/download/yem_admin_20171007-.xlsx",
               "https://data.humdata.org/dataset/6b2656e2-b915-4671-bfed-468d5edcd80a/resource/1574d33c-c002-4294-a7c4-89433728c9b3/download/yemen_admin_20171007_shape.zip")

data.file.names <- basename(data.urls)

mapply(download.file, data.urls, data.file.names, quiet=TRUE)
unzip(data.file.names[1])
unzip(data.file.names[3], exdir=gsub(".zip", "", data.file.names[3]))


info <- read_excel(data.file.names[2])
shapefile <- readOGR(gsub(".zip", "", data.file.names[3]), "yem_admin1")

#convert shapefile to dataframe
shapefile_df <- fortify(shapefile)

map <- ggplot(shapefile_df, aes(x = long, y = lat, group = group)) + 
  theme_bw() +
  geom_polygon(color = 'gray', fill = 'white') +
  theme(text=element_blank(), axis.ticks=element_blank())

print(map) 

saveRDS(info, "yemen_shp_info.rds")
saveRDS(shapefile_df, "yemen_shp.rds")
saveRDS(map, "yemen_map.rds")