
# This r.script is used to extract average monthly rainfall data for each district in Malawi.
# Raster layers are downloaded from CHIRPS database for Africa, which incorporates 0.05 degree
# resolution satellite imagery with in-situ situation data to create gridded rainfall time series.
# This program downloads and unzips raster files directly, and then write the extracted average
# values in a csv file. 


##install and load required packages
install.packages("maptools")
install.packages("raster")
install.packages("rasterVis")
install.packages("RCurl")
install.packages("R.utils")
install.packages("rgdal")

library(sp)
library(maptools)
library(raster)
library(rasterVis)
library(RCurl)
library(R.utils)
library(rgdal)

##Define working directory and read vector layer
setwd("P:\\Malawi\\CHIRPS")
Malawi <- readShapePoly("P:\\Malawi\\MW_Admin1_LHZ_2012.3\\MW_Admin1_LHZ_2012.3.shp")

## Define URL and file names for bulk download
url <-"ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/tifs/"
filename <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
filename <- strsplit(filename, "\r\n")
filenames <- unlist(filename)
filenames

for (filename in filenames) {
  download.file(paste(url, filename, sep = ""), 
                paste(getwd(), "/", filename, sep = ""))
}

## Unzip all the gz. files in working directory
zip.list <- list.files(getwd(),
                       pattern = "tif.gz$",
                       full.names = TRUE)
zip.list
for(zipfile in zip.list) {
  gunzip(zipfile)
}

## Stack raster layers in a list
rlist <- list.files(path="P:\\Malawi\\CHIRPS", 
                    pattern = "tif$",
                    full.names=TRUE)
r <- stack(rlist)
#rlist
#r
detach("package:R.utils", unload=TRUE)

## Define an empty matrix, extract and append mean values in a forloop
## Can modify FUN for other stats
mat.data <- c()
for(i in 1:nlayers(r)) {
  ex <- extract(r[[i]], Malawi)
  mat <- t(lapply(ex, FUN = mean))
  mat.data <-rbind(mat.data, mat)
}
write.csv(mat.data,"CHIRPS_Malawi_Mean.csv")

## Max values
mat.data <- c()
for(i in 1:nlayers(r)) {
  ex <- extract(r[[i]], Malawi)
  mat <- t(lapply(ex, FUN = max))
  mat.data <-rbind(mat.data, mat)
}
write.csv(mat.data,"CHIRPS_Malawi_Max.csv")

## Min values
mat.data <- c()
for(i in 1:nlayers(r)) {
  ex <- extract(r[[i]], Malawi)
  mat <- t(lapply(ex, FUN = min))
  mat.data <-rbind(mat.data, mat)
}
write.csv(mat.data,"CHIRPS_Malawi_Min.csv")

## Standard Deviation
mat.data <- c()
for(i in 1:nlayers(r)) {
  ex <- extract(r[[i]], Malawi)
  mat <- t(lapply(ex, FUN = sd ))
  mat.data <-rbind(mat.data, mat)
}
write.csv(mat.data,"CHIRPS_Malawi_SD.csv")



