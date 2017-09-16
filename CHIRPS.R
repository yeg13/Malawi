

## Load required packages
packages <- c("sp", "maptools", "raster", "rasterVis", "RCurl", "R.utils", "rgdal", "rgeos", "data.table")
lapply(packages, require, character.only = TRUE)

## Define working directory and read vector layer
setwd("C:\\Users\\guyuye2\\Desktop\\DHS\\GIS")

## Read shapefile, dissolve, union and spatial transformation
MW <- readOGR("Shapefiles/MW_Admin1_LHZ_2012.3", "MW_Admin1_LHZ_2012.3")
ZM <- readOGR("Shapefiles/ZM", "ZMB_adm0")
ZW <- readOGR("Shapefiles/ZW", "ZWE_adm0")

list <- list(MW,ZM, ZW)
countries <- do.call(bind, list)
countries <- gUnaryUnion(countries)

## Define projection system - Arc_1950_UTM_Zone_35S 
crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs.proj <- CRS("+init=epsg:20936")
countries_proj <- spTransform(countries, crs.proj)

## Read data from shapefiles
## Malawi
MW2000 <- readOGR("MW_2000_DHS", "MWGE43FL")
MW2004 <- readOGR("MW_2004_DHS", "MWGE4BFL")
MW2010 <- readOGR("MW_2010_DHS", "MWGE62FL")
MW2012 <- readOGR("MW_2012_MIS", "MWGE6AFL")
MW2013 <- readOGR("MW_2013-14_DHS", "MWGE6IFL")
MW2014 <- readOGR("MW_2014_MIS", "MWGE71FL")

## Zimbabwe
ZM2007 <- readOGR("ZM_2007_DHS", "ZMGE52FL")
ZM2013 <- readOGR("ZM_2013-14_DHS", "ZMGE61FL")

## Zambia
ZW1999 <- readOGR("ZW_1999_DHS", "ZWGE42FL")
ZW2005 <- readOGR("ZW_2005-06_DHS", "ZWGE52FL")
ZW2010 <- readOGR("ZW_2010-11_DHS", "ZWGE61FL")
ZW2015 <- readOGR("ZW_2015_DHS", "ZWGE71FL")

## MW 2013 has a different data structure and does not have an indicator for urban vs. rural
setnames(MW2013@data, old = c("SPAID", "FIPSCC", "SPAYEAR", "SPAFACID"), new = c("DHSID", "DHSCC","DHSYEAR","DHSCLUST"))
MW2013@data$URBAN_RURA <- "N"

## separate the whole dataset into urban and rural
cluster_list <- list(MW2000, MW2004, MW2010, MW2012, MW2013, MW2014, ZM2007, ZM2013, ZW1999, ZW2005, ZW2010, ZW2015)

small = MW2000[MW2000$DHSYEAR == 2010,]
for (i in 1:length(cluster_list)){
  part= cluster_list[[i]][, c("DHSID", "DHSCC", "DHSYEAR", "DHSCLUST", "URBAN_RURA", "LATNUM", "LONGNUM")]
  small = rbind(small, part)
}

small_proj <- spTransform(small, crs.proj)
small_u <- subset(small_proj, small_proj$URBAN_RURA == "U")
small_r <- subset(small_proj, small_proj$URBAN_RURA != "U")


## Define URL and file names for bulk download
## Skip if data is already downloaded
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

rlist <- list.files(getwd(), 
                    pattern = "tif$",
                    full.names=TRUE)
r <- stack(rlist)

## try with one layer
#clip <- crop(r[[1]], extent(countries), snap = "out")
#clip <- mask(clip, countries)
#clip_proj1 <- projectRaster(clip, crs = crs.proj)
#ex <- as.matrix(extract(clip_proj1, small_u, buffer=2000, fun = mean))

detach("package:R.utils", unload=TRUE)

mat.data.urban = c()
mat.data.rural = c()
for (i in 1:nlayers(r)){
  clip = crop(r[[i]], extent(countries), snap = "out")
  clip = mask(clip, countries)
  clip_proj <- projectRaster(clip, crs = crs.proj)
  ex_urban = extract(clip_proj, small_u, buffer = 2000, fun = mean)
  ex_rural = extract(clip_proj, small_r, buffer = 5000, fun = mean)
  mat_urban = t(as.matrix(ex_urban))
  mat_rural = t(as.matrix(ex_rural))
  #mat <- t(lapply(ex, FUN = max))
  mat.data.urban <-rbind(mat.data.urban, mat_urban) 
  mat.data.rural <-rbind(mat.data.rural, mat_rural) 
}
#write.csv(mat.data.urban,"CHIRPS_Urban.csv")
#write.csv(mat.data.rural,"CHIRPS_Rural.csv")

mat.data.urban <- read.csv("CHIRPS_Urban.csv", header = TRUE)
mat.data.rural <- read.csv("CHIRPS_Rural.csv")

small_u$ID <- paste0("V", seq(numeric(nrow(small_u))))
small_r$ID <- paste0("V", seq(numeric(nrow(small_r))))

period <- as.data.frame(seq(as.Date("1981/01/01"), as.Date("2017/01/01"), by = "month"))
colnames(period) <- "period"
mat.data.urban <- cbind( period, mat.data.urban)
mat.data.urban$X <- NULL
colnames(mat.data.urban) <- c("period", paste0("V", seq(nrow(small_u))))
mat.data.rural <- cbind(period, mat.data.rural)
mat.data.rural$X <- NULL

write.csv(mat.data.urban,"CHIRPS_Urban_All.csv")
write.csv(mat.data.rural,"CHIRPS_Rural_All.csv")
write.csv(small_u@data, "Link_Urban.csv")
write.csv(small_r@data, "Link_Rural.csv")



