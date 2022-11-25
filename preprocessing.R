#____
#Get weather data

library(dplyr); library(readr); library(purrr); library(stringr); library(tibble); library(magick); library(aws.s3)

save_object(
  object = "Official/annual/max_temp/2022.max_temp.nc",
  bucket = "s3://silo-open-data/Official/daily/max_temp/", 
  region = "ap-southeast-2",
  file = "2022.max.nc"
)

save_object(
  object = "Official/annual/min_temp/2022.min_temp.nc",
  bucket = "s3://silo-open-data/Official/daily/min_temp/", 
  region = "ap-southeast-2",
  file = "2022.min.nc"
)


#______
#Get daily hdu file
library(ncdf4); library(rgdal); library(ggplot2); library(rasterVis); library(maptools); library(maps); 
library(tidync); library(sf); library(sp); library(rgeos); library(devtools); library(terra); library(viridis); library(wesanderson)
library(raster)

#unzip("D:\\Maps\\Oceania\\Australia\\vector\\AU_adm.zip")
auadm0ll.sf <- st_read(dsn="C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/maps", layer="AU_adm0_gen-LL")
plot(auadm0ll.sf)
plot(auadm0ll.sf, max.plot=12)
auadm0ll.bb <- st_bbox(auadm0ll.sf)

fn <- "2022.min.nc"
fx <- "2022.max.nc"

dseq <- seq(from = as.Date("01-01-2022", format = "%d-%m-%Y"), to = as.Date((Sys.Date()-2), format = "%d-%m-%Y"), by = 1)

hdu.pname <- paste("hdu", format(dseq, format = "%Y%m%d"), ".tif", sep = "")

#sine method
i <- which(dseq==(Sys.Date())-2)

for(i in i:i){
  # for(i in 1:length(dseq)){
  #create individual raster brick for t min
  trasbrick <- brick(fn)
  #subset only 1 date - date is i to i
  tmin.r <- subset(trasbrick, i:i)
  plot(tmin.r)
  
  #repeat for t max
  trasbrick <- brick(fx)
  tmax.r <- subset(trasbrick, i:i)
  
  Tavg <- (tmax.r+tmin.r)/2
  base <- 14
  W <- (tmax.r-tmin.r)/2
  Q <- (base-Tavg)/W
  
  #transform >1 into 1, <-1 into -1
  
  Q[Q < -1] <- -1
  Q[Q > 1] <- 1
  
  A <- asin(Q)
  
  #calculate the HDU per day
  thdu.r <- ((W*cos(A))-((base-Tavg)*((pi/2)-A)))/pi
  
  # windows(); 
  plot(thdu.r)
  
  # If HDU is less than zero, assign a value of zero:
  thdu.r[thdu.r < 0] <- 0
  
  # Write the HDU raster out as a GTiff file:
  writeRaster(x = thdu.r, file.path("C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/hdumaps", hdu.pname[i]), overwrite=TRUE)
  cat(i, "\n"); flush.console()
}

#______________
#Stack this to the previous 29d of hdu daily files, for a chdu file
library(devtools); library(spatialkernel); library(cropgrowdays)

chdu.pname <- paste("chdu", format(dseq, format = "%Y%m%d"), ".tif", sep = "")
img.pname <- paste("chdu", format(dseq, format = "%Y%m%d"), sep = "")
obname <- data.frame(idx = 1:length(hdu.pname), hdu = hdu.pname, chdu = chdu.pname, img = img.pname)
obname$hdu <- paste("C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/hdumaps/", obname$hdu, sep="")

nday <- 30
it <- 0
it <- it + 1
dcut <- cut(32:length(dseq), breaks = 10)
dcut.n <- match(dcut, levels(dcut))

ord <- which(dcut.n == it)
ord <- (32:length(dseq))[ord]

for(i in i:i){
  # Select each day of interest in turn and list the HDU rasters for the previous 30 days:
  idx.start <- i - (nday - 1)
  idx.stop <- i
  idx <- idx.start:idx.stop 
  thdu.fname <- as.character(obname[idx,2])
  
  rasters <- 0
  
  for (j in 1:length(thdu.fname)){
    traster <- rast(thdu.fname[j])
    rasters <- c(rasters, traster)
  }
  
  rasters <- rasters[-1]
  
  tchdu.r <- rast(rasters)
  
  # Sum all the values in the raster stack:
  tchdu.r <- app(tchdu.r, fun=sum)
  
  # Plot to check, using 130HDU as the cut-off level:
  plot(tchdu.r)
  contour(tchdu.r, levels = 130, lty = 1, add = TRUE, lwd=1.5, col="purple")

  # Write the summed raster (i.e. the CHDU file) out as a GTiff:
  writeRaster(x = tchdu.r, (file.path("C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/hdumaps/", as.character(obname[i,3]))), overwrite = TRUE)
  cat(i, "\n"); flush.console()   
}

#_________
#Find each postcode's value

auspoa.sf <- st_read(dsn="C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/maps", layer="POA_2021_AUST_GDA2020")
auspoa.sf <- auspoa.sf[-c(661, 662, 2525, 2526, 2642:2644),]
list <- auspoa.sf$POA_NAME21
pnames <- c(list)

currentmax <- as.data.frame(matrix(NA, ncol = length(pnames), nrow = length(dseq)))
row.names(currentmax) <- c(dseq)

currentmin <- as.data.frame(matrix(NA, ncol=length(pnames), nrow = length(dseq)))
row.names(currentmin) <- c(dseq)

currentmed <- as.data.frame(matrix(NA, ncol=length(pnames), nrow = length(dseq)))
row.names(currentmed) <- c(dseq)

currentmean <- as.data.frame(matrix(NA, ncol=length(pnames), nrow = length(dseq)))
row.names(currentmean) <- c(dseq) 

# Create a data frame of file indexes and the path to the source HDU files, the destination CHDU file names ('chdu' = cumulative HDU) and the image files:
chdu.fname <- paste("C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/hdumaps/", "chdu", format(dseq, format = "%Y%m%d"), ".tif", sep = "")
obname <- data.frame(idx = 1:length(list), poa = auspoa.sf$POA_NAME21)
dseq.df <- data.frame(idx = 1:length(dseq), dseq=dseq)

#read in the existing dataframes
poa20152022max <- readRDS("poa20152022max.RDS")
y <- nrow(poa20152022max)+1

for (i in i:i){
  traster <- rast(as.character(chdu.fname[i]))
  plot(traster)
  
  for (j in 1:(length(pnames))){
    #subset the map of Australia to an area of interest
    id <- auspoa.sf$POA_NAME21 == list[j]
    tauspoa.sf <- auspoa.sf[id,]

    #extract data specific to that area, and run a function (max)
    x <- terra::extract(traster, tauspoa.sf, fun=summary, na.rm=TRUE, df=TRUE)
    
    #print it in the dataframe, as long as there is a value. Otherwise, print it as the previous postcode
    poa20152022max[y,j] <- x[,7]
    currentmed[i,j] <- x[,4]
    currentmin[i,j] <- x[,2]
    currentmean[i,j] <- x[,5]
    
    cat(j, "\n"); flush.console()
    
  }
  
  #year.df[i,j] <- poa.df$max[j]
  cat(i, "\n"); flush.console()
}

saveRDS(poa20152022max, "poa20152022max.RDS")
