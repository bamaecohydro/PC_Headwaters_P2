#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Delta force exploratory analysis
# Date: 10/10/2024
# Coder: Nate
# Purpose: Examine slope area relationships 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup workspace --------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear workspace (sorry JP!)
remove(list=ls())

#load packages
library(tidyverse)
library(raster)
library(sf)
library(elevatr)
library(whitebox)
library(stars)
library(mapview)
library(tmap)

#Create temp dir
temp_dir <- "data\\workspace\\"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gather data ------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define watershed outlet
outlet<-tibble(
  lat = 37.36023, 
  lon = -121.74306) %>% 
  st_as_sf(
    coords = c("lon", "lat"), 
    crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  st_transform(crs = 3160) 
  
outlet_buff<-st_buffer(outlet, 5000)

sf_use_s2(FALSE)

#Download DEM
dem <- get_elev_raster(outlet_buff, z=14)

#Export data to temp directory
writeRaster(dem,paste0(temp_dir, 'dem.tif'), overwrite=T)
st_write(outlet, paste0(temp_dir, "outlet.shp"), append=T)

#mapview
mapview(dem) +mapview(outlet)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create fdr and face ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Smooth the dem
wbt_fast_almost_gaussian_filter(
  input = "dem.tif",
  output = "dem_smooth.tif", 
  sigma = 1.8, 
  wd = temp_dir
)

#breach depressions 
wbt_breach_depressions(
  dem = "dem_smooth.tif",
  output = "dem_breach.tif", 
  wd= temp_dir
)

#flow direction
wbt_d8_pointer(
  dem = "dem_breach.tif",
  output = "fdr.tif",
  wd = temp_dir)

#flow accumulation
wbt_d8_flow_accumulation(
  input = 'dem_breach.tif',
  output = 'fac.tif',
  pntr = F,
  wd = temp_dir
)

#slope
wbt_slope(
  dem = 'dem_breach.tif',
  output = 'slope.tif'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Delineate Watershed-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Snap pour point
wbt_snap_pour_points(
  pour_pts = "outlet.shp",
  flow_accum = "fac.tif",
  snap_dist = 100,
  output = "snap.shp",
  wd = temp_dir
)

#Check snap pour point with mapview (you may need to change the search dist)
fac <- raster(paste0(temp_dir, "//fac.tif"))
fdr <- raster(paste0(temp_dir, "//fdr.tif"))
slope <- raster(paste0(temp_dir, "//slope.tif"))
snap<- st_read(paste0(temp_dir,"//snap.shp"))

#Create watershed
wbt_watershed(
  d8_pntr  = "fdr.tif",
  pour_pts = "snap.shp",
  output   = "watershed.tif",
  wd       = temp_dir
)

#read into R
watershed <- raster(paste0(temp_dir, "//watershed.tif"))

#Convert raster to vector
watershed <- watershed %>% st_as_stars() %>% st_as_sf(., merge = TRUE)

#Crop fac and fdr
fac <- mask(fac, watershed)
fac <- crop(fac, watershed)
slope <- mask(slope, watershed)
slope <- crop(slope, watershed)

#Convert to points
fac_pnts <- rasterToPoints(fac) %>% as_tibble()
slope_pnts <- rasterToPoints(slope) %>% as_tibble()
pnts <- left_join(fac_pnts, slope_pnts)
stream_pnts <- pnts %>% filter(fac >= quantile(pnts$fac, 0.95))

#Create threshold
plot(stream_pnts$fac*(4.25^2)/10000/1000, stream_pnts$slope, 
     pch=19, col="grey30", log="xy", cex=0.4, ylim = c(0.1, 50), 
     xlab = "Drainage Area [km^2]", ylab='Slope [%]')
